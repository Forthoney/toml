datatype value =
  Str of string
| Integer of int
| Float of real
| Boolean of bool
| Array of value list

type key = string list

structure Value:
sig
  val toString: value -> string
end =
struct
  fun toString (Str s) = "`" ^ s ^ "`"
    | toString (Integer i) = Int.toString i
    | toString (Float f) = Real.toString f
    | toString (Boolean b) = Bool.toString b
    | toString (Array a) =
        "[" ^ (String.concatWith ", " (map toString a)) ^ "]"
end

structure Parser:
sig
  datatype string_fmt = BASIC | LITERAL | MULTI_BASIC | MULTI_LITERAL

  exception Unterminated of string_fmt

  val key: substring -> key * substring
  val value: TextIO.instream -> substring -> value * substring
  val keyValuePair: TextIO.instream -> (key * value) option
end =
struct
  datatype string_fmt = BASIC | LITERAL | MULTI_BASIC | MULTI_LITERAL

  exception Unterminated of string_fmt
  exception InvalidEscape of string

  structure Opt = Option

  open Substring

  val triDoubleQuote = "\"\"\""

  fun last s =
    Opt.compose (fn s => sub (s, size s - 1), Opt.filter (not o isEmpty)) s

  fun literalString s =
    let
      val (pre, suf) = position "'" s
    in
      case getc suf of
        SOME (#"'", rest) => (string pre, rest)
      | _ => raise Unterminated LITERAL
    end

  fun multilineLiteralString strm s =
    let
      val s =
        case getc s of
          SOME (#"\n", s) => s
        | _ => s

      val (pre, suf) =
        let
          val (pre, suf) = position "'''" s
          fun loop (pre', suf) =
            if isEmpty suf then
              case TextIO.inputLine strm of
                SOME s => (loop o position "'''" o full) s
              | NONE => raise Unterminated MULTI_LITERAL
            else
              (string (span (pre, pre')), suf)
        in
          loop (pre, suf)
        end
    in
      if isPrefix ("''" ^ "'''") suf then (pre ^ "''", triml 5 suf)
      else if isPrefix ("'" ^ "'''") suf then (pre ^ "'", triml 4 suf)
      else if isPrefix "'''" suf then (pre, triml 3 suf)
      else raise Fail "Unreachable"
    end

  fun escapedString strm fmt terminator s =
    let
      val (termStart, termRest) = (Opt.valOf o getc o full) terminator
      val termRest = string termRest
      fun isEscape c =
        List.exists (fn c' => c' = c) [#"b", #"t", #"n", #"\"", #"\\"]
      fun loop (acc, s) =
        let
          val (pre, suf) = splitl (fn c => c <> termStart andalso c <> #"\\") s
          val pre = string pre
        in
          case getc suf of
            NONE =>
              (case TextIO.inputLine strm of
                 SOME l => loop (acc ^ pre, full l)
               | NONE => raise Unterminated fmt)
          | SOME (#"\\", sufEscaped) =>
              (case getc sufEscaped of
                 SOME (c, rest) =>
                   if isEscape c then loop (pre ^ (String.str c), rest)
                   else raise InvalidEscape (String.str #"\\" ^ String.str c)
               | NONE => raise InvalidEscape (String.str #"\\"))
          | SOME (_, sufEscaped) =>
              if isPrefix termRest sufEscaped then
                (acc ^ pre, triml (String.size termRest) sufEscaped)
              else
                (* false flag! continue on *)
                loop (pre, suf)
        end
    in
      loop ("", s)
    end

  val basicString = escapedString (TextIO.openString "") BASIC "\""

  fun multilineBasicString strm s =
    let
      val s =
        case getc s of
          SOME (#"\n", s) => s
        | _ => s
      fun maximalString (pre, suf) =
        if isPrefix "\"" suf then (pre ^ "\"", triml 1 suf)
        else if isPrefix "\"\"" suf then (pre ^ "\"\"", triml 2 suf)
        else (pre, suf)
    in
      maximalString (escapedString strm MULTI_BASIC "\"\"\"" s)
    end

  val key =
    let
      fun bareKey s =
        let
          fun isValid c =
            Char.isAscii c
            andalso (Char.isAlphaNum c orelse c = #"-" orelse c = #"_")
          val (pre, suf) = splitl isValid s
        in
          if isEmpty pre then
            raise Fail
              ("Bare key (key without quotes) must consist of ASCII alphanumeric, dashes, or underscores at"
               ^ "`" ^ (string s) ^ "`")
          else
            (string pre, suf)
        end

      fun loop acc s =
        let
          val thisKey =
            case getc s of
              SOME (#"'", inner) => literalString inner
            | SOME (#"\"", inner) => basicString inner
            | SOME _ => bareKey s
            | NONE => raise Fail "Expected key but found nothing"

          fun dotted (key, remaining) =
            case getc (dropl Char.isSpace remaining) of
              SOME (#".", next) => loop (key :: acc) (dropl Char.isSpace next)
            | _ => (key :: acc, remaining)
        in
          dotted thisKey
        end
    in
      (fn (acc, rest) => (rev acc, rest)) o loop []
    end

  fun value strm line =
    let
      fun array s =
        case getc s of
          SOME (#"[", s) =>
            let
              fun loop acc s =
                let
                  val (v, s) = value strm s
                  val acc = v :: acc
                in
                  case getc (dropl Char.isSpace s) of
                    SOME (#"]", s) => SOME (Array (rev acc), s)
                  | SOME (#",", s) =>
                      let
                        val s = (dropl Char.isSpace s)
                      in
                        case getc s of
                          SOME (#"]", s) => SOME (Array (rev acc), s)
                        | _ => loop acc s
                      end
                  | _ => NONE
                end
            in
              loop [] (dropl Char.isSpace s)
            end
        | _ => NONE

      fun str s =
        let
          fun wrap (v, rem) =
            SOME (Str v, rem)
        in
          if isPrefix "'''" s then
            (wrap o multilineLiteralString strm o triml 3) s
          else if isPrefix "\"\"\"" s then
            (wrap o multilineBasicString strm o triml 3) s
          else if isPrefix "'" s then
            (wrap o literalString o triml 1) s
          else if isPrefix "\"" s then
            (wrap o basicString o triml 1) s
          else
            NONE
        end

      fun skipUnderscore s =
        case getc s of
          SOME (#"_", strm) => skipUnderscore s
        | default => default

      val integer =
        let
          fun loop [] _ = NONE
            | loop (r :: rs) s =
                case Int.scan r skipUnderscore s of
                  SOME (i, rest) => SOME (Integer i, rest)
                | NONE => loop rs s
        in
          loop [StringCvt.DEC, StringCvt.HEX, StringCvt.BIN, StringCvt.OCT]
        end

      val float = Opt.compose
        (fn (v, rest) => (Float v, rest), Real.scan skipUnderscore)

      val bool = Opt.compose (fn (v, rest) => (Boolean v, rest), Bool.scan getc)

      fun tryMap [] v = NONE
        | tryMap (f :: fs) v =
            case f v of
              SOME v => SOME v
            | NONE => tryMap fs v
    in
      case tryMap [array, str, bool, float, integer] line of
        SOME v => v
      | NONE => raise Fail "Unknown value type"
    end

  fun equals s =
    case getc (dropl Char.isSpace s) of
      SOME (#"=", rest) => dropl Char.isSpace rest
    | _ => raise Fail "Expected to find '=' after key"

  fun keyValuePair strm =
    Opt.composePartial
      ( fn line =>
          if isEmpty line orelse isPrefix "#" line then
            keyValuePair strm
          else
            let
              val (k, line) = key line
              val (v, rest) = value strm (equals line)
            in
              if isEmpty (dropl Char.isSpace rest) then SOME (k, v)
              else raise Fail ("Stuff left in " ^ (string rest))
            end
      , Opt.compose (dropl Char.isSpace o full, TextIO.inputLine)
      ) strm
end
