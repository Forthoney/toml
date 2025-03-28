datatype value =
  Str of string
| Integer of int
| Float of real
| Boolean of bool

fun tryMap [] v = NONE
  | tryMap (f :: fs) v =
      case f v of
        SOME v => SOME v
      | NONE => tryMap fs v

structure Parser:
sig
  datatype string_fmt = BASIC | LITERAL | MULTI_BASIC | MULTI_LITERAL

  exception Unterminated of string_fmt

  val key: substring -> string list * substring
  val value: substring -> value * substring
end =
struct
  datatype string_fmt = BASIC | LITERAL | MULTI_BASIC | MULTI_LITERAL

  exception Unterminated of string_fmt
  exception InvalidEscape of string

  structure Opt = Option

  open Substring
  open Result

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

  fun multilineLiteralString s =
    let
      val s =
        case getc s of
          SOME (#"\n", s) => s
        | _ => s
      val (pre, suf) = position "'''" s
      val pre = string pre
    in
      if isPrefix ("''" ^ "'''") suf then (pre ^ "''", triml 5 suf)
      else if isPrefix ("'" ^ "'''") suf then (pre ^ "'", triml 4 suf)
      else if isPrefix "'''" suf then (pre, triml 3 suf)
      else raise Unterminated MULTI_LITERAL
    end

  fun escapedString fmt terminator s =
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
            NONE => raise Unterminated fmt
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

  val basicString = escapedString BASIC "\""

  fun multilineBasicString s =
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
      maximalString (escapedString MULTI_BASIC "\"\"\"" s)
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
              "Bare key (key without quotes) must consist of ASCII alphanumeric, dashes, or underscores "
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

  fun str s =
    let
      fun wrap (v, rest) =
        SOME (Str v, rest)
    in
      if isPrefix "'''" s then (wrap o multilineLiteralString o triml 3) s
      else if isPrefix "\"\"\"" s then (wrap o multilineBasicString o triml 3) s
      else if isPrefix "'" s then (wrap o literalString o triml 1) s
      else if isPrefix "\"" s then (wrap o basicString o triml 1) s
      else NONE
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

  val float = Real.scan skipUnderscore

  val bool = Bool.scan getc

  fun value s =
    case tryMap [str, integer, bool, float] s of
      SOME v => v
    | NONE => raise Fail "Unknown value type"

  fun equals s =
    case getc (dropl Char.isSpace s) of
      SOME (#"=", rest) => dropl Char.isSpace rest
    | _ => raise Fail "Expected to find '=' after key"

  val keyValuePair = (fn (k, rest) => (k, value (equals rest))) o key
end
