type key = (string * string list)
structure Parser:
sig
  datatype elem = KVPair | Header | TableArray

  exception Unterminated of string
  exception InvalidEscape of string
  exception DuplicateKey
  exception Remaining of {ty: elem, remaining: string}

  val key: substring -> key * substring
  val value: TextIO.instream -> substring -> value * substring
  val keyValuePair: TextIO.instream -> substring -> (key * value)
  val parse: TextIO.instream -> Document.doc
end =
struct
  datatype elem = KVPair | Header | TableArray

  exception Unterminated of string
  exception InvalidEscape of string
  exception DuplicateKey
  exception Remaining of {ty: elem, remaining: string}

  structure Opt = Option

  open Substring

  val triDoubleQuote = "\"\"\""

  fun last s =
    Opt.compose (fn s => sub (s, size s - 1), Opt.filter (not o isEmpty)) s

  fun isBlank line =
    case first (dropl Char.isSpace line) of
      SOME #"#" | NONE => true
    | SOME _ => false

  fun literalString s =
    let
      val (pre, suf) = position "'" s
    in
      case getc suf of
        SOME (#"'", rest) => (string pre, rest)
      | _ => raise Unterminated "'"
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
              | NONE => raise Unterminated "'''"
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

  fun escapedString strm terminator s =
    let
      val (termStart, termRest) = (Opt.valOf o getc o full) terminator
      val termRest = string termRest
      fun loop (acc, s) =
        let
          val (pre, suf) = splitl (fn c => c <> termStart andalso c <> #"\\") s
          val pre = string pre
          fun escape s =
            case getc s of
              SOME (#"n", rest) => loop (pre ^ "\n", rest)
            | SOME (#"b", rest) => loop (pre ^ "\b", rest)
            | SOME (#"t", rest) => loop (pre ^ "\t", rest)
            | SOME (#"\"", rest) => loop (pre ^ "\"", rest)
            | SOME (#"\\", rest) => loop (pre ^ "\\", rest)
            | SOME (c, rest) =>
                raise InvalidEscape (String.str #"\\" ^ String.str c)
            | NONE => raise InvalidEscape (String.str #"\\")
        in
          case getc suf of
            NONE =>
              (case TextIO.inputLine strm of
                 SOME l => loop (acc ^ pre, full l)
               | NONE => raise Unterminated terminator)
          | SOME (#"\\", sufEscaped) => escape sufEscaped
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

  val basicString = escapedString (TextIO.openString "") "\""

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
      maximalString (escapedString strm "\"\"\"" s)
    end

  fun key line =
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

      fun getKey s =
        case getc s of
          SOME (#"'", inner) => literalString inner
        | SOME (#"\"", inner) => basicString inner
        | SOME _ => bareKey s
        | NONE => raise Fail "Expected key but found nothing"

      fun loop acc s =
        let
          val s = dropl Char.isSpace s
        in
          case getc s of
            SOME (#".", rest) =>
              let val (k, rest) = getKey (dropl Char.isSpace rest)
              in loop (k :: acc) rest
              end
          | _ => (acc, s)
        end

      val (initKey, rest) = getKey line
      val (nestedKeys, rest) = loop [] rest
    in
      ((initKey, rev nestedKeys), rest)
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

  fun keyValuePair strm line =
    let
      val (k, line) = key line
      val (v, rest) = value strm (equals line)
    in
      if isBlank rest then (k, v)
      else raise Remaining {ty = KVPair, remaining = string rest}
    end

  fun header terminator line =
    let
      val ((k, ks), line) = key line
    in
      if
        isPrefix terminator line
        andalso isBlank (triml (String.size terminator) line)
      then (k, ks)
      else raise Unterminated terminator
    end

  fun parse strm =
    let
      datatype context =
        Insert of string list
      | Append of (string * string list)

      fun flush dest (Insert []) buffer = Document.concat (dest, buffer)
        | flush dest (Insert (k :: ks)) buffer =
            (Opt.valOf
               (Document.insert ((k, ks), Table (Document.toList buffer)) dest)
             handle Option => raise DuplicateKey)
        | flush dest (Append (k, ks)) buffer =
            (Opt.valOf
               (Document.pushAt ((k, ks), Table (Document.toList buffer)) dest)
             handle Option => raise DuplicateKey)

      fun loop topLevel context doc =
        let
          fun insert kv =
            Opt.valOf (Document.insert kv doc)
            handle Option => raise DuplicateKey

          fun helper line =
            case getc (dropl Char.isSpace line) of
              SOME (#"#", _) | NONE => loop topLevel context doc
            | SOME (#"[", line) =>
                let
                  val topLevel = flush topLevel context doc
                  val context =
                    case getc line of
                      SOME (#"[", line) => Append (header "]]" line)
                    | _ => (Insert o op:: o header "]") line
                in
                  loop topLevel context Document.new
                end
            | _ => (loop topLevel context o insert o keyValuePair strm) line
        in
          case Opt.compose (full, TextIO.inputLine) strm of
            SOME line => helper line
          | NONE => flush topLevel context doc
        end
    in
      loop Document.new (Insert []) Document.new
    end
end
