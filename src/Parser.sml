type key = (string * string list)

exception Unterminated of string
exception InvalidEscape of string
exception DuplicateKey
exception NotEndOfLine of string

structure Parser:
sig
  val key: substring -> key * substring
  val value: TextIO.instream -> substring -> value * substring
  val keyValuePair: TextIO.instream -> substring -> (key * value)
  val parse: TextIO.instream -> Document.doc
end =
struct
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
          fun escape (c, rest) =
            case c of
              #"n" => ("\n", rest)
            | #"b" => ("\b", rest)
            | #"t" => ("\t", rest)
            | #"f" => ("\f", rest)
            | #"r" => ("\r", rest)
            | #"\"" => ("\"", rest)
            | #"\\" => ("\\", rest)
            | #"u" => Opt.valOf (Utf8.shortEscape rest)
            | #"U" => Opt.valOf (Utf8.longEscape rest)
            | c => raise InvalidEscape (String.str #"\\" ^ String.str c)
        in
          case getc suf of
            NONE =>
              (case TextIO.inputLine strm of
                 SOME l => loop (acc ^ pre, full l)
               | NONE => raise Unterminated terminator)
          | SOME (#"\\", suf) =>
              (case getc suf of
                 SOME v =>
                   let val (escaped, rest) = escape v
                   in loop (acc ^ pre ^ escaped, rest)
                   end
               | NONE => raise InvalidEscape (String.str #"\\"))
          | SOME (_, suf) =>
              if isPrefix termRest suf then
                (acc ^ pre, triml (String.size termRest) suf)
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
          fun isValid c = Char.isAlphaNum c orelse c = #"-" orelse c = #"_"
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
                    SOME (#"]", s) => SOME (Array acc, s)
                  | SOME (#",", s) =>
                      let
                        val s = (dropl Char.isSpace s)
                      in
                        case getc s of
                          SOME (#"]", s) => SOME (Array acc, s)
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

      fun numeric s =
        let
          fun skipUnderscore s =
            case getc s of
              SOME (#"_", s) => getc s
            | otherwise => otherwise

          fun isDigit StringCvt.HEX c =
                Char.isDigit c
                orelse (Char.ord c >= 97 andalso Char.ord c < 103)
            | isDigit StringCvt.OCT c =
                Char.ord c >= 48 andalso Char.ord c < 56
            | isDigit StringCvt.BIN c = c = #"0" orelse c = #"1"
            | isDigit StringCvt.DEC c = Char.isDigit c

          fun nonDecimalInt radix s =
            case Opt.composePartial (Opt.filter (isDigit radix o #1), getc) s of
              NONE => NONE
            | SOME (#"0", s) =>
                (case Opt.composePartial (Opt.filter (isDigit radix), first) s of
                   SOME _ => NONE
                 | NONE => SOME (Integer 0, s))
            | SOME _ =>
                let
                  val (num, s) =
                    StringCvt.splitl (isDigit radix) skipUnderscore s
                in
                  case (Int.scan radix getc o full) num of
                    SOME (i, _) => SOME (Integer i, s)
                  | NONE => NONE
                end

          fun float prev s =
            case Opt.composePartial (Opt.filter Char.isDigit, first) s of
              NONE => NONE
            | SOME _ =>
                let
                  val (frac, s) = StringCvt.splitl Char.isDigit skipUnderscore s
                in
                  case Real.fromString (prev ^ frac) of
                    SOME v => SOME (Float v, s)
                  | NONE => NONE
                end

          fun exponent prev s =
            case getc s of
              NONE => NONE
            | SOME (#"+", s) => float prev s
            | SOME (#"-", s) => float (prev ^ "-") s
            | SOME _ => float prev s

          fun body sign s =
            if isPrefix "inf" s then
              case sign of
                SOME false => SOME (Float Real.negInf, triml 3 s)
              | _ => SOME (Float Real.posInf, triml 3 s)
            else if isPrefix "nan" s then
              SOME (Float (0.0 / 0.0), triml 3 s)
            else
              case Opt.composePartial (Opt.filter (Char.isDigit o #1), getc) s of
                NONE => NONE
              | SOME (#"0", s) =>
                  (case (getc s, sign) of
                     (SOME (#"x", s), NONE) => nonDecimalInt StringCvt.HEX s
                   | (SOME (#"o", s), NONE) => nonDecimalInt StringCvt.OCT s
                   | (SOME (#"b", s), NONE) => nonDecimalInt StringCvt.BIN s
                   | (SOME (#".", s), SOME false) => float "-0." s
                   | (SOME (#".", s), _) => float "0." s
                   | (SOME (#"e", s), SOME false) => exponent "-0e" s
                   | (SOME (#"e", s), _) => exponent "0e" s
                   | (SOME (#"E", s), SOME false) => exponent "-0E" s
                   | (SOME (#"E", s), _) => exponent "0E" s
                   | _ => SOME (Integer 0, s))
              | SOME _ =>
                  let
                    val (digits, s) =
                      StringCvt.splitl Char.isDigit skipUnderscore s
                    val digits =
                      case sign of
                        SOME false => "-" ^ digits
                      | _ => digits
                  in
                    case getc s of
                      SOME (#".", s) => float (digits ^ ".") s
                    | SOME (#"e", s) => exponent (digits ^ "e") s
                    | SOME (#"E", s) => exponent (digits ^ "E") s
                    | _ =>
                        (case Int.fromString digits of
                           SOME i => SOME (Integer i, s)
                         | NONE => NONE)
                  end
        in
          case getc s of
            NONE => NONE
          | SOME (#"+", s) => body (SOME true) s
          | SOME (#"-", s) => body (SOME false) s
          | SOME _ => body NONE s
        end

      val bool = Opt.compose (fn (v, rest) => (Boolean v, rest), Bool.scan getc)

      fun date s =
        case Rfc3339.partialScan s of
          SOME (Rfc3339.Date d, s) => SOME (LocalDate d, s)
        | SOME (Rfc3339.DateTime dt, s) => SOME (LocalDateTime dt, s)
        | SOME (Rfc3339.Full dto, s) => SOME (OffsetDateTime dto, s)
        | NONE =>
            (case Rfc3339.TimeOfDay.scan s of
               SOME (tod, s) => SOME (LocalTime tod, s)
             | NONE => NONE)

      fun tryMap [] v = NONE
        | tryMap (f :: fs) v =
            case f v of
              SOME v => SOME v
            | NONE => tryMap fs v
    in
      case tryMap [array, str, bool, date, numeric] line of
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
      val rest = dropl Char.isSpace rest
    in
      case getc rest of
        SOME (#"#", _) | NONE => (k, v)
      | SOME _ => raise NotEndOfLine (string rest)
    end

  fun header terminator line =
    let
      val ((k, ks), line) = key (dropl Char.isSpace line)
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
            (Opt.valOf (Document.insert dest
               ((k, ks), Table (Document.toList buffer)))
             handle Option => raise DuplicateKey)
        | flush dest (Append (k, ks)) buffer =
            (Opt.valOf (Document.pushAt dest
               ((k, ks), Table (Document.toList buffer)))
             handle Option => raise DuplicateKey)

      fun loop topLevel context doc =
        let
          fun insert kv =
            Opt.valOf (Document.insert doc kv)
            handle Option => raise DuplicateKey
        in
          case Opt.compose (dropl Char.isSpace o full, TextIO.inputLine) strm of
            NONE => flush topLevel context doc
          | SOME line =>
              (case getc line of
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
               | _ => (loop topLevel context o insert o keyValuePair strm) line)
        end
    in
      loop Document.new (Insert []) Document.new
    end
end
