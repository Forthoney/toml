type key = (string * string list)

exception Unterminated of string
exception InvalidEscape of string
exception DuplicateKey
exception NotEndOfLine of string
exception Header of string

structure Parser:
sig
  val key: substring -> key * substring
  val value: TextIO.instream -> substring -> value * substring
  val keyValuePair: TextIO.instream -> substring -> (key * value) * substring
  val parse: TextIO.instream -> Document.doc
end =
struct
  structure Opt = Option

  open Substring

  val triDoubleQuote = "\"\"\""

  fun eol s =
    case getc s of
      NONE => NONE
    | SOME (#"#", s) => NONE
    | s => s

  structure StringVal =
  struct
    fun literal s =
      let
        val (inner, after) = position "'" s
      in
        case getc after of
          SOME (#"'", rest) => (string inner, rest)
        | _ => raise Unterminated "'"
      end

    fun multilineLiteral strm s =
      let
        val s =
          case getc s of
            SOME (#"\n", s) => s
          | _ => s

        fun loop acc (inner, after) =
          if isEmpty after then
            case Option.compose (full, TextIO.inputLine) strm of
              SOME s => loop (inner :: acc) (position "'''" s)
            | NONE => raise Unterminated "'''"
          else
            (inner :: acc, after)

        val (inner, after) =
          let
            val (inner, after) = loop [] (position "'''" s)
            val (quotes, after') = splitl (fn c => c = #"'") after
          in
            if size quotes < 6 then (trimr 3 quotes :: inner, after')
            else (inner, triml 3 after)
          end
      in
        (concat (rev inner), after)
      end

    fun escapedString (strm, multiline) s =
      let
        val terminator = if multiline then "\"\"\"" else "\""

        fun chomp s =
          if isEmpty s then
            case Option.compose (full, TextIO.inputLine) strm of
              NONE => raise Unterminated terminator
            | SOME line => chomp (dropl Char.isSpace line)
          else
            s

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

        fun loop acc s =
          let
            val (inner, after) =
              splitl (fn c => c <> #"\"" andalso c <> #"\\") s
            val acc = inner :: acc
          in
            case getc after of
              NONE =>
                (case Option.compose (full, TextIO.inputLine) strm of
                   SOME s => loop acc s
                 | NONE => raise Unterminated terminator)
            | SOME (#"\\", after) =>
                if isEmpty (dropl Char.isSpace after) then
                  loop acc (chomp (full ""))
                else
                  let
                    val (escaped, after) =
                      case getc after of
                        NONE => raise InvalidEscape (String.str #"\\")
                      | SOME v => escape v
                  in
                    loop (full escaped :: acc) after
                  end
            | SOME (quot, after') =>
                if isPrefix terminator after then (acc, after)
                else loop (full (String.str quot) :: acc) after'
          end
      in
        loop [] s
      end

    fun basic s =
      let val (inner, after) = escapedString (TextIO.openString "", false) s
      in (concat (rev inner), triml 1 after)
      end

    fun multilineBasic strm s =
      let
        val s =
          case getc s of
            SOME (#"\n", s) => s
          | _ => s

        val (inner, after) =
          let
            val (inner, after) = escapedString (strm, true) s
            val (quotes, after') = splitl (fn c => c = #"\"") after
          in
            if size quotes < 6 then (trimr 3 quotes :: inner, after')
            else (inner, triml 3 after)
          end
      in
        (concat (rev inner), after)
      end
  end

  fun key line =
    let
      fun bareKey s =
        let
          fun isValid c =
            Char.isAlphaNum c orelse c = #"-" orelse c = #"_"
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
          SOME (#"'", inner) => StringVal.literal inner
        | SOME (#"\"", inner) => StringVal.basic inner
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
            (wrap o StringVal.multilineLiteral strm o triml 3) s
          else if isPrefix "\"\"\"" s then
            (wrap o StringVal.multilineBasic strm o triml 3) s
          else if isPrefix "'" s then
            (wrap o StringVal.literal o triml 1) s
          else if isPrefix "\"" s then
            (wrap o StringVal.basic o triml 1) s
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
                Char.isDigit c orelse Char.contains "abcdefABCDEF" c
            | isDigit StringCvt.OCT c = Char.contains "01234567" c
            | isDigit StringCvt.BIN c = c = #"0" orelse c = #"1"
            | isDigit StringCvt.DEC c = Char.isDigit c

          fun nonDecimalInt radix s =
            case Opt.composePartial (Opt.filter (isDigit radix o #1), getc) s of
              NONE => NONE
            | SOME _ =>
                let
                  val (num, s) =
                    StringCvt.splitl (isDigit radix) skipUnderscore s
                in
                  case (Int.scan radix getc o full) num of
                    SOME (i, _) => SOME (Integer i, s)
                  | NONE => NONE
                end

          fun exponent acc s =
            let
              fun digits acc s =
                case Opt.compose (Opt.filter Char.isDigit, first) s of
                  NONE => NONE
                | SOME _ =>
                    let
                      val (digits, s) =
                        StringCvt.splitl Char.isDigit skipUnderscore s
                    in
                      SOME (acc ^ digits, s)
                    end
            in
              case getc s of
                NONE => NONE
              | SOME (#"+", s) => digits acc s
              | SOME (#"-", s) => digits (acc ^ "-") s
              | SOME _ => digits acc s
            end

          fun float acc s =
            case Opt.composePartial (Opt.filter Char.isDigit, first) s of
              NONE => NONE
            | SOME _ =>
                let
                  val (frac, s) = StringCvt.splitl Char.isDigit skipUnderscore s
                  val (acc, s) =
                    case getc s of
                      SOME (#"E" | #"e", s) =>
                        Opt.getOpt
                          (exponent (acc ^ frac ^ "e") s, (acc ^ frac, s))
                    | _ => (acc ^ frac, s)
                in
                  case Real.fromString acc of
                    SOME v => SOME (Float v, s)
                  | NONE => NONE
                end

          fun exponentOnly acc s =
            case exponent acc s of
              NONE => NONE
            | SOME (v, s) =>
                (case Real.fromString v of
                   SOME v => SOME (Float v, s)
                 | NONE => NONE)

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
                   | (SOME (#"e", s), SOME false) => exponentOnly "-0e" s
                   | (SOME (#"e", s), _) => exponentOnly "0e" s
                   | (SOME (#"E", s), SOME false) => exponentOnly "-0E" s
                   | (SOME (#"E", s), _) => exponentOnly "0E" s
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
                    | SOME (#"e", s) => exponentOnly (digits ^ "e") s
                    | SOME (#"E", s) => exponentOnly (digits ^ "E") s
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
  and keyValuePair strm line =
    let
      val (k, line) = key line
      val line = 
        case getc (dropl Char.isSpace line) of
          SOME (#"=", rest) => dropl Char.isSpace rest
        | _ => raise Fail "Expected to find '=' after key"
      val (v, rest) = value strm line
    in
      ((k, v), rest)
    end

  fun header terminator line =
    let
      val (keys, line) = key (dropl Char.isSpace line)
    in
      if isPrefix terminator line then
        let
          val line = triml (String.size terminator) line
        in
          case eol (dropl Char.isSpace line) of
            NONE => keys
          | SOME _ => raise Header (string line)
        end
      else
        raise Unterminated terminator
    end

  fun parse strm =
    let
      datatype context =
        Insert of string list
      | Append of (string * string list)

      val init = {root = Document.new, doc = Document.new, context = Insert []}

      fun flush dest buffer =
        fn (Insert []) => Document.concat (dest, buffer)
         | (Insert (k :: ks)) =>
          (Opt.valOf (Document.insert dest
             ((k, ks), Table (Document.toList buffer)))
           handle Option => raise DuplicateKey)
         | (Append (k, ks)) =>
          (Opt.valOf (Document.pushAt dest
             ((k, ks), Table (Document.toList buffer)))
           handle Option => raise DuplicateKey)

      fun loop {root, context, doc} =
        case Opt.compose (dropl Char.isSpace o full, TextIO.inputLine) strm of
          NONE => flush root doc context
        | SOME line =>
            loop
              (case eol line of
                 NONE => {root, context, doc}
               | SOME (#"[", line) =>
                   { doc = Document.new
                   , root = flush root doc context
                   , context =
                       case getc line of
                         SOME (#"[", line) => Append (header "]]" line)
                       | _ => (Insert o op:: o header "]") line
                   }
               | _ =>
                   let
                     val (kv, rest) = keyValuePair strm line
                     val rest = dropl Char.isSpace rest
                     val doc = Opt.valOf (Document.insert doc kv)
                   in
                     case eol rest of
                       NONE => {root, context, doc}
                     | SOME _ => raise NotEndOfLine (string rest)
                   end)
    in
      loop init
    end
end
