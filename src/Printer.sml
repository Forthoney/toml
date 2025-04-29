signature PRINTER =
sig
  val toString: value -> string
end

signature JSON_PRINT_TOKEN =
sig
  val colon: string
  val comma: string
  val curly: {opener: string, closer: string}
  val square: {opener: string, closer: string}
  val nIndent: int
end

signature JSON_PRINT_FMT =
sig
  val object: (string * string) list -> string
  val array: string list -> string
end

functor JsonPrintFmtFn(TOK: JSON_PRINT_TOKEN): JSON_PRINT_FMT =
struct
  val indentation =
    let
      fun loop acc 0 = acc
        | loop acc n =
            loop (" " ^ acc) (n - 1)
    in
      loop "" TOK.nIndent
    end

  val indent =
    String.concatWith "\n" o map (fn l => indentation ^ l)
    o String.tokens (fn c => c = #"\n")

  fun object kvs =
    #opener TOK.curly
    ^
    (indent o String.concatWith TOK.comma
     o map (fn (k, v) => "\"" ^ k ^ "\"" ^ TOK.colon ^ v)) kvs
    ^ #closer TOK.curly

  fun array xs =
    #opener TOK.square ^ indent (String.concatWith TOK.comma (rev xs))
    ^ #closer TOK.square
end

functor TomlTestJsonPrinterFn(FMT: JSON_PRINT_FMT): PRINTER =
struct
  val jsonEscape = String.translate
    (fn #"\"" => "\\\""
      | #"\\" => "\\\\"
      | #"/" => "\\/"
      | #"\b" => "\\b"
      | #"\f" => "\\f"
      | #"\n" => "\\n"
      | #"\r" => "\\r"
      | #"\t" => "\\t"
      | #"\000" => "\\u0000"
      | c => String.str c)

  fun tag ty v =
    FMT.object [("type", "\"" ^ ty ^ "\""), ("value", "\"" ^ v ^ "\"")]

  fun patchSign s =
    case Substring.getc (Substring.full s) of
      SOME (#"~", rest) => "-" ^ Substring.string rest
    | _ => s

  val rec toString =
    fn (Str s) => tag "string" (jsonEscape s)
     | (Integer i) => (tag "integer" o patchSign o Int.toString) i
     | (Float f) => (tag "float" o patchSign o Real.fmt StringCvt.EXACT) f
     | (Boolean b) => tag "bool" (Bool.toString b)
     | (OffsetDateTime dt) => tag "datetime" (Rfc3339.toString dt)
     | (LocalDateTime (day, time)) =>
      tag "datetime-local"
        (Rfc3339.Date.toString day ^ "T" ^ Rfc3339.TimeOfDay.toString time)
     | (LocalDate day) => tag "date-local" (Rfc3339.Date.toString day)
     | (LocalTime time) => tag "time-local" (Rfc3339.TimeOfDay.toString time)
     | (Array xs) => FMT.array (map toString xs)
     | (Table kvs) =>
      FMT.object (map (fn (k, v) => (jsonEscape k, toString v)) kvs)
end

structure Printer =
struct
  structure HumanToken: JSON_PRINT_TOKEN =
  struct
    val colon = ": "
    val comma = ",\n"
    val curly = {opener = "{\n", closer = "\n}"}
    val square = {opener = "[\n", closer = "\n]"}
    val nIndent = 2
  end

  structure CompactToken: JSON_PRINT_TOKEN =
  struct
    val colon = ":"
    val comma = ","
    val curly = {opener = "{", closer = "}"}
    val square = {opener = "[", closer = "]"}
    val nIndent = 0
  end

  structure HumanFmt = JsonPrintFmtFn(HumanToken)
  structure CompactFmt = JsonPrintFmtFn(CompactToken)

  structure Debug = TomlTestJsonPrinterFn(HumanFmt)
  structure Default = TomlTestJsonPrinterFn(HumanFmt)
end
