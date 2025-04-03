val strm =
  case CommandLine.arguments () of
    ["-"] => TextIO.stdIn
  | [file] => TextIO.openIn file
  | _ => raise Fail "Invalid options"

val doc = Parser.parse strm
val _ = print (Value.toString (Table (Document.toList doc)) ^ "\n")
