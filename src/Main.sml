fun flagparse ("--human" :: args) = (Printer.Debug.toString, args)
  | flagparse args = (Printer.Default.toString, args)

val (stringify, args) = flagparse (CommandLine.arguments ())
val strm =
  case args of
    [] | ["-"] => TextIO.stdIn
  | [file] => TextIO.openIn file
  | _ => raise Fail "Invalid options"
val _ = print ((stringify o Table o Document.toList o Parser.parse) strm ^ "\n")
