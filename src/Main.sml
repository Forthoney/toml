fun flagparse ("--human" :: args) = (Printer.Debug.toString, args)
  | flagparse args = (Printer.Default.toString, args)

val (stringify, args) = flagparse (CommandLine.arguments ())
val strm =
  case args of
    [] | ["-"] => TextIO.stdIn
  | [file] => TextIO.openIn file
  | _ => raise Fail "Invalid options"

fun printErr s = (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr; OS.Process.exit OS.Process.failure)

val _ = print ((stringify o Table o Document.toList o Parser.parse) strm ^ "\n")
handle Unterminated s => printErr (s ^ "was not terminated") 
     | InvalidEscape s => printErr ("Invalid escape string: " ^ s)
     | DuplicateKey => printErr ("Duplicate key: ")
     | NotEndOfLine s => printErr ("Expected end of line after " ^ s) 
