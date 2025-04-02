val _ =
  let
    val strm =
      case CommandLine.arguments () of
        ["-"] => TextIO.stdIn
      | [file] => TextIO.openIn file
      | _ => raise Fail "Invalid options"

    fun loop () =
      case Parser.keyValuePair strm of
        SOME (k, v) =>
          ( print ((String.concatWith "." k) ^ ":" ^ (Value.toString v) ^ "\n")
          ; loop ()
          )
      | NONE => ()
  in
    loop ()
  end
