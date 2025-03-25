structure Parser:
sig
  val key: substring -> (string list * substring, string) Result.result
end =
struct
  structure Opt = Option

  open Substring
  open Result

  fun last s =
    Opt.compose (fn s => sub (s, size s - 1), Opt.filter (not o isEmpty)) s

  fun literalString s =
    let
      val (pre, suf) = position "'" s
    in
      case getc suf of
        SOME (#"'", rest) => OK (string pre, rest)
      | _ =>
          ERR
            "Literal string (string with single quote) did not have an ending quote"
    end

  fun escapedString terminator s =
    let
      val unfinished =
        "Unfinished escape sequence. If you meant to use a backslash, you need to use two of them like '\\'"
      fun unrecognized c =
        "Unrecognized escape sequence \\" ^ String.str c
      val unterminated =
        "Did not find " ^ terminator ^ " marking the end of the string"

      val (termStart, termRest) = (Opt.valOf o getc o full) terminator
      val (pre, suf) = splitl (fn c => c <> termStart andalso c <> #"\\") s

      val (termRest, pre) = (string termRest, string pre)

      val escape =
        let
          fun helper c rest =
            OK (pre ^ (String.str c), rest)

          fun esc (#"b", rest) = helper #"\b" rest
            | esc (#"t", rest) = helper #"\t" rest
            | esc (#"n", rest) = helper #"\n" rest
            | esc (#"\"", rest) = helper #"\"" rest
            | esc (#"\\", rest) = helper #"\\" rest
            | esc (c, _) =
                ERR (unrecognized c)

          fun merge (pre, suf) =
            Result.compose
              (fn (pre', suf') => (pre ^ pre', suf'), escapedString terminator)
              suf
        in
          Result.composePartial (merge, esc)
        end
    in
      case getc suf of
        SOME (#"\\", sufEscaped) =>
          Result.composePartial (escape, Result.fromOption) (getc sufEscaped, unfinished)
      | SOME (_, suf) =>
          if isPrefix termRest suf then
            OK (pre, triml (String.size termRest) suf)
          else
            Result.compose
              (fn (pre', suf') => (pre ^ pre', suf'), escapedString terminator)
              suf
      | NONE => ERR unterminated
    end

  val basicString = escapedString "\""

  val multilineBasicString =
    let
      fun maximalString (pre, suf) =
        if isPrefix "\"" suf then
          (pre ^ "\"", triml 1 suf)
        else if isPrefix "\"\"" suf then
          (pre ^ "\"\"", triml 2 suf)
        else
          (pre, suf)
    in
      Result.compose (maximalString, escapedString "\"\"\"")
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
            ERR
              "Bare key (key without quotes) must consist of ASCII alphanumeric, dashes, or underscores "
          else
            OK (string pre, suf)
        end

      fun loop acc s =
        let
          val thisKey =
            case getc s of
              SOME (#"'", inner) => literalString inner
            | SOME (#"\"", inner) => basicString inner
            | SOME _ => bareKey s
            | NONE => ERR "Expected key but found nothing"

          fun dotted (key, remaining) =
            case getc (dropl Char.isSpace remaining) of
              SOME (#".", next) => loop (key :: acc) (dropl Char.isSpace next)
            | _ => OK (key :: acc, remaining)
        in
          Result.mapPartial dotted thisKey
        end
    in
      Result.compose (fn (acc, rest) => (rev acc, rest), loop [])
    end

  fun value s = ERR "unimplemented"

  fun keyValuePair s =
    case key s of
      OK (k, rest) =>
        (case getc (dropl Char.isSpace rest) of
           SOME (#"=", rest) => value (dropl Char.isSpace rest)
         | _ => ERR "Expected to find '=' after key")
    | ERR e => ERR e
end
