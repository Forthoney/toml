open Parser
open Result

fun keyTest (input, expected) =
  let
    val actual =
      Result.compose
        (fn (k, rem) => (k, Substring.string rem), Parser.key o Substring.full)
        input
    fun showResult (ERR e) = "ERR " ^ e
      | showResult (OK (keys, remaining)) =
          "OK ([" ^ String.concatWith ", " keys ^ "], \"" ^ remaining ^ "\")"
  in
    case (actual, expected) of
      (ERR e, ERR e') =>
        if e = e' then
          ()
        else
          print
            ("Test failed for input: `" ^ input ^ "`\n" ^ "  Expected: "
             ^ showResult expected ^ "\n" ^ "  Actual:   " ^ showResult actual
             ^ "\n")
    | (OK (keys', remaining'), OK (keys, remaining)) =>
        if
          ListPair.all (fn (k, k') => k = k') (keys', keys)
          andalso remaining = remaining'
        then
          ()
        else
          print
            ("Test failed for input: `" ^ input ^ "`\n" ^ "  Expected: "
             ^ showResult expected ^ "\n" ^ "  Actual:   " ^ showResult actual
             ^ "\n")
    | _ =>
        print
          ("Test failed for input: `" ^ input ^ "`\n" ^ "  Expected: "
           ^ showResult expected ^ "\n" ^ "  Actual:   " ^ showResult actual
           ^ "\n")
  end

val tests =
  [ ("bareKey", (OK (["bareKey"], "")))
  , ("\"quotedKey\"", (OK (["quotedKey"], "")))
  , ("\"quoted key\"", (OK (["quoted key"], "")))
  , ("\"quoted key\\\"!\"", (OK (["quoted key\"!"], "")))
  , ("'quotedKey'", (OK (["quotedKey"], "")))
  , ("'quoted key'", (OK (["quoted key"], "")))
  , ("'quoted key\\''", (OK (["quoted key\\"], "'")))
  , ("dotted.key.hi", (OK (["dotted", "key", "hi"], "")))
  ]

val _ = List.app (fn test => ignore (keyTest test)) tests
