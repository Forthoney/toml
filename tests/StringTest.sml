open Asura.Assert

structure P = Parser

fun helper s (parsedAssert, restAssert) =
  let
    val strm = TextIO.openString s
  in
    case (P.value strm o full o Option.valOf o TextIO.inputLine) strm of
      (Str s, rest) => (parsedAssert s; restAssert (Substring.trimr 1 rest))
    | _ => assert false
  end

structure StringTest: ASURA_SUITE =
struct
  val desc = "Parsing string values"

  fun basic () =
    helper "\"hello world\"" (eqStr "hello world", isEmptySubstr)

  fun basicWithEscape () =
    helper "\"\\thello world\"" (eqStr "\thello world", isEmptySubstr)

  fun basicWithQuoteEscape () =
    helper "\"hello \\\" world\"" (eqStr "hello \" world", isEmptySubstr)

  fun literal () =
    helper "'hello world'" (eqStr "hello world", isEmptySubstr)

  fun longBasic () =
    helper "\"hello world\"hello" (eqStr "hello world", eqSubstr' "hello")

  structure Multiline =
  struct
    fun literal () =
      helper "'''hello\n world\n'''" (eqStr "hello\n world\n", isEmptySubstr)

    fun literalWith5QuotesAtEnd () =
      helper "'''hello\n world\n'''''"
        (eqStr "hello\n world\n''", isEmptySubstr)

    fun literalWith4QuotesAtEnd () =
      helper "'''hello\n world\n''''" (eqStr "hello\n world\n'", isEmptySubstr)

    fun literalWith6QuotesAtEnd () =
      helper "'''hello\n world\n''''''" (eqStr "hello\n world\n", eqSubstr' "'''")

    fun surround s = "\"\"\"" ^ s ^ "\"\"\""

    fun basic () =
      helper (surround "hello\n world\n")
        (eqStr "hello\n world\n", isEmptySubstr)

    fun basicLineEndingBackslash () =
      helper (surround "hello \\ \n world") (eqStr "hello world", isEmptySubstr)

    fun basicWithUnescapedQuotes () =
      helper (surround "hello\n \"\" world\n")
        (eqStr "hello\n \"\" world\n", isEmptySubstr)

    fun basicWithThreeQuotesSeparatedBySpace () =
      helper (surround "hello\n \"\"\n\" world\n")
        (eqStr "hello\n \"\"\n\" world\n", isEmptySubstr)

    fun basicWith4QuotesAtEnd () =
      helper (surround "hello world\"") (eqStr "hello world\"", isEmptySubstr)

    fun basicWith5QuotesAtEnd () =
      helper (surround "hello world\"\"")
        (eqStr "hello world\"\"", isEmptySubstr)

    fun basicWith6QuotesAtEnd () =
      helper (surround "hello world\"\"\"")
        (eqStr "hello world", eqSubstr' "\"\"\"")
  end


  val tests =
    [ basic
    , basicWithEscape
    , basicWithQuoteEscape
    , literal
    , Multiline.literal
    , Multiline.literalWith5QuotesAtEnd
    , Multiline.literalWith4QuotesAtEnd
    , Multiline.literalWith6QuotesAtEnd
    , Multiline.basic
    , Multiline.basicWithUnescapedQuotes
    , Multiline.basicWithThreeQuotesSeparatedBySpace
    , Multiline.basicLineEndingBackslash
    , Multiline.basicWith4QuotesAtEnd
    , Multiline.basicWith5QuotesAtEnd
    , Multiline.basicWith6QuotesAtEnd
    ]
end

structure T = AsuraAutoRunner(StringTest)
