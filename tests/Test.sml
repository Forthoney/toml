open Asura.Assert

structure P = Parser

val emptyStrm = TextIO.openString ""
val full = Substring.full

structure StringTests: ASURA_SUITE =
struct
  val desc = "Parsing string values"

  fun basic () =
    case P.value emptyStrm (full "\"hello world\"") of
      (Str s, rest) => (eqStr ("hello world", s); isEmptySubstr rest)
    | _ => assert false

  fun basicWithEscape () =
    case P.value emptyStrm (full "\"\\thello world\"") of
      (Str s, rest) => (eqStr ("\thello world", s); isEmptySubstr rest)
    | _ => assert false

  fun basicWithQuoteEscape () =
    case P.value emptyStrm (full "\"hello \\\" world\"") of
      (Str s, rest) => (eqStr ("hello \" world", s); isEmptySubstr rest)
    | _ => assert false

  fun literal () =
    case P.value emptyStrm (full "'hello world'") of
      (Str s, rest) => (eqStr ("hello world", s); isEmptySubstr rest)
    | _ => assert false

  structure Multiline =
  struct
    fun literal () =
      case P.value emptyStrm (full "'''hello\n world\n'''") of
        (Str s, rest) => (eqStr ("hello\n world\n", s); isEmptySubstr rest)
      | _ => assert false

    fun literalWith5QuotesAtEnd () =
      case P.value emptyStrm (full "'''hello\n world\n'''''") of
        (Str s, rest) => (eqStr ("hello\n world\n''", s); isEmptySubstr rest)
      | _ => assert false

    fun literalWith4QuotesAtEnd () =
      case P.value emptyStrm (full "'''hello\n world\n''''") of
        (Str s, rest) => (eqStr ("hello\n world\n'", s); isEmptySubstr rest)
      | _ => assert false

    fun literalWith6QuotesAtEnd () =
      case P.value emptyStrm (full "'''hello\n world\n''''''") of
        (Str s, rest) => (eqStr ("hello\n world\n", s); isEmptySubstr rest)
      | _ => assert false

    fun surround s =
      "\"\"\"" ^ s ^ "\"\"\""

    fun basic () =
      case P.value emptyStrm (full (surround "hello\n world\n")) of
        (Str s, rest) => (eqStr ("hello\n world\n", s); isEmptySubstr rest)
      | _ => assert false

    fun basicWithUnescapedQuotes () =
      case P.value emptyStrm (full (surround "hello\n \"\" world\n")) of
        (Str s, rest) => (eqStr ("hello\n \"\" world\n", s); isEmptySubstr rest)
      | _ => assert false

    fun basicWithThreeQuotesSeparatedBySpace () =
      case P.value emptyStrm (full (surround "hello\n \"\"\n\" world\n")) of
        (Str s, rest) => (eqStr ("hello\n \"\"\n\" world\n", s); isEmptySubstr rest)
      | _ => assert false
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
  ]
end

structure T = AsuraAutoRunner(StringTests)

structure FloatTests: ASURA_SUITE =
struct
  val desc = "Parsing float values"

  fun success v =
    case P.value emptyStrm (full v) of
      (Float _, rest) => assert (Substring.isEmpty rest)
    | _ => assert false

  fun failure v =
    raisesAny (fn () => (ignore o P.value emptyStrm o full) v)

  fun decimal () = success "3.14"

  fun decimalPositive () = success "+3.14"

  fun decimalNegative () = success "-3.14"

  fun decimalTildeReject () = failure "~3.14"

  fun inf () =
    case P.value emptyStrm (full "inf") of
      (Float fl, rest) =>
        ( assert (Substring.isEmpty rest)
        ; assert (not (Real.isFinite fl))
        ; assert (Real.sign fl = 1)
        )
    | _ => assert false

  fun infPositive () =
    case P.value emptyStrm (full "+inf") of
      (Float fl, rest) =>
        ( assert (Substring.isEmpty rest)
        ; assert (not (Real.isFinite fl))
        ; assert (Real.sign fl = 1)
        )
    | _ => assert false

  fun infNegative () =
    case P.value emptyStrm (full "-inf") of
      (Float fl, rest) =>
        ( assert (Substring.isEmpty rest)
        ; assert (not (Real.isFinite fl))
        ; assert (Real.sign fl = ~1)
        )
    | _ => assert false

  fun infTildeReject () = failure "~inf"

  fun exponent () = success "3e2"
  fun exponentTrailingZero () = success "3e002"
  fun exponentPositive () = success "4e+22"
  fun exponentPositiveTrailingZero () = success "4e+0022"
  fun exponentNegative () = success "4e+0022"
  fun exponentPrecededDotReject () = failure "3.e20"


  val tests = 
    [ decimalPositive
    , decimalNegative
    , decimalTildeReject
    , inf
    , infNegative
    , infTildeReject
    , exponent
    , exponentTrailingZero
    , exponentPositive
    , exponentPositiveTrailingZero
    , exponentNegative
    , exponentPrecededDotReject
    ]
end

structure T = AsuraAutoRunner(FloatTests)
