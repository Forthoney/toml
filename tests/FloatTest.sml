open Asura.Assert

structure P = Parser
val emptyStrm = TextIO.openString ""
val full = Substring.full

structure FloatTest: ASURA_SUITE =
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

structure T = AsuraAutoRunner(FloatTest)
