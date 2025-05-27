open Asura.Assert

structure P = Parser
val emptyStrm = TextIO.openString ""
val full = Substring.full

structure FloatTest: ASURA_SUITE =
struct
  val desc = "Parsing float values"

  fun success v =
    case P.value emptyStrm (full v) of
      (Float _, rest) => isEmptySubstr rest
    | _ => assert false

  fun failure v =
    raisesAny (fn () => (ignore o P.value emptyStrm o full) v)

  structure Decimal =
  struct
    fun basic () = success "3.14"

    fun positive () = success "+3.14"

    fun negative () = success "-3.14"

    fun tildeReject () = failure "~3.14"
  end

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

  structure Exponent =
  struct
    fun basic () = success "3e2"
    fun trailingZero () = success "3e002"
    fun positive () = success "4e+22"
    fun positiveTrailingZero () = success "4e+0022"
    fun negative () = success "4e+0022"
    fun decimalAndExponent () = success "6.22e08"

    fun precededDotReject () = failure "3.e20"
    fun supercededUnderscoreReject () = failure "3e_20"
  end


  val tests = 
    [ Decimal.basic
    , Decimal.positive
    , Decimal.negative
    , Decimal.tildeReject
    , inf
    , infNegative
    , infTildeReject
    , Exponent.basic
    , Exponent.trailingZero
    , Exponent.positive
    , Exponent.positiveTrailingZero
    , Exponent.negative
    , Exponent.decimalAndExponent
    , Exponent.precededDotReject
    , Exponent.supercededUnderscoreReject
    ]
end

structure T = AsuraAutoRunner(FloatTest)
