open Asura.Assert

structure P = Parser
val emptyStrm = TextIO.openString ""
val full = Substring.full

structure IntTest: ASURA_SUITE =
struct
  val desc = "Parsing int values"

  fun success expected v =
    case P.value emptyStrm (full v) of
      (Integer i, rest) => (eqInt expected i; isEmptySubstr rest)
    | _ => assert false

  fun hex () = success 1 "0x1"

  val tests =
    [ hex
    ]
end

structure T = AsuraAutoRunner(IntTest)
