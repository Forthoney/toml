datatype ('a, 'e) result = OK of 'a | ERR of 'e

signature RESULT =
sig
  exception Result

  val isOk: ('a, 'e) result -> bool
  val isError: ('a, 'e) result -> bool
  val valOf: ('a, 'e) result -> 'a
  val map: ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
  val mapError: ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
  val mapPartial: ('a -> ('b, 'e) result) -> ('a, 'e) result -> ('b, 'e) result
  val compose: ('a -> 'b) * ('c -> ('a, 'e) result) -> 'c -> ('b, 'e) result
  val composePartial: ('a -> ('b, 'e) result) * ('c -> ('a, 'e) result)
                      -> 'c
                      -> ('b, 'e) result
  val getOrElse: ('a, 'e) result -> 'a -> 'a
  val toOption: ('a, 'e) result -> 'a option
  val fromOption: ('a option * 'e) -> ('a, 'e) result
end

structure Result: RESULT =
struct
  exception Result

  fun isOk (OK _) = true
    | isOk _ = false

  fun isError (ERR _) = true
    | isError _ = false

  fun valOf (OK x) = x
    | valOf _ = raise Result

  fun map f (OK x) =
        OK (f x)
    | map _ (ERR e) = ERR e

  fun mapError f (ERR e) =
        ERR (f e)
    | mapError _ (OK v) = OK v

  fun mapPartial f (OK x) = f x
    | mapPartial _ (ERR e) = ERR e

  fun compose (f, g) x =
    map f (g x)

  fun composePartial (f, g) x =
    mapPartial f (g x)

  fun getOrElse (OK x) _ = x
    | getOrElse _ default = default

  fun toOption (OK x) = SOME x
    | toOption _ = NONE

  fun fromOption (SOME x, _) = OK x
    | fromOption (NONE, err) = ERR err
end
