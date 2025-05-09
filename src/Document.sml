datatype value =
  Str of string
| Integer of int
| Float of real
| Boolean of bool
| OffsetDateTime of
    {date: Rfc3339.date, time: Rfc3339.time_of_day, offset: Rfc3339.offset}
| LocalDateTime of (Rfc3339.date * Rfc3339.time_of_day)
| LocalDate of Rfc3339.date
| LocalTime of Rfc3339.time_of_day
| Array of value list
| Table of (string * value) list

signature DOCUMENT =
sig
  eqtype k
  type v
  type doc

  val new: doc
  val fromList: (k * v) list -> doc
  val toList: doc -> (k * v) list
  val concat: doc * doc -> doc
  val traverse: (v * v option -> v option)
                -> doc
                -> ((k * k list) * v)
                -> doc option
  val insert: doc -> ((k * k list) * v) -> doc option
  val pushAt: doc -> ((k * k list) * v) -> doc option
end

structure Document :> DOCUMENT where type k = string where type v = value =
struct
  type k = string
  type v = value
  type doc = (k * v) list

  val new = []

  fun fromList l = l
  fun toList t = t

  fun concat (d1, d2) = d1 @ d2

  fun search k =
    let
      fun loop _ [] = NONE
        | loop prev ((k', v') :: xs) =
            if k' = k then SOME (prev, v', xs) else loop ((k', v') :: prev) xs
    in
      loop []
    end

  fun insertSelf ((prev, myKey, post), tbl) =
    List.revAppend (prev, (myKey, Table tbl) :: post)

  fun traverse f =
    let
      fun loop acc tbl =
        fn ((k, []), v) =>
          let
            val init =
              case search k tbl of
                SOME (prev, v', rest) =>
                  (case f (v, SOME v') of
                    SOME v => SOME (List.revAppend (prev, (k, v)::rest))
                  | NONE => NONE)
              | NONE =>
                (case f (v, NONE) of
                  SOME v => SOME ((k, v) :: tbl)
                | NONE => NONE)
          in
            case init of
              SOME init => SOME (foldl insertSelf init acc)
            | NONE => NONE
          end
         | ((k0, k1 :: ks), v) =>
          case search k0 tbl of
            SOME (prev, Table tbl, rest) =>
              loop ((prev, k0, rest) :: acc) tbl ((k1, ks), v)
          | NONE => loop (([], k0, tbl) :: acc) [] ((k1, ks), v)
          | SOME _ => NONE
    in
      loop []
    end

  fun merge (xs, ys) =
    if List.exists (fn (xk, _) => List.exists (fn (yk, _) => yk = xk) ys) xs then
      NONE
    else
      SOME (xs @ ys)

  val insert = traverse
    (fn (Table kvs, SOME (Table kvs')) =>
       Option.compose (Table, merge) (kvs, kvs')
      | (v, NONE) => SOME v
      | (v, SOME _) => NONE)

  val pushAt = traverse
    (fn (v, SOME (Array vs)) => SOME (Array (v :: vs))
      | (v, NONE) => SOME (Array [v])
      | (v, SOME _) => NONE)
end
