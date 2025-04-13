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
  val concat: (doc * doc) -> doc
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

  fun searchWithContext k =
    let
      fun loop _ [] = NONE
        | loop prev ((k', v') :: xs) =
            if k' = k then SOME (prev, v', xs) else loop ((k', v') :: prev) xs
    in
      loop []
    end

  fun insertSelf ((prev, myKey, post), tbl) =
    List.revAppend (prev, (myKey, Table tbl) :: post)

  fun traverse baseCase ctxts tbl =
    fn ((k, []), v) =>
      (case baseCase (k, v, tbl) of
         SOME init => SOME (foldl insertSelf init ctxts)
       | NONE => NONE)
     | ((k, k1 :: ks), v) =>
      case searchWithContext k tbl of
        SOME (prev, Table tbl, rest) =>
          traverse baseCase ((prev, k, rest) :: ctxts) tbl ((k1, ks), v)
      | NONE => traverse baseCase (([], k, tbl) :: ctxts) [] ((k1, ks), v)
      | SOME _ => NONE

  fun merge (xs, ys) =
    if List.exists (fn (xk, _) => List.exists (fn (yk, _) => yk = xk) ys) xs then
      NONE
    else
      SOME (xs @ ys)

  val insert =
    traverse
      (fn (k, Table kvs, tbl) =>
         (case searchWithContext k tbl of
            SOME (prev, Table kvs', rest) =>
              (case merge (kvs, kvs') of
                 NONE => NONE
               | SOME kvs =>
                   SOME (List.revAppend (prev, (k, Table kvs) :: rest)))
          | NONE => SOME ((k, Table kvs) :: tbl)
          | SOME _ => NONE)
        | (k, v, tbl) =>
         if List.all (fn (k', _) => k' <> k) tbl then SOME ((k, v) :: tbl)
         else NONE) []

  val pushAt =
    traverse
      (fn (k, v, tbl) =>
         case searchWithContext k tbl of
           SOME (prev, Array vs, rest) =>
             SOME (List.revAppend (prev, (k, Array (v :: vs)) :: rest))
         | NONE => SOME ((k, Array [v]) :: tbl)
         | SOME _ => NONE) []
end
