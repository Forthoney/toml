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

  (* type memo = (k * k list) list *)
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

  fun traverse f =
    let
      fun search k =
        let
          fun loop acc [] = (acc, NONE, [])
            | loop acc ((k', v') :: kvs) =
                if k' = k then (acc, SOME v', kvs)
                else loop ((k', v') :: acc) kvs
        in
          loop []
        end

      fun insertSelf ((prev, (k, context), post), tbl) =
        case context of
          NONE => List.revAppend (prev, (k, Table tbl) :: post)
        | SOME tbls =>
            List.revAppend (prev, (k, Array (Table tbl :: tbls)) :: post)

      fun loop acc tbl ((k, ks), v) =
        let
          val (prev, v', post) = search k tbl
        in
          case ks of
            [] =>
              (case f (v, v') of
                 SOME v =>
                   let val init = List.revAppend (prev, (k, v) :: post)
                   in SOME (foldl insertSelf init acc)
                   end
               | NONE => NONE)
          | k' :: ks =>
              let
                val next =
                  case v' of
                    SOME (Table tbl) => SOME (NONE, tbl)
                  | SOME (Array (Table tbl :: rest)) => SOME (SOME rest, tbl)
                  | NONE => SOME (NONE, [])
                  | SOME _ => NONE

              in
                case next of
                  SOME (context, tbl) =>
                    loop ((prev, (k, context), post) :: acc) tbl ((k', ks), v)
                | NONE => NONE
              end
        end
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
