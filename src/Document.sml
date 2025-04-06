datatype value =
  Str of string
| Integer of int
| Float of real
| Boolean of bool
| OffsetDateTime of (Rfc3339.date * Rfc3339.time_of_day * Rfc3339.offset)
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
  val insert: ((k * k list) * v) -> doc -> doc option
  val pushAt: ((k * k list) * v) -> doc -> doc option
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

  fun insert kv doc =
    let
      fun loop ctxts tbl =
        fn ((k, []), v) =>
          if List.all (fn (k', _) => k' <> k) tbl then
            let
              val init = tbl @ [(k, v)]
              fun insertSelf ((prev, myKey, post), tbl) =
                List.revAppend (prev, (myKey, Table tbl) :: post)
            in
              SOME (foldl insertSelf init ctxts)
            end
          else
            NONE
         | ((k, k1 :: ks), v) =>
          let
            fun search prev =
              fn [] => loop ((prev, k, []) :: ctxts) [] ((k1, ks), v)
               | (k', v') :: rest =>
                case (k = k', v') of
                  (false, _) => search ((k', v') :: prev) rest
                | (true, Table tbl) =>
                    loop ((prev, k, rest) :: ctxts) tbl ((k1, ks), v)
                | _ => NONE
          in
            search [] tbl
          end
    in
      loop [] doc kv
    end

  fun traverse baseCase =
    fn ((k, []), v) => baseCase (k, v)
     | ((k, kNext :: ks), v) =>
      fn [] =>
        Option.compose
          ( fn updated => [(k, Table updated)]
          , traverse baseCase ((kNext, ks), v)
          ) []
       | (k', v') :: rest =>
        case (k' = k, v') of
          (false, _) =>
            Option.compose
              ( fn recResult => (k', v') :: recResult
              , traverse baseCase ((k, kNext :: ks), v)
              ) rest
        | (_, Table inner) =>
            Option.compose
              ( fn updated => (k, Table updated) :: rest
              , traverse baseCase ((kNext, ks), v)
              ) inner
        | _ => NONE

  val pushAt =
    let
      fun loop acc (k, v) =
        fn [] => SOME (rev ((k, Array [v]) :: acc))
         | (k', v') :: rest =>
          case (k' = k, v') of
            (false, _) => traverse (loop ((k', v') :: acc)) ((k, []), v) rest
          | (_, Array vs) =>
              SOME (List.revAppend (acc, (k', Array (v :: vs)) :: rest))
          | _ => NONE
    in
      traverse (loop [])
    end
end
