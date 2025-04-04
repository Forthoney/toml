datatype value =
  Str of string
| Integer of int
| Float of real
| Boolean of bool
| Array of value list
| Table of (string * value) list

structure Value =
struct
  fun toString (Str s) =
        "\"" ^ (String.toString s) ^ "\""
    | toString (Integer i) = Int.toString i
    | toString (Float f) = Real.toString f
    | toString (Boolean b) = Bool.toString b
    | toString (Array a) =
        "[" ^ (String.concatWith ", " (map toString a)) ^ "]"
    | toString (Table entries) =
        "{"
        ^
        (String.concatWith ", "
           (map (fn (k, v) => k ^ ":" ^ (toString v)) entries)) ^ "}"
end

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

  fun traverse baseCase ((k, []), v) tbl =
        baseCase (k, v) tbl
    | traverse bc ((k, kNext :: ks), v) [] =
        Option.compose
          (fn updated => [(k, Table updated)], traverse bc ((kNext, ks), v)) []
    | traverse bc ((k, kNext :: ks), v) ((k', v') :: rest) =
        if k' = k then
          case v' of
            Table inner =>
              Option.compose
                ( fn updated => (k, Table updated) :: rest
                , traverse bc ((kNext, ks), v)
                ) inner
          | _ => NONE
        else
          Option.compose
            ( fn recResult => (k', v') :: recResult
            , traverse bc ((k, kNext :: ks), v)
            ) rest

  val insert = traverse (fn (k, v) =>
    Option.compose
      ( fn tbl => (k, v) :: tbl
      , Option.filter (List.all (fn (k', _) => k' <> k))
      ))

  val pushAt =
    let
      fun helper acc (k, v) [] =
            SOME (rev ((k, Array [v]) :: acc))
        | helper acc (k, v) ((k', v') :: rest) =
            case (k' = k, v') of
              (false, _) =>
                traverse (helper ((k', v') :: acc)) ((k, []), v) rest
            | (_, Array vs) =>
                SOME (List.revAppend (acc, (k', Array (v :: vs)) :: rest))
            | _ => NONE
    in
      traverse (helper [])
    end
end
