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
        "`" ^ (String.toString s) ^ "`"
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
  type table

  val new: table
  val insert: ((k * k list) * v) -> table -> table option
end

structure Document: DOCUMENT =
struct
  type k = string
  type v = value
  type table = (string * value) list

  val new = []

  fun insert ((k, []), v) tbl =
        Option.compose
          ( fn tbl => (k, v) :: tbl
          , Option.filter (List.all (fn (k', _) => k' <> k))
          ) tbl
    | insert ((k, kNext :: ks), v) [] =
        Option.compose
          (fn updated => [(k, Table updated)], insert ((kNext, ks), v)) []
    | insert ((k, kNext :: ks), v) ((k', v') :: rest) =
        if k' = k then
          case v' of
            Table inner =>
              Option.compose
                ( fn updated => (k, Table updated) :: rest
                , insert ((kNext, ks), v)
                ) inner
          | _ => NONE
        else
          Option.compose
            ( fn recResult => (k', v') :: recResult
            , insert ((k, kNext :: ks), v)
            ) rest
end
