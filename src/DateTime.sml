structure Rfc3339 =
struct
  open Substring
  structure Opt = Option

  exception Invalid

  fun digits n {min, max} s =
    let
      val (n, s) = splitAt (s, n)
      fun isCvtSuccess (n, s) =
        n >= min andalso n < max andalso isEmpty s
    in
      Opt.valOf
        (Opt.compose
           ( fn (n, _) => (n, s)
           , Opt.composePartial
               (Opt.filter isCvtSuccess, Int.scan StringCvt.DEC getc)
           ) n)
    end

  fun consume opts =
    #2 o Opt.valOf
    o
    Opt.composePartial
      (Opt.filter (fn (c, _) => List.exists (fn c' => c' = c) opts), getc)

  structure Date =
  struct
    type date = {year: int, month: Date.month, day: int}

    fun isLeapYear y =
      y mod 4 = 0 andalso (y mod 100 <> 0 orelse y mod 400 = 0)

    val monthFromInt =
      fn 1 => Date.Jan
       | 2 => Date.Feb
       | 3 => Date.Mar
       | 4 => Date.Apr
       | 5 => Date.May
       | 6 => Date.Jun
       | 7 => Date.Jul
       | 8 => Date.Aug
       | 9 => Date.Sep
       | 10 => Date.Oct
       | 11 => Date.Nov
       | 12 => Date.Dec

    val monthToInt =
      fn Date.Jan => 1
       | Date.Feb => 2
       | Date.Mar => 3
       | Date.Apr => 4
       | Date.May => 5
       | Date.Jun => 6
       | Date.Jul => 7
       | Date.Aug => 8
       | Date.Sep => 9
       | Date.Oct => 10
       | Date.Nov => 11
       | Date.Dec => 12

    fun toString {year, month, day} =
      (String.concatWith "-" o map Int.toString) [year, monthToInt month, day]

    fun scan' s =
      let
        val (year, s) = digits 4 {min = 1, max = 10000} s
        val s = consume [#"-"] s
        val (month, s) = digits 2 {min = 1, max = 13} s
        val s = consume [#"-"] s
        val (day, s) =
          let
            val maxDays =
              case month of
                2 => if isLeapYear year then 29 else 28
              | 4 | 6 | 9 | 11 => 30
              | _ => 31
          in
            digits 2 {min = 1, max = maxDays + 1} s
          end
      in
        ({year = year, month = monthFromInt month, day = day}, s)
      end

    fun scan s =
      SOME (scan' s)
      handle Option | Subscript => NONE
  end

  val timeHour = digits 2 {min = 0, max = 23}
  val timeMinute = digits 2 {min = 0, max = 59}

  structure TimeOfDay =
  struct
    type time_of_day =
      {hour: int, minute: int, second: int, secfrac: int option}

    fun scan' s =
      let
        val (hour, s) = timeHour s
        val (minute, s) = timeMinute s
        val (second, s) = digits 2 {min = 0, max = 59} s
        val (secfrac, s) =
          case getc s of
            SOME (#".", s) =>
              Opt.valOf
                (Opt.compose
                   ( fn (i, s) => (SOME i, s)
                   , Opt.composePartial
                       ( Int.scan StringCvt.DEC getc
                       , Opt.filter (fn s =>
                           Opt.getOpt
                             (Opt.compose (Char.isDigit, first) s, false))
                       )
                   ) s)
          | _ => (NONE, s)
      in
        ({hour = hour, minute = minute, second = second, secfrac = secfrac}, s)
      end

    fun scan s =
      SOME (scan' s)
      handle Option | Subscript => NONE

    fun toString {hour, minute, second, secfrac} =
      let
        val s = Opt.getOpt (Opt.map (fn s => "." ^ Int.toString s) secfrac, "")
      in
        (String.concatWith ":" o map Int.toString) [hour, minute, second] ^ s
      end
  end

  structure Offset =
  struct
    type offset = (bool * int * int) option

    fun scan' s =
      let
        fun helper sign s =
          let
            val (hour, s) = timeHour s
            val s = consume [#":"] s
            val (minute, s) = timeMinute s
          in
            (SOME (sign, hour, minute), s)
          end
      in
        case getc s of
          SOME (#"Z", s) => (NONE, s)
        | SOME (#"z", s) => (NONE, s)
        | SOME (#"+", s) => helper true s
        | SOME (#"-", s) => helper false s
        | _ => raise Invalid
      end

    fun scan s =
      SOME (scan' s)
      handle Option | Subscript | Invalid => NONE

    fun toString (SOME (pos, hour, minute)) =
          (if pos then "+" else "-")
          ^ (String.concatWith ":" o map Int.toString) [hour, minute]
      | toString NONE = "Z"
  end

  type date = Date.date
  type time_of_day = TimeOfDay.time_of_day
  type offset = Offset.offset

  fun toString (date, time, offset) =
    Date.toString date ^ "T" ^ TimeOfDay.toString time ^ Offset.toString offset

  fun scan s =
    let
      val (date, s) = Date.scan' s
      val s = consume [#"T", #"t", #" "] s
      val (time, s) = TimeOfDay.scan' s
      val (offset, s) = Offset.scan' s
    in
      SOME ({date = date, time = time, offset = offset}, s)
      handle Option | Subscript | Invalid => NONE
    end
end
