exception Month of int

signature SERIALIZABLE =
sig
  type t
  val scan': substring -> (t * substring)
  val scan: substring -> (t * substring) option
  val toString: t -> string
end

signature DATE =
sig
  include SERIALIZABLE

  val isLeapYear: int -> bool
  val monthFromInt: int -> Date.month
  val monthToInt: Date.month -> int
end

signature RFC3339 =
sig
  structure Date: DATE
  structure TimeOfDay: SERIALIZABLE
  structure Offset: SERIALIZABLE

  type date = Date.t
  type time_of_day = TimeOfDay.t
  type offset = Offset.t

  type t = {date: date, time: time_of_day, offset: offset}

  datatype partial = Date of date | DateTime of date * time_of_day | Full of t

  val toString: t -> string
  val scan: substring -> (t * substring) option
  val partialScan: substring -> (partial * substring) option
end

structure Rfc3339: RFC3339 =
struct
  open Substring
  structure Opt = Option

  exception Format

  fun digits n {min, max} s =
    if size s < n then
      raise Format
    else
      let
        val (num, s) = splitAt (s, n)
      in
        case Int.scan StringCvt.DEC getc num of
          NONE => raise Format
        | SOME (n, s') =>
            if n >= min andalso n < max andalso isEmpty s' then (n, s)
            else raise Format
      end

  fun consume opts s =
    case getc s of
      NONE => raise Format
    | SOME (c, s) =>
        if List.exists (fn c' => c' = c) opts then s else raise Format

  structure Date: DATE =
  struct
    type t = {year: int, month: Date.month, day: int}

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
       | otherwise => raise Month otherwise

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
      handle Format | Month _ => NONE
  end

  val timeHour = digits 2 {min = 0, max = 23}
  val timeMinute = digits 2 {min = 0, max = 59}

  structure TimeOfDay: SERIALIZABLE =
  struct
    type t = {hour: int, minute: int, second: int, secfrac: int option}

    fun scan' s =
      let
        val (hour, s) = timeHour s
        val s = consume [#":"] s
        val (minute, s) = timeMinute s
        val s = consume [#":"] s
        val (second, s) = digits 2 {min = 0, max = 59} s
        val (secfrac, s) =
          case getc s of
            SOME (#".", s) =>
              (case first s of
                 NONE => raise Format
               | SOME c =>
                   if Char.isDigit c then
                     case Int.scan StringCvt.DEC getc s of
                       SOME (i, s) => (SOME i, s)
                     | NONE => raise Format
                   else
                     raise Format)
          | _ => (NONE, s)
      in
        ({hour = hour, minute = minute, second = second, secfrac = secfrac}, s)
      end

    fun scan s =
      SOME (scan' s)
      handle Format => NONE

    fun toString {hour, minute, second, secfrac} =
      let
        fun normalize i =
          if i < 10 then "0" ^ Int.toString i else Int.toString i
        val s =
          case secfrac of
            SOME v => "." ^ Int.toString v
          | NONE => ""
      in
        (String.concatWith ":" o map normalize) [hour, minute, second] ^ s
      end
  end

  structure Offset: SERIALIZABLE =
  struct
    type t = (bool * int * int) option

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
        | _ => raise Format
      end

    fun scan s =
      SOME (scan' s)
      handle Format | Month _ => NONE

    fun toString (SOME (pos, hour, minute)) =
          (if pos then "+" else "-")
          ^ (String.concatWith ":" o map Int.toString) [hour, minute]
      | toString NONE = "Z"
  end

  fun toString {date, time, offset} =
    Date.toString date ^ "T" ^ TimeOfDay.toString time ^ Offset.toString offset

  fun scan s =
    let
      val (date, s) = Date.scan' s
      val s = consume [#"T", #"t", #" "] s
      val (time, s) = TimeOfDay.scan' s
      val (offset, s) = Offset.scan' s
    in
      SOME ({date = date, time = time, offset = offset}, s)
      handle Format | Month _ => NONE
    end


  type date = Date.t
  type time_of_day = TimeOfDay.t
  type offset = Offset.t
  type t = {date: date, time: time_of_day, offset: offset}

  datatype partial = Date of date | DateTime of date * time_of_day | Full of t

  fun partialScan s =
    case Date.scan s of
      NONE => NONE
    | SOME (date, s) =>
        (case getc s of
           SOME (#"T", s') | SOME (#"t", s') | SOME (#" ", s') =>
             (case TimeOfDay.scan s' of
                NONE => SOME (Date date, s)
              | SOME (time, s) =>
                  (case Offset.scan s of
                     NONE => SOME (DateTime (date, time), s)
                   | SOME (offset, s) =>
                       SOME
                         (Full {date = date, time = time, offset = offset}, s)))
         | _ => SOME (Date date, s))
end
