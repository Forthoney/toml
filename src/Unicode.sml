structure Utf8:
sig
  val hexToWord: char -> Word8.word option
  val unicodify: Word32.word -> string
  val shortEscape: substring -> (string * substring) option
  val longEscape: substring -> (string * substring) option
end =
struct
  val << = Word32.<<;
  val >> = Word32.>>;
  val orb = Word32.orb;
  val andb = Word32.andb;

  val cast8To32 = Word32.fromLarge o Word8.toLarge
  val cast32To8 = Word8.fromLarge o Word32.toLarge

  infix orb andb << >>

  fun hexToWord c =
    let
      val c = Char.toLower c
    in
      case c of
        #"a" | #"b" | #"c" | #"d" | #"e" | #"f" =>
          SOME (Byte.charToByte c - Byte.charToByte #"a" + 0w10)
      | c =>
          if Char.isDigit c then SOME (Byte.charToByte c - Byte.charToByte #"0")
          else NONE
    end

  fun hexesToUnicode maxCount =
    let
      fun loop (acc: Word32.word, count) s =
        if count = maxCount then
          SOME (acc, s)
        else
          case Substring.getc s of
            NONE => NONE
          | SOME (c, rest) =>
              (case hexToWord c of
                 NONE => NONE
               | SOME byte =>
                   loop (acc << 0w4 orb cast8To32 byte, count + 1) rest)
    in
      loop (0w0, 0)
    end

  fun unicodify bytes =
    let
      (* Set the biggest two bits of a byte as 10 *)
      fun setTop10 b =
        b andb 0wb0011_1111 orb 0wb1000_0000

      val bvec =
        if bytes < 0wb1000_0000 then
          [bytes]
        else if bytes < 0wb1000_0000_0000 then
          [0wb1100_0000 orb (bytes >> 0w6), setTop10 bytes]
        else if bytes < 0wx10000 then
          [ 0wb1110_0000 orb (bytes >> 0w12)
          , setTop10 (bytes >> 0w6)
          , setTop10 bytes
          ]
        else
          [ 0wb1111_0000 orb (bytes >> 0w18)
          , setTop10 (bytes >> 0w12)
          , setTop10 (bytes >> 0w6)
          , setTop10 bytes
          ]
    in
      (Byte.bytesToString o Word8Vector.fromList o map cast32To8) bvec
    end

  val shortEscape = Option.compose
    (fn (bytes, rest) => (unicodify bytes, rest), hexesToUnicode 4)

  val longEscape = Option.compose
    (fn (bytes, rest) => (unicodify bytes, rest), hexesToUnicode 8)
end
