import IsoCodec.{IntString, LLLString, LLString}
import scodec._
import scodec.bits.BitVector

object Iso8583BitMap {

  case class Iso8583(de02: String,
                     de03: String,
                     de04: String,
                     de05: String,
                     de06: String,
                     de07: String,
                     de09: String,
                     de10: String,
                     de11: String,
                     de12: String,
                     de13: String,
                     de15: String,
                     de16: String,
                     de18: String,
                     de20: String,
                     de21: String,
                     de22: String,
                     de32: String,
                     de35: String,
                     de37: String,
                     de39: String,
                     de41: String,
                     de42: String,
                     de43: String,
                     de49: String,
                     de52: String,
                     de53: String,
                     de55: String,
                     de61: String,
                     de62: String,
                     de63: String,
                     de120: String,
                     de122: String,
                     de127: String
                    )

  val codec = (bitmap: BitVector) => {
    (LLString(bitmap.get(2), "PAN") ::
      IntString(6, bitmap.get(3), "Processing code") ::
      IntString(12, bitmap.get(4), "Transaction value") ::
      IntString(12, bitmap.get(5), "") ::
      IntString(12, bitmap.get(6), "Card holder") ::
      IntString(10, bitmap.get(7), "Standard date time") ::
      IntString(8, bitmap.get(9), "") ::
      IntString(8, bitmap.get(10), "") ::
      IntString(6, bitmap.get(11), "Local NSU") ::
      IntString(6, bitmap.get(6), "Local time") ::
      IntString(4, bitmap.get(13), "Local date") ::
      IntString(4, bitmap.get(15), "Accounting date") ::
      IntString(4, bitmap.get(16), "") ::
      IntString(4, bitmap.get(18), "") ::
      IntString(3, bitmap.get(20), "") ::
      IntString(3, bitmap.get(21), "") ::
      IntString(3, bitmap.get(22), "Input mode") ::
      LLString(bitmap.get(32), "Operator code") ::
      LLString(bitmap.get(35), "Second card trail") ::
      IntString(12, bitmap.get(37), "Origin NSU") ::
      IntString(2, bitmap.get(39), "Response code") ::
      IntString(8, bitmap.get(41), "Terminal ID") ::
      IntString(15, bitmap.get(42), "Establishment") ::
      IntString(40, bitmap.get(43), "Local and address") ::
      IntString(3, bitmap.get(49), "Input mode") ::
      IntString(16, bitmap.get(52), "Encrypted password") ::
      IntString(16, bitmap.get(53), "Password size") ::
      LLLString(bitmap.get(55), "Encrypted chip") ::
      LLLString(bitmap.get(61), "Terminal type") ::
      LLLString(bitmap.get(62), "Transaction data") ::
      LLLString(bitmap.get(63), "Positive ID") ::
      LLLString(bitmap.get(120), "Generic data") ::
      LLLString(bitmap.get(122), "Complementary data") ::
      LLLString(bitmap.get(127), "Response NSU")).as[Iso8583]
  }
}
