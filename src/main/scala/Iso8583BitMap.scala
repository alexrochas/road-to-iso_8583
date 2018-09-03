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
    (LLString(bitmap.get(1), "PAN") ::
      IntString(6, bitmap.get(2), "Processing code") ::
      IntString(12, bitmap.get(3), "Transaction value") ::
      IntString(12, bitmap.get(4), "") ::
      IntString(12, bitmap.get(5), "Card holder") ::
      IntString(10, bitmap.get(6), "Standard date time") ::
      IntString(8, bitmap.get(8), "") ::
      IntString(8, bitmap.get(9), "") ::
      IntString(6, bitmap.get(10), "Local NSU") ::
      IntString(6, bitmap.get(11), "Local time") ::
      IntString(4, bitmap.get(12), "Local date") ::
      IntString(4, bitmap.get(14), "Accounting date") ::
      IntString(4, bitmap.get(15), "") ::
      IntString(4, bitmap.get(17), "") ::
      IntString(3, bitmap.get(19), "") ::
      IntString(3, bitmap.get(20), "") ::
      IntString(3, bitmap.get(21), "Input mode") ::
      LLString(bitmap.get(31), "Operator code") ::
      LLString(bitmap.get(34), "Second card trail") ::
      IntString(12, bitmap.get(36), "Origin NSU") ::
      IntString(2, bitmap.get(38), "Response code") ::
      IntString(8, bitmap.get(40), "Terminal ID") ::
      IntString(15, bitmap.get(41), "Establishment") ::
      IntString(40, bitmap.get(42), "Local and address") ::
      IntString(3, bitmap.get(48), "Input mode") ::
      IntString(16, bitmap.get(51), "Encrypted password") ::
      IntString(16, bitmap.get(52), "Password size") ::
      LLLString(bitmap.get(54), "Encrypted chip") ::
      LLLString(bitmap.get(60), "Terminal type") ::
      LLLString(bitmap.get(61), "Transaction data") ::
      LLLString(bitmap.get(62), "Positive ID") ::
      LLLString(bitmap.get(119), "Generic data") ::
      LLLString(bitmap.get(121), "Complementary data") ::
      LLLString(bitmap.get(126), "Response NSU")).as[Iso8583]
  }
}
