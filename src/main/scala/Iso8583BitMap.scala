import IsoCodec.{IntString, LLLString, LLString}
import scodec._

object Iso8583BitMap {
  case class Iso8583Bit(index: Int, codec: Codec[_], description: String)

  val bitMap = List[Iso8583Bit](
    Iso8583Bit(2, LLString, "PAN"),
    Iso8583Bit(3, IntString(6), "Processing code"),
    Iso8583Bit(4, IntString(12), "Transaction value"),
    Iso8583Bit(5, IntString(12), ""),
    Iso8583Bit(6, IntString(12), "Card holder"),
    Iso8583Bit(7, IntString(10), "Standard date time"),
    Iso8583Bit(9, IntString(8), ""),
    Iso8583Bit(10, IntString(8), ""),
    Iso8583Bit(11, IntString(6), "Local NSU"),
    Iso8583Bit(12, IntString(6), "Local time"),
    Iso8583Bit(13, IntString(4), "Local date"),
    Iso8583Bit(15, IntString(4), "Accounting date"),
    Iso8583Bit(16, IntString(4), "Accounting date"),
    Iso8583Bit(18, IntString(4), "Accounting date"),
    Iso8583Bit(20, IntString(3), "Accounting date"),
    Iso8583Bit(21, IntString(3), "Accounting date"),
    Iso8583Bit(22, IntString(3), "Input mode"),
    Iso8583Bit(32, LLString, "Operator code"),
    Iso8583Bit(35, LLString, "Second card trail"),
    Iso8583Bit(37, IntString(12), "Origin NSU"),
    Iso8583Bit(39, IntString(2), "Response code"),
    Iso8583Bit(41, IntString(8), "Terminal ID"),
    Iso8583Bit(42, IntString(15), "Establishment"),
    Iso8583Bit(43, IntString(40), "Local and address"),
    Iso8583Bit(49, IntString(3), "Input mode"),
    Iso8583Bit(52, IntString(16), "Encrypted password"),
    Iso8583Bit(53, IntString(16), "Password size"),
    Iso8583Bit(55, LLLString, "Encrypted chip"),
    Iso8583Bit(61, LLLString, "Terminal type"),
    Iso8583Bit(62, LLLString, "Transaction data"),
    Iso8583Bit(63, LLLString, "Positive ID"),
    Iso8583Bit(120, LLLString, "Generic data"),
    Iso8583Bit(122, LLLString, "Complementary data"),
    Iso8583Bit(127, LLLString, "Response NSU"),
  )
}
