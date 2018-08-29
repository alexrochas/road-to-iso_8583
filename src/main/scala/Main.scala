import com.google.gson.{Gson, GsonBuilder}
import scodec.Attempt.{Failure, Successful}
import scodec.DecodeResult
import scodec.bits.BitVector

object Main {


  def main(args: Array[String]): Unit = {
    val input = "0200F238040100E090000000000000000100165369006144785170022000000000001000080716301300000116301308070500319600000008000000009999888Saque via Saque e Pague                 9863F7C6D81BE452F2D017#102@1#103@292435"
    //val input = "9000E21A040128C09A080000000000000100329867CC2DE430FDD49C795FDD3EDA84370681010807163226163226080708070501100000000196649867CC2DE430FDD49C795FDD3EDA84379019EC25CF124DADBFF550AD2D1E61F2000018825071000000080000000099998889863F7C6D81BE452F2D00000000000000042448407A000000555B0105F3401009F03060000000000009F2608DB1E3CBE5A736BBA9F2701809F10200FA501A03000000000000000000000000F058A071490C671065D8E00000000009F3704A017FB299F360226D6950580000400009A031808079C01319F02060000000000005F2A020986820258009F1A020076003008037#908@037#909@55#910@0003532526#997@CC"
    decode(input)
  }

  def decode(input: String) = {
    val firstBitMap = BitVector.fromHex(input.slice(4, 20)).get
    val secondBitMap = BitVector.fromHex(input.slice(20, 36)).get
    val body = input.substring(36)
    val bitmap = firstBitMap ++ secondBitMap
    val gson = new GsonBuilder().create()

    Iso8583BitMap
      .codec(bitmap)
      .decode(BitVector(body.getBytes)) match {
      case Successful(DecodeResult(decodedIso, _)) => println(gson.toJson(decodedIso))
      case Failure(e) => println(e)
    }
  }
}


