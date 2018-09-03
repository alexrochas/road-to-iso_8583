import com.google.gson.{Gson, GsonBuilder}
import scodec.Attempt.{Failure, Successful}
import scodec.DecodeResult
import scodec.bits.BitVector

object Decoder {
  def decode(input: String): Iso8583BitMap.Iso8583 = {
    val firstBitMap = BitVector.fromHex(input.slice(4, 20)).get
    val secondBitMap = BitVector.fromHex(input.slice(20, 36)).get
    val body = input.substring(36)
    val bitmap = firstBitMap.get(0) match {
      case true => firstBitMap ++ secondBitMap
      case _ => firstBitMap
    }
    val gson = new GsonBuilder().create()

    val decodedMessage: Iso8583BitMap.Iso8583 = Iso8583BitMap
      .codec(bitmap)
      .decode(BitVector(body.getBytes)) match {
      case Successful(DecodeResult(decodedIso, _)) => decodedIso
      case Failure(e) => throw new IllegalStateException(e.message)
    }
    println(gson.toJson(decodedMessage))
    decodedMessage
  }
}


