import scodec.bits.{BitVector, _}
import scodec.codecs._
import scodec.{Attempt, DecodeResult, _}

object Main {

  def main(args: Array[String]): Unit = {
    case class Point(x: Int, y: Int, z: Int)

    val pointCodec = (int8 :: int8 :: int8).as[Point]

    val encoded: Attempt[BitVector] = pointCodec.encode(Point(-5, 10, 1))
    println(encoded)
    // Successful(BitVector(24 bits, 0xfb0a01))

    val decoded: Attempt[DecodeResult[Point]] = pointCodec.decode(hex"0xfb0a01".bits)
    println(decoded)
    // Successful(DecodeResult(Point(-5, 10, 1), BitVector(empty)))

    case class FirstBitMap(
                            secondBitMap: Boolean,
                            pan: Boolean,
                            processingCode: Boolean,
                            transactionValue: Boolean,
                            n5 : Boolean,
                            n6 : Boolean,
                            dateTime: Boolean,
                            n8 : Boolean,
                            capturedNSU: Boolean
                          )

    // Second bitmap 0000000000000100
    println(ByteVector(BigInt("F21A040128C0980A", 16).toByteArray).shiftLeft(8).toBin)

    val bitMap = hex"F21A040128C0980A".bits
    println(bitMap.toBin)
    println((bool
      :: bool
      :: bool
      :: bool
      :: bool
      :: bool
      :: bool
      :: bool
      :: bool
      ).as[FirstBitMap].decode(bitMap))
  }

}
