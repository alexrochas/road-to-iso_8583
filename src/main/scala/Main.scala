import scodec.bits.BitVector

object Main {

  def main(args: Array[String]): Unit = {
    val input = "0200F238040100E09008000000000000010016536900512989610902200000000001000002091104266123210604270209051098402950040003124900TECBAN BRAZILTECBAN SUP GOLF CIN    LONDRINA      BRA9861F01C9EDAA87681802610000000015010760000000000011#102@0#103@"
    decode(input)
  }

  def decode(input: String) = {
    var index = 0
    val transactionType = input.substring(0, 4)
    val firstBitMap = BitVector.fromHex(input.slice(4, 20)).get
    val secondBitMap = BitVector.fromHex(input.slice(20, 36)).get
    var body = input.substring(36)

    var codec = Iso8583BitMap.bitMap
      .filter(i => (firstBitMap ++ secondBitMap).toBin.charAt(i.index - 1).equals('1'))
      .sortBy(i => i.index)
      .map(i => i.codec)
      .map(i => i)
      .reduce((a,b) => a ~ b)
      .flattenLeftPairs
    println(codec.decode(BitVector(body.getBytes)))
  }
}


