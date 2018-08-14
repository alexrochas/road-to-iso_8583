import scodec.bits.{BitVector, _}

object Main {

  def main(args: Array[String]): Unit = {
    val input = "0200F238040100E09008000000000000010016536900512989610902200000000001000002091104266123210604270209051098402950040003124900TECBAN BRAZILTECBAN SUP GOLF CIN    LONDRINA      BRA9861F01C9EDAA87681802610000000015010760000000000011#102@0#103@"
    println(input.slice(0,4))
    println(input.slice(4,20))
    println(input.slice(20,36))
    println(input.substring(36))
    println(hex"F238040100E090080000000000000100".bits.toBin)
    decode(input)
  }

  case class Message()

  object BitMap {
    val SecondBitMap = 1
    val Pan = 2
    val ProcessingCode = 3
    val TransactionValue = 4
    val StandardDateTime = 7
    val CapturedNSU = 11
    val LocalTime = 12
    val LocalDate = 13
    val AccountingDate = 15
    val InputMode = 22
    val OperatorCode = 32
    val SecondCardTrail = 35
    val OriginNSU = 37
    val ResponseCode = 39
    val TerminalId = 41
    val Establishment = 42
    val CurrencyCode = 49
    val EncriptedPassword = 52
    val PasswordSize = 53
    val EmmiterChipCrypt = 55
    val TerminalType = 61
    val TransactionData = 62
    val PositiveId = 63
    val GenericData = 120
    val ComplementaryData = 122
    val ResponseNSU = 127
  }

  def decode(input: String) = {
    var index = 0
    val transactionType = input.substring(0, 4)
    val firstBitMap = BitVector.fromHex(input.slice(4, 20)).get
    val secondBitMap = BitVector.fromHex(input.slice(20, 36)).get
    var body = input.substring(36)
    val pan = if (firstBitMap.toBin.charAt(BitMap.Pan - 1).equals('1')) {
      val panSize = 2
      val contentSize = body.slice(index, panSize) // because is LL
      val content = body.slice(index + panSize, contentSize.toInt + contentSize.length)
      index = contentSize.toInt + contentSize.length
      println("Pan " + content)
      content
    }
    val processingCode = if (firstBitMap.toBin.charAt(BitMap.ProcessingCode - 1).equals('1')) {
      val contentSize = 6 // because is LL
      val content = body.slice(index, index + 6)
      index = index + 6
      println("Processing code " + content)
      content
    }
    val transactionValue = if (firstBitMap.toBin.charAt(BitMap.TransactionValue - 1).equals('1')) {
      val contentSize = 12 // because is LL
      val content = body.slice(index, index + 12)
      index = index + 12
      println("Transaction value " + content)
      content
    }
    val standardDateTime = if (firstBitMap.toBin.charAt(BitMap.StandardDateTime - 1).equals('1')) {
      val contentSize = 10 // because is LL
      val content = body.slice(index, index + 10)
      index = index + 10
      println("Standard date time value " + content)
      content
    }
    val nsu = if (firstBitMap.toBin.charAt(BitMap.CapturedNSU - 1).equals('1')) {
      val contentSize = 6 // because is LL
      val content = body.slice(index, index + 6)
      index = index + 6
      println("NSU value " + content)
      content
    }
    val localTime = if (firstBitMap.toBin.charAt(BitMap.LocalTime - 1).equals('1')) {
      val contentSize = 6 // because is LL
      val content = body.slice(index, index + 6)
      index = index + 6
      println("Local time value " + content)
      content
    }
    val localDate = if (firstBitMap.toBin.charAt(BitMap.LocalDate - 1).equals('1')) {
      val contentSize = 4 // because is LL
      val content = body.slice(index, index + 4)
      index = index + 4
      println("Local date value " + content)
      content
    }
    val accountingDate = if (firstBitMap.toBin.charAt(BitMap.AccountingDate - 1).equals('1')) {
      val contentSize = 4 // because is LL
      val content = body.slice(index, index + 4)
      index = index + 4
      println("Accounting date value " + content)
      content
    }
    val inputMode = if (firstBitMap.toBin.charAt(BitMap.InputMode - 1).equals('1')) {
      val contentSize = 3 // because is LL
      val content = body.slice(index, index + 3)
      index = index + 3
      println("Input mode value " + content)
      content
    }
    val operatorCode = if (firstBitMap.toBin.charAt(BitMap.OperatorCode - 1).equals('1')) {
      val operatorCodeSize = 2
      val contentSize = body.slice(index, index + operatorCodeSize) // because is LL
      val content = body.slice(index + operatorCodeSize, index + contentSize.toInt + contentSize.length)
      index = index + operatorCodeSize + contentSize.toInt
      println("Operator code value " + content)
      content
    }

    Message()
  }
}
