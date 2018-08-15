import java.nio.charset.Charset

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, DecodeResult, _}

object Main {

  def main(args: Array[String]): Unit = {
    val input = "0200F238040100E09008000000000000010016536900512989610902200000000001000002091104266123210604270209051098402950040003124900TECBAN BRAZILTECBAN SUP GOLF CIN    LONDRINA      BRA9861F01C9EDAA87681802610000000015010760000000000011#102@0#103@"

    val body = "16536900512989610902200000000001000002091104266123210604270209051098402950040003124900TECBAN BRAZILTECBAN SUP GOLF CIN    LONDRINA      BRA9861F01C9EDAA87681802610000000015010760000000000011#102@0#103@"
    println(
      (LLString :: IntString(6)).decode(BitVector(body.getBytes))
    )

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

case object LLString extends Codec[String] {
  implicit val charset: Charset = Charset.defaultCharset()

  def encode(b: String) = {
    string.encode(b)
  }

  def decode(b: BitVector) = {
    string.decode(b) match {
      case Attempt.Successful(str) => {
        val contentSize = str.value.slice(0, 2)
        val content = str.value.slice(2, contentSize.toInt + contentSize.length).toString
        val remain = str.value.slice(contentSize.toInt + contentSize.length, str.value.length)
        Attempt.successful(DecodeResult.apply(content, BitVector(remain.getBytes)))
      }
      case Attempt.Failure(e) => Attempt.failure(e)
    }
  }

  override def sizeBound: SizeBound = SizeBound.atLeast(2)
}

case object LLLString extends Codec[String] {
  implicit val charset: Charset = Charset.defaultCharset()

  def encode(b: String) = {
    string.encode(b)
  }

  def decode(b: BitVector) = {
    string.decode(b) match {
      case Attempt.Successful(str) => {
        val contentSize = str.value.slice(0, 3)
        val content = str.value.slice(3, contentSize.toInt + contentSize.length).toString
        val remain = str.value.slice(contentSize.toInt + contentSize.length, str.value.length)
        Attempt.successful(DecodeResult.apply(content, BitVector(remain.getBytes)))
      }
      case Attempt.Failure(e) => Attempt.failure(e)
    }
  }

  override def sizeBound: SizeBound = SizeBound.atLeast(2)
}

case class IntString(size:Int) extends Codec[String] {
  implicit val charset: Charset = Charset.defaultCharset()

  def encode(b: String) = {
    string.encode(b)
  }

  def decode(b: BitVector) = {
    string.decode(b) match {
      case Attempt.Successful(str) => {
        val content = str.value.slice(0, size)
        val remain = str.value.slice(size, str.value.length)
        Attempt.successful(DecodeResult.apply(content, BitVector(remain.getBytes)))
      }
      case Attempt.Failure(e) => Attempt.failure(e)
    }
  }

  override def sizeBound: SizeBound = SizeBound.atLeast(size)
}
