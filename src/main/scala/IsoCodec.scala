import java.nio.charset.Charset

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, DecodeResult, _}

package object IsoCodec {

  case class LLString(applies:Boolean, description: String = "") extends Codec[String] {
    implicit val charset: Charset = Charset.defaultCharset()

    def encode(b: String) = {
      string.encode("%02d".format(b.length) + b)
    }

    def decode(b: BitVector) = {
      if (!applies) {
        Attempt.successful(DecodeResult.apply(null, b))
      } else {
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
    }

    override def sizeBound: SizeBound = SizeBound.atLeast(2)
  }

  case class LLLString(applies:Boolean, description: String = "") extends Codec[String] {
    implicit val charset: Charset = Charset.defaultCharset()

    def encode(b: String) = {
      string.encode("%03d".format(b.length) + b)
    }

    def decode(b: BitVector) = {
      if (!applies) {
        Attempt.successful(DecodeResult.apply(null, b))
      } else {
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
    }

    override def sizeBound: SizeBound = SizeBound.atLeast(2)
  }

  case class IntString(size:Int, applies:Boolean, description: String) extends Codec[String] {
    implicit val charset: Charset = Charset.defaultCharset()

    def encode(b: String) = {
      string.encode(b)
    }

    def decode(b: BitVector) = {
      if (!applies) {
        Attempt.successful(DecodeResult.apply(null, b))
      } else {
        string.decode(b) match {
          case Attempt.Successful(str) => {
            val content = str.value.slice(0, size)
            val remain = str.value.slice(size, str.value.length)
            Attempt.successful(DecodeResult.apply(content, BitVector(remain.getBytes)))
          }
          case Attempt.Failure(e) => Attempt.failure(e)
        }
      }
    }

    override def sizeBound: SizeBound = SizeBound.atLeast(size)
  }
}
