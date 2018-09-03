import org.scalatest.{FeatureSpec, GivenWhenThen};

class CodecTest extends FeatureSpec with GivenWhenThen {

  feature("Decode") {
    scenario("success") {
      Given("an message")
      val message = "9000E238040100E080080000000000000100165369006144785170068101090312285272941012285209030500319600000008000000009996588Consulta de Saldo via Saque e Pague     986003008017#102@1#103@292435"
      When("decoding")
      val decodedMessage: Iso8583BitMap.Iso8583 = Decoder.decode(message)
      Then("should match object")
      assert(decodedMessage.de02 == "5369006144785170")
    }
  }

}
