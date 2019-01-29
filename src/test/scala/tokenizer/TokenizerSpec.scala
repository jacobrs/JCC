package tokenizer

import org.scalatest.{Matchers, WordSpec}
import tokenizer.Token.{FLOAT, ID, INTEGER}

class TokenizerSpec extends WordSpec with Matchers {

  "Tokenizer scanner" should {
    "remove single line comments" in {
      Tokenizer.scan(
        """id = 0
          |// this sets the id
          |ok
          |// maybe this works
          |hope this works
        """.stripMargin) should be(
        """id = 0
          |ok
          |hope this works
        """.stripMargin)
    }

    "remove multi line comments" in {
      Tokenizer.scan(
        """/* this is a test
          | //test */
          |testing = true;
          |/* testing//test */""".stripMargin) should be(
        """testing = true;
          |""".stripMargin
      )
    }

    "remove nested comments" in {
      Tokenizer.scan("// test //// test // testing removal") should be("")
      Tokenizer.scan(
        """frontline = 0
          |this /* this is a test //test */
          |testing//test""".stripMargin) should be(
        """frontline = 0
          |this testing""".stripMargin
      )
    }
  }

  "Tokenizer parser" should {

    "parse ids" in {
      Tokenizer.parse("Abdvsbj adnfbdj") should be(
        List(ID("Abdvsbj"), ID("adnfbdj")))
      Tokenizer.parse("abc") should be(List(ID("abc")))
      Tokenizer.parse("abc1") should be(List(ID("abc1")))
      Tokenizer.parse("abc_1") should be(List(ID("abc_1")))
      Tokenizer.parse("abc_") should be(List(ID("abc_")))
      Tokenizer.parse("1abc") should be(List(INTEGER("1"), ID("abc")))
      Tokenizer.parse("_1abc") should be(List(INTEGER("1"), ID("abc")))
      Tokenizer.parse("_abc1") should be(List(ID("abc1")))
    }

    "parse integers" in {
      Tokenizer.parse("0123") should be (List(INTEGER("0"), INTEGER("123")))
      Tokenizer.parse("12345") should be (List(INTEGER("12345")))
      Tokenizer.parse("100") should be (List(INTEGER("100")))
    }

    "parse simple floats" in {
      Tokenizer.parse("01.23") should be(List(INTEGER("0"), FLOAT("1.23")))
      Tokenizer.parse("12.340") should be(List())
      Tokenizer.parse("012.340") should be(List(INTEGER("0")))
    }

    "parse exponent floats" in {
      Tokenizer.parse("12.34e01") should be(List(FLOAT("12.34e0"), INTEGER("1")))
      Tokenizer.parse("12345.6789e-123") should be(List(FLOAT("12345.6789e-123")))
    }
  }

}
