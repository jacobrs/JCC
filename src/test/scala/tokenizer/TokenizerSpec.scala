package tokenizer

import org.scalatest.{Matchers, WordSpec}
import tokenizer.Token._

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
        List(ID("Abdvsbj", Location(1,8)), ID("adnfbdj", Location(1,16))))
      Tokenizer.parse("abc") should be(List(ID("abc", Location(1,4))))
      Tokenizer.parse("abc1") should be(List(ID("abc1", Location(1,5))))
      Tokenizer.parse("abc_1") should be(List(ID("abc_1", Location(1,6))))
      Tokenizer.parse("abc_") should be(List(ID("abc_", Location(1,5))))
      Tokenizer.parse("1abc") should be(List(INTEGER("1", Location(1,2)), ID("abc", Location(1,5))))
      Tokenizer.parse("_1abc") should be(List(INTEGER("1", Location(1,3)), ID("abc", Location(1,6))))
      Tokenizer.parse("_abc1") should be(List(ID("abc1", Location(1,6))))
    }

    "parse integers" in {
      Tokenizer.parse("0123") should be (List(INTEGER("0", Location(1,2)), INTEGER("123", Location(1,5))))
      Tokenizer.parse("12345") should be (List(INTEGER("12345", Location(1,6))))
      Tokenizer.parse("100") should be (List(INTEGER("100", Location(1,4))))
    }

    "parse simple floats" in {
      Tokenizer.parse("01.23") should be(List(INTEGER("0", Location(1,2)), FLOAT("1.23", Location(1,6))))
      Tokenizer.parse("12.340") should be(List())
      Tokenizer.parse("012.340") should be(List(INTEGER("0", Location(1,2))))
    }

    "parse exponent floats" in {
      Tokenizer.parse("12.34e01") should be(List(FLOAT("12.34e0", Location(1,8)), INTEGER("1", Location(1,9))))
      Tokenizer.parse("12345.6789e-123") should be(List(FLOAT("12345.6789e-123", Location(1,16))))
    }

    "parse punctuation" in {
      Tokenizer.parse("Tokenizer.parse") should be (List(
        ID("Tokenizer", Location(1,10)), PUNCTUATION(".", Location(1,11)), ID("parse", Location(1,16))
      ))
    }

    "parse multi-character operator" in {
      Tokenizer.parse("||") should be (List(OPERATOR("||", Location(1,3))))
      Tokenizer.parse("&&") should be (List(OPERATOR("&&", Location(1,3))))
      Tokenizer.parse("|&") should be (List(OPERATOR("|", Location(1,2)), OPERATOR("&", Location(1,3))))
    }

    "parse multi-character punctuation" in {
      Tokenizer.parse("<=") should be (List(PUNCTUATION("<=", Location(1,3))))
      Tokenizer.parse(">=") should be (List(PUNCTUATION(">=", Location(1,3))))
      Tokenizer.parse("==") should be (List(PUNCTUATION("==", Location(1,3))))
      Tokenizer.parse("<>") should be (List(PUNCTUATION("<>", Location(1,3))))
      Tokenizer.parse("::") should be (List(PUNCTUATION("::", Location(1,3))))
      Tokenizer.parse("<:") should be (List(PUNCTUATION("<", Location(1,2)), PUNCTUATION(":", Location(1,3))))
    }
  }

}
