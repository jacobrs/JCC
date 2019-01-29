package tokenizer

trait Token {
  def value: String
}

object Token {

  case class ID(value: String) extends Token
  case class FLOAT(value: String) extends Token
  case class INTEGER(value: String) extends Token
  case class OPERATOR(value: String) extends Token
  case class RESERVED(value: String) extends Token
  case class PUNCTUATION(value: String) extends Token

}
