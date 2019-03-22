package tokenizer

import tokenizer.Token.Location

trait Token {
  def value: String
  def location: Location
}

object Token {

  case class Location(row: Integer, col: Integer)
  case class ID(value: String, location: Location = Location(0,0)) extends Token
  case class FLOAT(value: String, location: Location = Location(0,0)) extends Token
  case class INTEGER(value: String, location: Location = Location(0,0)) extends Token
  case class OPERATOR(value: String, location: Location = Location(0,0)) extends Token
  case class RESERVED(value: String, location: Location = Location(0,0)) extends Token
  case class PUNCTUATION(value: String, location: Location = Location(0,0)) extends Token
  case class TERMINATION(value: String, location: Location = Location(0,0)) extends Token

}
