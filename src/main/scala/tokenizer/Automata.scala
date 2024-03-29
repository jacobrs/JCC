package tokenizer

object Automata {

  case class State(name: String, prevChars: Seq[Char])

  val INTEGER_TAG = "INTEGER"
  val ZERO_INTEGER_TAG = "ZERO_INTEGER"
  val FLOAT_INTEGER_TAG = "FLOAT_INTEGER"
  val FLOAT_ZERO_INTEGER_TAG = "FLOAT_ZERO_INTEGER"
  val ID_TAG = "ID"
  val STARTING_TAG = "STARTING"
  val FLOAT_TAG = "FLOAT"
  val NON_ZERO_FLOAT_TAG = "NON_ZERO_FLOAT"
  val ZERO_FLOAT_TAG = "ZERO_FLOAT"
  val DOUBLE_ZERO_FLOAT_TAG = "DOUBLE_ZERO_FLOAT"
  val EXPONENT_FLOAT_TAG = "EXPONENT_FLOAT"
  val ENDING_EXPONENT_FLOAT_TAG = "ENDING_EXPONENT_FLOAT"
  val ERROR_TAG = "ERROR"
  val PUNCTUATION_TAG = "PUNCTUATION"
  val POTENTIAL_DOUBLE_PUNCTUATION_TAG = "POTENTIAL_DOUBLE_PUNCTUATION_TAG"
  val RESERVED_TAG = "RESERVED"
  val OPERATOR_TAG = "OPERATOR"
  val POTENTIAL_DOUBLE_OPERATOR_TAG = "POTENTIAL_DOUBLE_OPERATOR_TAG"

  val ReservedWords: Seq[String] = Seq(
    "if",
    "then",
    "else",
    "for",
    "class",
    "integer",
    "float",
    "read",
    "write",
    "return",
    "main"
  )

}
