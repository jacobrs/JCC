package tokenizer

import tokenizer.Automata.State
import tokenizer.Automata._
import tokenizer.Token._

import scala.util.matching.Regex
import scalaz.syntax.std.boolean._

object Tokenizer extends Enumeration {

  private val Digit = "([0-9])".r
  private val Letter = "([a-z]|[A-Z])".r
  private val AlphaNum = "([0-9]|[a-z]|[A-Z]|\\_)".r
  private val NonZero = "([1-9])".r
  private val Punctuation = "([<>\\.,:;{}\\(\\)\\[\\]=])".r
  private val Operator = "([\\+\\-\\*\\/\\&\\!\\|])".r

  def scan(body: String): String = {
    val multiLineCommentRegex: Regex = "/\\*(.|\n)*?\\*/(\n)?".r
    val singleLineComment: Regex = "//.*(\n)?".r
    body.replaceAll(multiLineCommentRegex.regex, "").
      replaceAll(singleLineComment.regex, "")
  }

  def parse(body: String): Seq[Token] = {
    val cleanBody = scan(body)
    var tokens = Seq.empty[Token]
    var currentState = State(STARTING_TAG, Seq())

    var i = 0
    cleanBody.foreach {
      c => {
        var newState: State = State(ERROR_TAG, Seq())
        currentState match {
          case State(STARTING_TAG, _) =>
            newState = runEmptyStateAfterTokenCompletion(c)
          case s@State(ID_TAG, p) =>
            c match {
              case AlphaNum(l) =>
                newState = createIDState(s, l)
              case _ =>
                tokens = tokens ++ Seq(generateIDOrReserved(p.mkString("")))
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(INTEGER_TAG, p) =>
            c match {
              case Digit(l) =>
                newState = State(INTEGER_TAG, p ++ Seq(l))
              case '.' =>
                newState = State(FLOAT_TAG, p ++ Seq('.'))
              case l =>
                tokens = tokens ++ Seq(INTEGER(p.mkString("")))
                newState = runEmptyStateAfterTokenCompletion(l)
            }
          case State(ZERO_INTEGER_TAG, p) =>
            tokens = tokens ++ Seq(INTEGER(p.mkString("")))
            newState = runEmptyStateAfterTokenCompletion(c)
          case State(FLOAT_TAG, p) =>
            c match {
              case NonZero(l) =>
                newState = State(NON_ZERO_FLOAT_TAG, p ++ Seq(l))
              case Digit(_) =>
                newState = State(ZERO_FLOAT_TAG, p ++ Seq('0'))
              case _ =>
                val integerPart = p.mkString("").substring(0, p.size - 2)
                tokens = tokens ++ Seq(INTEGER(integerPart), PUNCTUATION("."))
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(ZERO_FLOAT_TAG, p) =>
            c match {
              case NonZero(l) =>
                newState = State(NON_ZERO_FLOAT_TAG, p ++ Seq(l))
              case Digit(l) =>
                newState = State(DOUBLE_ZERO_FLOAT_TAG, p ++ Seq(l))
              case 'E'|'e' =>
                newState = State(EXPONENT_FLOAT_TAG, p ++ Seq(c))
              case _ =>
                tokens = tokens ++ Seq(FLOAT(p.mkString("")))
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(DOUBLE_ZERO_FLOAT_TAG, p) =>
            c match {
              case NonZero(l) =>
                newState = State(NON_ZERO_FLOAT_TAG, p ++ Seq(l))
              case Digit(l) =>
                newState = State(DOUBLE_ZERO_FLOAT_TAG, p ++ Seq(l))
              case _ =>
                System.out.println(s"Invalid float ${p.mkString("")}")
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(NON_ZERO_FLOAT_TAG, p) =>
            c match {
              case NonZero(l) =>
                newState = State(NON_ZERO_FLOAT_TAG, p ++ Seq(l))
              case Digit(l) =>
                newState = State(DOUBLE_ZERO_FLOAT_TAG, p ++ Seq(l))
              case 'E'|'e' =>
                newState = State(EXPONENT_FLOAT_TAG, p ++ Seq(c))
              case _ =>
                tokens = tokens ++ Seq(FLOAT(p.mkString("")))
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(EXPONENT_FLOAT_TAG, p) =>
            c match {
              case NonZero(l) =>
                newState = State(FLOAT_INTEGER_TAG, p ++ Seq(l))
              case Digit(l) =>
                newState = State(FLOAT_ZERO_INTEGER_TAG, p ++ Seq(l))
              case l@('+'|'-') =>
                newState = State(ENDING_EXPONENT_FLOAT_TAG, p ++ Seq(l))
              case _ =>
                System.out.println(s"Invalid float ${p.mkString("")}")
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(ENDING_EXPONENT_FLOAT_TAG, p) =>
            c match {
              case NonZero(l) =>
                newState = State(FLOAT_INTEGER_TAG, p ++ Seq(l))
              case Digit(l) =>
                newState = State(FLOAT_ZERO_INTEGER_TAG, p ++ Seq(l))
              case _ =>
                System.out.println(s"Invalid float ${p.mkString("")}")
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(FLOAT_INTEGER_TAG, p) =>
            c match {
              case Digit(l) =>
                newState = State(FLOAT_INTEGER_TAG, p ++ Seq(l))
              case l =>
                tokens = tokens ++ Seq(FLOAT(p.mkString("")))
                newState = runEmptyStateAfterTokenCompletion(l)
            }
          case State(FLOAT_ZERO_INTEGER_TAG, p) =>
            tokens = tokens ++ Seq(FLOAT(p.mkString("")))
            newState = runEmptyStateAfterTokenCompletion(c)
          case State(PUNCTUATION_TAG, p) =>
            val compound = p.mkString("") + c
            compound match {
              case "<=" | ">=" | "::" | "==" | "<>" =>
                newState = State(POTENTIAL_DOUBLE_PUNCTUATION_TAG, p ++ Seq(c))
              case _ =>
                tokens = tokens ++ Seq(PUNCTUATION(p.mkString("")))
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(POTENTIAL_DOUBLE_PUNCTUATION_TAG, p) =>
            tokens = tokens ++ Seq(PUNCTUATION(p.mkString("")))
            newState = runEmptyStateAfterTokenCompletion(c)
          case State(OPERATOR_TAG, p) =>
            val compound = p.mkString("") + c
            compound match {
              case "&&" | "||" =>
                newState = State(POTENTIAL_DOUBLE_OPERATOR_TAG, p ++ Seq(c))
              case _ =>
                tokens = tokens ++ Seq(OPERATOR(p.mkString("")))
                newState = runEmptyStateAfterTokenCompletion(c)
            }
          case State(POTENTIAL_DOUBLE_OPERATOR_TAG, p) =>
            tokens = tokens ++ Seq(OPERATOR(p.mkString("")))
            newState = runEmptyStateAfterTokenCompletion(c)
          case s =>
            System.out.println(
              s"Invalid state ${s.name} at character ${s.prevChars.head} position: $i")
            newState = runEmptyStateAfterTokenCompletion(c)
        }
        currentState = newState
        i += 1
      }
    }

    currentState match {
      case State(ID_TAG, c) =>
        tokens ++ Seq(generateIDOrReserved(c.mkString("")))
      case State(INTEGER_TAG, c) =>
        tokens ++ Seq(INTEGER(c.mkString("")))
      case State(FLOAT_TAG|NON_ZERO_FLOAT_TAG|ZERO_FLOAT_TAG|
                 FLOAT_INTEGER_TAG|FLOAT_ZERO_INTEGER_TAG, c) =>
        tokens ++ Seq(FLOAT(c.mkString("")))
      case State(PUNCTUATION_TAG | POTENTIAL_DOUBLE_PUNCTUATION_TAG, c) =>
        tokens ++ Seq(PUNCTUATION(c.mkString("")))
      case State(OPERATOR_TAG | POTENTIAL_DOUBLE_OPERATOR_TAG, c) =>
        tokens ++ Seq(OPERATOR(c.mkString("")))
      case State(RESERVED_TAG, c) =>
        tokens ++ Seq(RESERVED(c.mkString("")))
      case _ =>
        tokens
    }
  }

  def generateIDOrReserved(word: String): Token = {
    ReservedWords.contains(word).fold(RESERVED(word), ID(word))
  }

  def createIDState(oldState: State, newChar: Char): State ={
    val prev = oldState.prevChars ++ Seq(newChar)
    State("ID", prev)
  }

  def runEmptyStateAfterTokenCompletion(terminatingToken: Char): State = {
    terminatingToken match {
      case Letter(l) => State(ID_TAG, Seq(l))
      case NonZero(l) => State(INTEGER_TAG, Seq(l))
      case Digit(l) => State(ZERO_INTEGER_TAG, Seq(l))
      case Punctuation(l) => State(PUNCTUATION_TAG, Seq(l))
      case Operator(l) => State(OPERATOR_TAG, Seq(l))
      case ' '|'\n' => State(STARTING_TAG, Seq())
      case _ => State(ERROR_TAG, Seq(terminatingToken))
    }
  }

}
