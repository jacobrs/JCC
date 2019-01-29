package tokenizer

import tokenizer.Token._

object AtoCCConverter {

  def convertToAtoCC(tokens: Seq[Token]): String = {

    tokens.map {
      case OPERATOR(v) => s"$v "
      case PUNCTUATION(v) => s"$v "
      case v@(INTEGER(_)|FLOAT(_)|ID(_)|RESERVED(_)) =>
        s"${v.getClass.getSimpleName.toUpperCase()} "
      case _ => ""
    }.mkString("")
  }

}
