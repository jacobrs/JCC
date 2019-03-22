package tokenizer

import tokenizer.Token._

object AtoCCConverter {

  def convertToAtoCC(tokens: Seq[Token]): String = {

    tokens.map {
      case OPERATOR(v, _) => s"$v "
      case PUNCTUATION(v, _) => s"$v "
      case v@(INTEGER(_, _)|FLOAT(_, _)|ID(_, _)|RESERVED(_, _)) =>
        s"${v.getClass.getSimpleName.toUpperCase()} "
      case _ => ""
    }.mkString("")
  }

}
