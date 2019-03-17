import java.io.{File, PrintWriter}

import parser.Parser
import tokenizer.{AtoCCConverter, Tokenizer}

import scala.io.Source

// Compiler acts as the driver
object Compiler {

  def main (args: Array[String]): Unit = {

    val programFiles = 3

    for(i <- 3 to programFiles) {

      System.out.println(s"Compiling program$i.txt")

      val program1 = Source.fromResource(s"program$i.txt").getLines().mkString("\n")
      val writer = new PrintWriter(new File(s"program$i-parse.txt"))
      val a2ccWriter = new PrintWriter(new File(s"program$i-a2cc.txt"))

      val tokens = Tokenizer.parse(program1)
      val output = tokens.map(token =>
        s"[${token.getClass.getSimpleName.toLowerCase()}:${token.value}]"
      ).mkString("\n")

      Parser.parse(tokens)

      /*
      writer.write(output)
      a2ccWriter.write(AtoCCConverter.convertToAtoCC(tokens))

      a2ccWriter.close()
      writer.close()
      */


    }

  }

}