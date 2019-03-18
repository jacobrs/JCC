import java.io.{File, PrintWriter}

import parser.Parser
import semantic.SymbolTableGenerator
import tokenizer.{AtoCCConverter, Tokenizer}

import scala.io.Source

// Compiler acts as the driver
object Compiler {

  def main (args: Array[String]): Unit = {

    val programFiles = 3

    for(i <- 1 to programFiles) {

      System.out.println(s"Compiling program$i.txt")

      val program1 = Source.fromResource(s"program$i.txt").getLines().mkString("\n")
      val tokenWriter = new PrintWriter(new File(s"program$i-tokens.txt"))
      val a2ccWriter = new PrintWriter(new File(s"program$i-a2cc.txt"))
      val astWriter = new PrintWriter(new File(s"program$i-ast.txt"))
      val symbolWriter = new PrintWriter(new File(s"program$i-symbols.txt"))

      val tokens = Tokenizer.parse(program1)
      val output = tokens.map(token =>
        s"[${token.getClass.getSimpleName.toLowerCase()}:${token.value}]"
      ).mkString("\n")

      // token generation
      tokenWriter.write(output)
      tokenWriter.close()
      a2ccWriter.write(AtoCCConverter.convertToAtoCC(tokens))
      a2ccWriter.close()

      // ast generation
      val result = Parser.parse(tokens)
      astWriter.write(result.tree.print())
      astWriter.close()

      // symbol generation
      val semanticParser = new SymbolTableGenerator()
      semanticParser.generate(result.tree)
      symbolWriter.write(semanticParser.globalTable.printOutput())
      symbolWriter.close()

    }

  }

}