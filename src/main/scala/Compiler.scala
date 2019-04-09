import java.io.{File, PrintWriter}

import generator.CodeGenerator._
import generator.SymbolMemoryTable
import parser.Parser
import semantic.SymbolTableGenerator
import semantic.SymbolTableGenerator.SemanticError
import tokenizer.{AtoCCConverter, Tokenizer}

import scala.io.Source

// Compiler acts as the driver
object Compiler {

  def main (args: Array[String]): Unit = {

    val programFiles = 4

    for(i <- 4 to programFiles) {

      System.out.println(s"Compiling program$i.txt")

      val program = Source.fromResource(s"program$i.txt").getLines().mkString("\n")
      val tokenWriter = new PrintWriter(new File(s"output/program$i-tokens.txt"))
      val a2ccWriter = new PrintWriter(new File(s"output/program$i-a2cc.txt"))
      val astWriter = new PrintWriter(new File(s"output/program$i-ast.txt"))
      val symbolWriter = new PrintWriter(new File(s"output/program$i-symbols.txt"))

      val tokens = Tokenizer.parse(program)
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

      semanticParser.errors.
        sortWith((a,b) => a.location.row < b.location.row && a.location.col < b.location.col).
        foreach(printSemanticError)

      val symbolTableWithMemoryOffsets = new SymbolMemoryTable(semanticParser.globalTable)
      symbolWriter.write(symbolTableWithMemoryOffsets.printOutput())
      symbolWriter.close()

      generateCode(result.tree, symbolTableWithMemoryOffsets, s"output/program$i.m")

    }

  }

  def printSemanticError(error: SemanticError): Unit = println(
    s"${error.message} @ L${error.location.row}:${error.location.col}")

}