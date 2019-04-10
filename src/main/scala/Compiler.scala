import java.io.{File, PrintWriter}

import generator.CodeGenerator._
import generator.SymbolMemoryTable
import parser.Parser
import semantic.SymbolTableGenerator
import semantic.SymbolTableGenerator.SemanticError
import tokenizer.{AtoCCConverter, Tokenizer}

import scala.io.Source

// Compiler acts as the driver
object Compiler extends App {

  if(args.length < 1){
    val programFiles = 6

    for(i <- 6 to programFiles) {
      compileProgram(s"program$i")
    }
  } else {
    val filename = args.head
    compileProgram(filename)
  }

  def compileProgram(program: String): Unit = {
    System.out.println(s"Compiling $program.txt")

    val programFile = Source.fromResource(s"$program.txt").getLines().mkString("\n")
    val tokenWriter = new PrintWriter(new File(s"output/$program-tokens.txt"))
    val a2ccWriter = new PrintWriter(new File(s"output/$program-a2cc.txt"))
    val astWriter = new PrintWriter(new File(s"output/$program-ast.txt"))
    val symbolWriter = new PrintWriter(new File(s"output/$program-symbols.txt"))

    val tokens = Tokenizer.parse(programFile)
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
      sortWith((a, b) => a.location.row < b.location.row && a.location.col < b.location.col).
      foreach(printSemanticError)

    val symbolTableWithMemoryOffsets = new SymbolMemoryTable(semanticParser.globalTable)
    symbolWriter.write(symbolTableWithMemoryOffsets.printOutput())
    symbolWriter.close()

    generateCode(result.tree, symbolTableWithMemoryOffsets, s"output/$program.m")
  }

  def printSemanticError(error: SemanticError): Unit = println(
    s"${error.message} @ L${error.location.row}:${error.location.col}")

}