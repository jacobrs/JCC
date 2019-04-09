package generator

import java.io.{File, PrintWriter}

import parser.ASTNode
import generator.GeneratorTraversers._

object CodeGenerator {

  def generateCode(ast: ASTNode, symbolTableWithMemoryOffsets: SymbolMemoryTable, moonOutputFile: String): Unit = {
    val writer = new PrintWriter(new File(moonOutputFile))

    writer.write(f"${" "}%-15s entry %% Start here\n")

    createFunctionsAndMain(symbolTableWithMemoryOffsets, ast, writer)

    writer.write(f"${" "}%-15s hlt   %% Stop here\n")

    allocateMemory(symbolTableWithMemoryOffsets, writer)

    writer.close()
  }

  /**
    * Creates function blocks in moon code
    * @param table symbol table containing memory information
    * @param node root node for ast tree
    * @param writer program file writer
    */
  def createFunctionsAndMain(table: SymbolMemoryTable, node: ASTNode, writer: PrintWriter): Unit = {
    traverseMain(table, node, writer)
  }

  /**
    * Allocate top level memory used by the main block
    * @param table symbol table containing memory requirements
    * @param writer program file writer
    */
  def allocateMemory(table: SymbolMemoryTable, writer: PrintWriter): Unit = {
    table.symbols.find(s => s.kind.equals("function") && s.name.equals("main")).fold()(l => {
      l.link.fold()(t => {
        t.symbols.foreach(e => writer.write(f"${e.name}%-15s res ${e.size}\n"))
      })
    })
  }

}
