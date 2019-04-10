package generator

import java.io.{File, PrintWriter}

import parser.ASTNode
import generator.GeneratorTraversers._

object CodeGenerator {

  def generateCode(ast: ASTNode, symbolTableWithMemoryOffsets: SymbolMemoryTable, moonOutputFile: String): Unit = {
    val writer = new PrintWriter(new File(moonOutputFile))

    createFunctionsAndMain(symbolTableWithMemoryOffsets, ast, writer)
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
    writer.write(f"${" "}%-15s entry %% Start here\n")
    traverseMain(table, node, writer)
    writer.write(f"${" "}%-15s hlt   %% Stop here\n")
    traverseOtherFunctions(table, node, writer)
  }

  /**
    * Allocate top level memory used by the main block
    * @param table symbol table containing memory requirements
    * @param writer program file writer
    */
  def allocateMemory(table: SymbolMemoryTable, writer: PrintWriter): Unit = {
    table.symbols.find(s => s.kind.equals("function") && s.name.equals("main")).fold()(l => {
      l.link.fold()(t => {
        t.symbols.foreach(e => writer.write(f"${e.name}%-15s res   ${e.size}\n"))
      })
    })
  }

  def allocateMemoryForFunction(table: SymbolMemoryTable, functionName: String,
                                className: Option[String], writer: PrintWriter): Unit = {
    className.fold(
      table.symbols.find(s => s.kind.equals("function") && s.name.equals(functionName)).fold()(l => {
        l.link.fold()(t => {
          t.symbols.foreach(e => writer.write(f"${e.name}%-15s res   ${e.size}\n"))
        })
      })
    )(name =>
      table.symbols.find(s => s.kind.equals("class") && s.name.equals(name)).fold()(c => {
        c.link.fold()(t => {
          t.symbols.find(s => s.kind.equals("function") && s.name.equals(functionName)).fold()(l => {
            l.link.fold()(v => {
              v.symbols.foreach(e => writer.write(f"${e.name}%-15s res   ${e.size}\n"))
            })
          })
        })
      })
    )
  }

}
