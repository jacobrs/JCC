package generator

import java.io.{File, PrintWriter}

import parser.ASTNode

object CodeGenerator {

  def generateCode(ast: ASTNode, symbolTableWithMemoryOffsets: SymbolMemoryTable, moonOutputFile: String): Unit = {
    val writer = new PrintWriter(new File(moonOutputFile))

    writer.write(f"${" "}%-15s entry %% Start here\n")

    writer.write(f"${" "}%-15s htl   %% Stop here\n")

    allocateMemory(symbolTableWithMemoryOffsets, writer)

    writer.close()
  }

  def allocateMemory(table: SymbolMemoryTable, writer: PrintWriter): Unit = {
    table.symbols.find(s => s.kind.equals("function") && s.name.equals("main")).fold()(l => {
      l.link.fold()(t => {
        t.symbols.foreach(e => writer.write(f"${e.name}%-15s res ${e.size}\n"))
      })
    })
  }

}
