package generator

import java.io.PrintWriter

import parser.ASTNode

object GeneratorIO {

  def traverseWrite(symbols: SymbolMemoryTable, node: ASTNode, writer: PrintWriter, first: Boolean = true): Unit = {
    node.metadata match {
      case Some("INTEGER") =>
        writer.write(f"${" "}%-15s addi  r1,r0,${node.value}\n")
      case Some("FLOAT") =>
        writer.write(f"${" "}%-15s addi  r1,r0,${node.value}\n")
      case Some("ID") =>
        writer.write(f"${" "}%-15s lw    r1,${node.value}(r0)\n")
      case _ =>
        // nested expression
        node.children.foreach(traverseWrite(symbols, _, writer, first = false))
    }
    if(first){
      writer.write(f"${" "}%-15s jl    r15,putint\n")
      writer.write(f"${" "}%-15s addi  r1,r0,10\n")
      writer.write(f"${" "}%-15s putc  r1\n")
    }
  }

}
