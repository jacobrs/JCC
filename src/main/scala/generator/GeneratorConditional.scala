package generator

import java.io.PrintWriter

import parser.ASTNode

object GeneratorConditional {

  var conditionalStatementCounter = 0

  def traverseIfStatement(symbols: SymbolMemoryTable, root: ASTNode, writer: PrintWriter): Unit = {
    val ifID = conditionalStatementCounter
    GeneratorConditional.conditionalStatementCounter += 1
    writer.write(f"${"if" + ifID}%-15s align\n")

    val registerWithResult = collapseConditional(symbols, root.children(2), writer)

    val ifTrueBlock = root.children(5)
    writer.write(f"${" "}%-15s bz    $registerWithResult,else$ifID\n")
    GeneratorTraversers.traverseFunction(symbols, ifTrueBlock, writer)
    writer.write(f"${" "}%-15s j     aftif$ifID\n")
    writer.write(f"${"else" + ifID}%-15s align\n")
    val elseBlock = root.children(7)
    GeneratorTraversers.traverseFunction(symbols, elseBlock, writer)
    writer.write(f"${"aftif" + ifID}%-15s align\n")
  }

  def traverseForStatement(symbols: SymbolMemoryTable, root: ASTNode, writer: PrintWriter): Unit = {

  }

  def collapseConditional(symbols: SymbolMemoryTable, root: ASTNode, writer: PrintWriter): String = {
    val registerPool = GeneratorTraversers.allRegisters
    val registerForFirstTerm = GeneratorTraversers. computeExpressionResult(symbols,
      root.children.head, writer, registerPool)
    val registerSecondTerm = GeneratorTraversers.computeExpressionResult(symbols,
      root.children(1).children(1), writer, registerPool.filterNot(_.equals(registerForFirstTerm)))
    val resultRegister = registerPool.filterNot(r => r.equals(registerForFirstTerm) || r.equals(registerSecondTerm))
    root.children(1).children.head.children.head.value match {
      case "=" =>
        writer.write(f"${" "}%-15s ceq   ${resultRegister.head},$registerForFirstTerm,$registerSecondTerm\n")
      case "<>" =>
        writer.write(f"${" "}%-15s cne   ${resultRegister.head},$registerForFirstTerm,$registerSecondTerm\n")
      case "<" =>
        writer.write(f"${" "}%-15s clt   ${resultRegister.head},$registerForFirstTerm,$registerSecondTerm\n")
      case ">" =>
        writer.write(f"${" "}%-15s cgt   ${resultRegister.head},$registerForFirstTerm,$registerSecondTerm\n")
      case "<=" =>
        writer.write(f"${" "}%-15s cle   ${resultRegister.head},$registerForFirstTerm,$registerSecondTerm\n")
      case ">=" =>
        writer.write(f"${" "}%-15s cge   ${resultRegister.head},$registerForFirstTerm,$registerSecondTerm\n")
      case _ => // invalid relop noop
    }
    resultRegister.head
  }

}
