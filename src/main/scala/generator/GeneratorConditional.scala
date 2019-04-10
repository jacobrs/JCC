package generator

import java.io.PrintWriter
import scalaz.syntax.std.boolean._

import parser.ASTNode

object GeneratorConditional {

  var conditionalStatementCounter = 0
  var loopStatementCounter = 0

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

  def allocateForCounters(symbols: SymbolMemoryTable, root: ASTNode, writer: PrintWriter): Unit = {
    val typeNode = root.children(2).children.head
    val primitiveType: Option[String] = typeNode.metadata.contains("ID").fold(None, Some(typeNode.value))
    val varType: String = primitiveType.fold(typeNode.value)(p => p)
    val memoryRequirement = primitiveType.fold(symbols.symbols.find(_.name.equals(varType)).fold(4)(_.size))(_ => 4)

    writer.write(f"${root.children(3).value}%-15s res   $memoryRequirement\n")
  }

  def traverseForStatement(symbols: SymbolMemoryTable, root: ASTNode, writer: PrintWriter): Unit = {
    val forID = loopStatementCounter
    GeneratorConditional.loopStatementCounter += 1

    val registerPool = GeneratorTraversers.allRegisters

    val evalExpr = GeneratorTraversers.computeExpressionResult(symbols, root.children(5), writer, registerPool)
    writer.write(f"${" "}%-15s sw    ${root.children(3).value}(r0),$evalExpr\n")
    writer.write(f"${"sfor"+forID}%-15s align\n")
    val registerWithResult = collapseConditional(symbols, root.children(7), writer)
    writer.write(f"${" "}%-15s bz    $registerWithResult,endfor$forID\n")

    GeneratorTraversers.traverseFunction(symbols, root.children(11), writer)
    val resultAfterLoop = GeneratorTraversers.computeExpressionResult(symbols, root.children(9).children(2),
      writer, registerPool)

    writer.write(f"${" "}%-15s sw    ${root.children(3).value}(r0),$resultAfterLoop\n")
    writer.write(f"${" "}%-15s j     sfor$forID\n")
    writer.write(f"${"endfor"+forID}%-15s align\n")

  }

  def collapseConditional(symbols: SymbolMemoryTable, root: ASTNode, writer: PrintWriter): String = {
    val registerPool = GeneratorTraversers.allRegisters
    val registerForFirstTerm = GeneratorTraversers. computeExpressionResult(symbols,
      root.children.head, writer, registerPool)
    val registerSecondTerm = GeneratorTraversers.computeExpressionResult(symbols,
      (root.children.size > 2).fold(root.children(2), root.children(1).children(1)),
      writer, registerPool.filterNot(_.equals(registerForFirstTerm)))
    val resultRegister = registerPool.filterNot(r => r.equals(registerForFirstTerm) || r.equals(registerSecondTerm))

    (root.children.size > 2).fold(root.children(1).children.head.value,
      root.children(1).children.head.children.head.value) match {
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
