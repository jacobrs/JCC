package generator

import java.io.PrintWriter

import parser.ASTNode
import scalaz.syntax.std.boolean._
import semantic.SymbolTable

object GeneratorTraversers {

  val allRegisters: Seq[String] =
    Seq("r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "r13")

  case class FunctionHeadExtractionResult(name: String, className: Option[String] = None)

  def traverseMain(symbols: SymbolMemoryTable, ast: ASTNode, writer: PrintWriter): Unit = {
    ast.children.foreach(c => c.value match {
      case "funcBody" =>
        // this is the main function
        writer.write(f"main            align\n")
        traverseFunction(symbols, c, writer)
      case _ => // no op
    })
  }

  def traverseOtherFunctions(symbols: SymbolMemoryTable, ast: ASTNode, writer: PrintWriter): Unit = {
    ast.children.find(_.value.equals("funcDefWrapper")).fold()(
      traverseFunctionDef(symbols, _, writer)
    )
  }

  def traverseFunctionDef(symbols: SymbolMemoryTable, ast: ASTNode, writer: PrintWriter): Unit = {
    ast.children.foreach(n => n.value match {
      case "funcDef" => // this is a function
        val result = extractFunctionName(n.children.head)
        val functionTag = result.className.fold("")(_+"_") + result.name
        CodeGenerator.allocateMemoryForFunction(symbols, result.name, result.className, writer)
        writer.write(f"$functionTag%-15s align\n")
        n.children.foreach(traverseFunction(symbols, _, writer))
        writer.write(f"${" "}%-15s jr    r14\n")
      case _ => // not a function maybe a wrapper
        ast.children.foreach(traverseFunctionDef(symbols, _, writer))
    })
  }

  def extractFunctionName(funcHeadNode: ASTNode): FunctionHeadExtractionResult = {
    (funcHeadNode.children(1).children(1).children.size > 1).fold(
      FunctionHeadExtractionResult(funcHeadNode.children(1).children(1).children.last.value,
        Some(funcHeadNode.children(1).children.head.value)),
      FunctionHeadExtractionResult(funcHeadNode.children(1).children.head.value)
    )
  }

  def traverseFunction(symbols: SymbolMemoryTable, node: ASTNode, writer: PrintWriter): Unit = {
    node.value match {
      case "assignStatAndVar" =>
        val target = node.children((node.children.size > 3).fold(1, 0))
        traverseAssignStartAndVar(symbols, node.children.last, writer, target.value, node)
        traverseAssignStartAndVar(symbols, node.children(1), writer, target.value, node)
      case "statement" =>
        if(node.children.exists(_.value.equals("write"))) {
          GeneratorIO.traverseWrite(symbols, node.children(2), writer)
          node.children.foreach(traverseFunction(symbols, _, writer))
        }
        else if(node.children.exists(_.value.equals("if"))) {
          GeneratorConditional.traverseIfStatement(symbols, node, writer)
        }
        else if(node.children.exists(_.value.equals("for"))) {
          GeneratorConditional.traverseForStatement(symbols, node, writer)
        }
        else {
          node.children.foreach(traverseFunction(symbols, _, writer))
        }
      case _ => node.children.foreach(traverseFunction(symbols, _, writer))
    }
  }

  def traverseAssignStartAndVar(symbols: SymbolMemoryTable, node: ASTNode, writer: PrintWriter,
                                target: String, parentNode: ASTNode): Unit = {
    node.value match {
      case "optionalAssignOp" =>
        if(node.children.size > 1){
          val expr = computeExpressionResult(symbols, node.children(1), writer, allRegisters)
          writer.write(f"${" "}%-15s sw    $target(r0),$expr %% generated by line ${node.location.row}\n")
        }
      case "statOrVarExt" =>
        if(node.children.size >= 4){ // this is a function call
          val functionName = parentNode.children.head.value
          val functionTable = symbols.symbols.find(_.name.equals(functionName)).
            fold(new SymbolMemoryTable(new SymbolTable("undefined")))(_.link.get)
          transferFunctionParameters(functionTable, node.children(1), writer)
          writer.write(f"${" "}%-15s jl    r14,$functionName\n")
        }
      case _ => // noop
    }
  }

  def transferFunctionParameters(functionTable: SymbolMemoryTable, node: ASTNode, writer: PrintWriter): Unit = {
    val parameters = functionTable.symbols.filter(_.kind.equals("parameter"))
    var expressionRegisters = recursivelyFindExpressions(functionTable, node, writer, allRegisters)
    parameters.foreach(p => {
      writer.write(f"${" "}%-15s sw    ${p.name}(r0),${expressionRegisters.head}\n")
      expressionRegisters = expressionRegisters.tail
    })
  }

  def recursivelyFindExpressions(functionTable: SymbolMemoryTable, node: ASTNode, writer: PrintWriter,
                                 availableRegisters: Seq[String]): Seq[String] = {
    var registerPool = allRegisters
    var expressionRegisters = Seq.empty[String]
    node.children.foreach(n => n.value match {
      case "expr" =>
        registerPool.nonEmpty.fold({
          val dedicatedRegister = computeExpressionResult(functionTable, n, writer, registerPool)
          expressionRegisters = expressionRegisters ++ Seq(dedicatedRegister)
          registerPool = registerPool.tail
        }, {
          println(s"[error] ran out of registers for function parameters function ${functionTable.name}")
        })
      case _ =>
        expressionRegisters = expressionRegisters ++ recursivelyFindExpressions(functionTable, n, writer, registerPool)
    })
    expressionRegisters
  }

  def computeExpressionResult(symbols: SymbolMemoryTable, node: ASTNode, writer: PrintWriter,
                              availableRegisters: Seq[String]): String = {
    node.metadata match {
      case Some(_) =>
        computeFactor(symbols, node, writer, availableRegisters)
      case None =>
        node.value match {
          case "arithExpr" =>
            (node.children(1).children.size > 1).fold({
              val t1 = computeExpressionResult(symbols, node.children.head, writer, availableRegisters.tail)
              var navail = availableRegisters.tail
              val t2 = computeExpressionResult(symbols, node.children(1), writer, navail.tail)
              navail = navail.tail
              node.children(1).children.head.children.head.value match {
                case "+" => writer.write(f"${" "}%-15s add   ${availableRegisters.head},$t1,$t2\n")
                case "-" => writer.write(f"${" "}%-15s sub   ${availableRegisters.head},$t1,$t2\n")
              }
              availableRegisters.head
            },{
              computeExpressionResult(symbols, node.children.head, writer, availableRegisters)
            })
          case "term" =>
            (node.children(1).children.size > 1).fold({
              val t1 = computeExpressionResult(symbols, node.children.head, writer, availableRegisters.tail)
              var navail = availableRegisters.tail
              val t2 = computeExpressionResult(symbols, node.children(1), writer, navail.tail)
              navail = navail.tail
              node.children(1).children.head.children.head.value match {
                case "*" => writer.write(f"${" "}%-15s mul   ${availableRegisters.head},$t1,$t2\n")
                case "/" => writer.write(f"${" "}%-15s div   ${availableRegisters.head},$t1,$t2\n")
              }
              availableRegisters.head
            },{
              computeFactor(symbols, node.children.head, writer, availableRegisters)
            })
          case _ =>
            node.children.foreach(computeExpressionResult(symbols, _, writer, availableRegisters))
            availableRegisters.head
        }
    }
  }

  def computeFactor(symbols: SymbolMemoryTable, node: ASTNode, writer: PrintWriter,
                    availableRegisters: Seq[String]): String ={
    node.metadata match {
      case Some("INTEGER") =>
        writer.write(f"${" "}%-15s addi  ${availableRegisters.head},r0,${node.value}\n")
        availableRegisters.head
      case Some("FLOAT") =>
        writer.write(f"${" "}%-15s addi  ${availableRegisters.head},r0,${node.value}\n")
        availableRegisters.head
      case Some("ID") =>
        writer.write(f"${" "}%-15s lw    ${availableRegisters.head},${node.value}(r0)\n")
        availableRegisters.head
      case _ =>
        node.children.map(computeFactor(symbols, _, writer, availableRegisters))
        availableRegisters.head
    }
  }

}
