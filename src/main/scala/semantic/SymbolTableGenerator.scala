package semantic

import parser.ASTNode
import semantic.Symbol.SymbolEntry
import scalaz.syntax.std.boolean._
import semantic.SymbolTableGenerator.SemanticError
import tokenizer.Token.Location

class SymbolTableGenerator {

  var globalTable = new SymbolTable("global")
  var currentRoot = new ASTNode("prog")
  var errors: Seq[SemanticError] = Seq.empty

  def generate(tree: ASTNode): Unit = {
    currentRoot = tree
    tree.children.foreach(traverse)
  }

  def traverse(root: ASTNode): Unit = {
    // extract current node
    root.value match {
      case "classDecl" =>
        println("[info] classDecl creating class symbol")
        globalTable.addSymbol(traverseClass(root))
      case "funcDef" =>
        println("[info] funcDef creating function symbol")
        traverseFunction(root).fold()(globalTable.addSymbol)
      case "funcBody" =>
        // this is the main body
        println("[info] funcBody creating class symbol, main")
        val functionTable = new SymbolTable("main")
        traverseAttributes(root, functionTable)
        globalTable.addSymbol(SymbolEntry("main", "function", "void", Some(functionTable)))
      case _ =>
        root.children.foreach(traverse)
    }
  }

  def traverseClass(root: ASTNode): SymbolEntry = {
    val className = root.children(1).value
    val classSymbolTable = new SymbolTable(className)
    val inheritedClasses = getInheritedClasses(root)
    inheritedClasses.foreach(classSymbolTable.addSymbol)
    root.children.foreach(child => {
      traverseAttributes(child, classSymbolTable)
    })
    SymbolEntry(className, "class", "", Some(classSymbolTable))
  }

  def traverseFunction(root: ASTNode): Option[SymbolEntry] = {
    val head = root.children.head
    val optionalIDSR = head.children(1)
    val optionalIDSRExt = head.children(1).children(1)

    (optionalIDSRExt.children.size < 2).fold({
        // function belongs to global table
        val functionName = optionalIDSR.children.head.value
        val functionSymbolTable = new SymbolTable(functionName)
        val parameterSymbols = getFunctionParameters(head)
        parameterSymbols.foreach(functionSymbolTable.addSymbol)
        traverseAttributes(root, functionSymbolTable)
        Some(SymbolEntry(functionName, "function", head.children.head.children.head.value, Some(functionSymbolTable)))
      }, {
        // function belongs to class
        addFunctionToClass(root)
        None
      }
    )
  }

  /**
    * This function takes an AST funcDef node that has a class identifier in optionalIDSRExt
    * and adds it to the appropriate class table in the global table
    * @param functionRoot is an AST funcDef node to be added to a class
    */
  def addFunctionToClass(functionRoot: ASTNode): Unit = {
    val head = functionRoot.children.head
    val parameterSymbols = getFunctionParameters(head)
    val optionalIDSR = head.children(1)
    val optionalIDSRExt = head.children(1).children(1)
    val functionName = optionalIDSRExt.children(1).value
    val className = optionalIDSR.children.head.value
    val classSymbol = globalTable.symbols.find(s => s.kind.equals("class") && s.name.equals(className))
    classSymbol.fold(
      errors = errors ++ Seq(
        SemanticError(s"[error] $className::$functionName semantic error $className not defined", head.location))
    )(s => {
      val functionSymbolTable = new SymbolTable(functionName)
      parameterSymbols.foreach(functionSymbolTable.addSymbol)
      traverseAttributes(functionRoot, functionSymbolTable)
      val classFunctionSymbol =
        SymbolEntry(functionName, "function", head.children.head.children.head.value, Some(functionSymbolTable))
      val newClassTable = s.link.getOrElse(new SymbolTable(className))
      newClassTable.symbols = newClassTable.symbols.filter(s => !s.name.equals(functionName))
      newClassTable.addSymbol(classFunctionSymbol)
      globalTable.symbols = globalTable.symbols.filter(s => !(s.kind.equals("class") && s.name.equals(className))) ++
        Seq(s.copy(link = Some(newClassTable)))
    })
  }

  /**
    * This function takes a root that might contain variables in the subtree and returns all
    * SymbolEntry objects found in the subtree
    * @param root is the root of the subtree to be scanned
    * @param table is te table that the symbols will be appended to
    */
  private def traverseAttributes(root: ASTNode, table: SymbolTable): Unit = {
    root.value match {
      case "assignStatAndVar" =>
        // detected variable
        val varType = root.children.head.value
        val varName = root.children(1).value
        if(varName != "statOrVarExt") {
          table.symbols.exists(c => c.name.equals(varName)).fold(
            errors = errors ++ Seq(SemanticError(s"[error] $varName was already declared in this scope", root.location)),
            table.addSymbol(SymbolEntry(varName, "variable", varType, None))
          )
        }
      case "genericDecl" =>
        // detected variable declaration
        val varType = root.children.head.children.head.value
        val varName = root.children(1).value
        table.symbols.exists(c => c.name.equals(varName)).fold(
          errors = errors ++ Seq(SemanticError(s"[error] $varName was already declared in this scope", root.location)),
          table.addSymbol(SymbolEntry(varName, "variable", varType, None))
        )
      case _ =>
        root.children.foreach(traverseAttributes(_, table))
    }
  }

  def getFunctionParameters(root: ASTNode): Seq[SymbolEntry] = {
    root.children.find(_.value.equals("fParams")).fold(Seq.empty[SymbolEntry])(
      fParams => (fParams.children.size > 1).fold[Seq[SymbolEntry]](
        Seq(SymbolEntry(fParams.children(1).value, "parameter", fParams.children.head.children.head.value, None))
          ++ parseTail(fParams.children.find(_.value.equals("fParamsTailWrapper")).get),
        Seq.empty[SymbolEntry])
    )
  }

  def parseTail(fParamsTailWrapper: ASTNode): Seq[SymbolEntry] = {
    (fParamsTailWrapper.children.size > 1).fold[Seq[SymbolEntry]](
      Seq(SymbolEntry(fParamsTailWrapper.children.head.children(2).value, "parameter",
        fParamsTailWrapper.children.head.children(1).value, None))
        ++ parseTail(fParamsTailWrapper.children.head.children(3)),
      Seq.empty[SymbolEntry]
    )
  }

  def getInheritedClasses(root: ASTNode): Seq[SymbolEntry] = {
    root.children.find(_.value.equals("optionalIDExt")).fold(Seq.empty[SymbolEntry])(
      optionalIDExt => (optionalIDExt.children.size > 1).fold[Seq[SymbolEntry]]({
          val inheritedClassName = optionalIDExt.children(1).value
          val classSymbol = globalTable.symbols.find(s => s.name.equals(inheritedClassName) && s.kind.equals("class"))
          classSymbol.fold[Seq[SymbolEntry]]({
              errors = errors ++ Seq(
                SemanticError(s"[error] inherited class $inheritedClassName not defined", optionalIDExt.location))
              Seq.empty
            })({
              s =>
                s.link.getOrElse(new SymbolTable("none")).symbols ++
                  parseIdWrapper(optionalIDExt.children.find(_.value.equals("idWrapper")).get)
            })
        },
        Seq.empty[SymbolEntry])
    )
  }

  def parseIdWrapper(idWrapper: ASTNode): Seq[SymbolEntry] = {
    (idWrapper.children.size > 1).fold[Seq[SymbolEntry]]({
        val inheritedClassName = idWrapper.children.head.children(1).value
        val classSymbol = globalTable.symbols.find(s => s.name.equals(inheritedClassName) && s.kind.equals("class"))
        classSymbol.fold[Seq[SymbolEntry]]({
          errors = errors ++ Seq(
            SemanticError(s"[error] inherited class $inheritedClassName not defined", idWrapper.location))
            Seq.empty
          })({
            s =>
              s.link.getOrElse(new SymbolTable("none")).symbols ++
                parseIdWrapper(idWrapper.children(1))
          })
      },
      Seq.empty[SymbolEntry]
    )
  }
}

object SymbolTableGenerator {
  case class SemanticError(message: String, location: Location)
}
