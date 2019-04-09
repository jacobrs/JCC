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
    tree.children.foreach(validateAssignments)
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

  def validateAssignments(root: ASTNode): Unit = {
    // extract current node
    root.value match {
      case "funcDef" =>
        validateFunctionBody(root)
      case "funcBody" =>
        validateAssignments(root, globalTable.symbols.find(
          p => p.kind.equals("function") && p.name.equals("main")).fold(new SymbolTable("empty"))
        (_.link.getOrElse(new SymbolTable("empty"))))
      case _ =>
        root.children.foreach(validateAssignments)
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
    val functionReturnType = head.children.head.children.head.value
    val optionalIDSR = head.children(1)
    val optionalIDSRExt = head.children(1).children(1)

    (optionalIDSRExt.children.size < 2).fold({
        // function belongs to global table
        val functionName = optionalIDSR.children.head.value
        val functionSymbolTable = new SymbolTable(functionName)
        val parameterSymbols = getFunctionParameters(head)
        parameterSymbols.foreach(functionSymbolTable.addSymbol)
        traverseAttributes(root, functionSymbolTable)
        validateFunctionReturnType(functionReturnType, root, functionSymbolTable)
        Some(SymbolEntry(functionName, "function", functionReturnType, Some(functionSymbolTable)))
      }, {
        // function belongs to class
        addFunctionToClass(root)
        None
      }
    )
  }

  def validateFunctionBody(root: ASTNode): Unit = {
    val head = root.children.head
    val optionalIDSR = head.children(1)
    val optionalIDSRExt = head.children(1).children(1)

    (optionalIDSRExt.children.size < 2).fold({
        // function belongs to global table
        val functionName = optionalIDSR.children.head.value
        validateAssignments(root, globalTable.symbols.find(
          p => p.kind.equals("function") && p.name.equals(functionName)).fold(new SymbolTable("empty"))
        (_.link.getOrElse(new SymbolTable("empty"))))
      }, {
        // function belongs to class
        val className = optionalIDSR.children.head.value
        validateAssignments(root, globalTable.symbols.find(
          p => p.kind.equals("class") && p.name.equals(className)).fold(new SymbolTable("empty"))
            (_.link.getOrElse(new SymbolTable("empty"))))
      }
    )
  }

  def validateAssignments(root: ASTNode, scope: SymbolTable): Unit = {
    root.value match {
      case "assignStatAndVar" =>
        // detected variable
        val varType = root.children.head.value
        var varName = root.children(1).value
        if (varName.equals("statOrVarExt")) {
          varName = root.children(1).children.head.value
          if (varName.equals("EPSILON") && root.children(2).children.size > 1) {
            // this is an assignment statement
            val varIdentifier = root.children.head.value
            val expr = root.children(2).children(1)
            validateSubtreeIsOnlyOfTypeX(deriveTypeFromID(varIdentifier, scope).getOrElse("Null"), expr, scope)
          } else if (root.children(2).children.size > 1) {
            val expr = root.children(2).children(1)
            validateSubtreeIsOnlyOfTypeX(deriveTypeFromID(varType, scope).getOrElse("Null"), expr, scope)
          }
        }
      case _ =>
        root.children.foreach(validateAssignments(_, scope))
    }
  }

  /**
    * This function takes an AST funcDef node that has a class identifier in optionalIDSRExt
    * and adds it to the appropriate class table in the global table
    * @param functionRoot is an AST funcDef node to be added to a class
    */
  def addFunctionToClass(functionRoot: ASTNode): Unit = {
    val head = functionRoot.children.head
    val functionReturnType = head.children.head.children.head.value
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
        SymbolEntry(functionName, "function", functionReturnType, Some(functionSymbolTable))
      val newClassTable = s.link.getOrElse(new SymbolTable(className))
      newClassTable.symbols = newClassTable.symbols.filter(s => !s.name.equals(functionName))
      newClassTable.addSymbol(classFunctionSymbol)
      validateFunctionReturnType(functionReturnType, functionRoot, newClassTable)
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
        var varName = root.children(1).value
        val dimensions = parseArrayDimensions(root)
        if (varName != "statOrVarExt") {
          table.symbols.exists(c => c.name.equals(varName)).fold(
            errors = errors ++ Seq(SemanticError(s"[error] $varName was already declared in this scope", root.location)),
            table.addSymbol(SymbolEntry(varName, "variable", varType, None, dimensions))
          )
        } else {
          varName = root.children(1).children.head.value
          if (root.children(2).children.size <= 1) {
            table.symbols.exists(c => c.name.equals(varName)).fold(
              errors = errors ++ Seq(SemanticError(s"[error] $varName was already declared in this scope", root.location)),
              table.addSymbol(SymbolEntry(varName, "variable", varType, None, dimensions))
            )
          }
        }
      case "genericDecl" =>
        // detected variable declaration
        val varType = root.children.head.children.head.value
        val varName = root.children(1).value
        val dimensions = parseArrayDimensions(root.children(2))
        table.symbols.exists(c => c.name.equals(varName)).fold(
          errors = errors ++ Seq(SemanticError(s"[error] $varName was already declared in this scope", root.location)),
          table.addSymbol(SymbolEntry(varName, "variable", varType, None, dimensions))
        )
      case _ =>
        root.children.foreach(traverseAttributes(_, table))
    }
  }

  /**
    * parseArrayDimensions parses a subtree of the AST to find an arrays dimensions
    * @param root root of the AST subtree that contains dimensions
    * @param currentDimensions dimensions found so far
    * @return all dimensions found in order
    */
  def parseArrayDimensions(root: ASTNode, currentDimensions: Seq[Int] = Seq.empty): Seq[Int] = {
    var newDimensions = currentDimensions
    root.value match {
      case "arraySize" =>
        newDimensions = Seq(root.children(1).value.toInt) ++ currentDimensions
      case _ =>
        root.children.foreach(node =>
          newDimensions = newDimensions ++ parseArrayDimensions(node, currentDimensions))
    }
    newDimensions
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

  def validateFunctionReturnType(expectedReturnType: String, root: ASTNode, scopeRoot: SymbolTable): Unit = {
    (root.children.count(_.value.equals("return")) > 0).fold(
        validateSubtreeIsOnlyOfTypeX(expectedReturnType, root.children(2), scopeRoot),
        root.children.foreach(validateFunctionReturnType(expectedReturnType, _, scopeRoot))
    )
  }

  def validateSubtreeIsOnlyOfTypeX(x: String, root: ASTNode, scopeRoot: SymbolTable): Unit = {
    x match {
      case "integer" =>
        root.metadata.fold()({
          case "INTEGER" =>
          case "FLOAT" =>
            errors = errors ++ Seq(SemanticError(s"[error] invalid expr of type float expected $x",
              root.location))
          case t =>
            testTypeMatch(root, x, scopeRoot)
        })
      case "float" =>
        root.metadata.fold()({
          case "FLOAT" =>
          case "INTEGER" =>
            errors = errors ++ Seq(SemanticError(s"[error] invalid expr of type integer expected $x",
              root.location))
          case t =>
            testTypeMatch(root, x, scopeRoot)
        })
      case _ =>
        root.metadata.fold()({
          case "ID" =>
            testTypeMatch(root, x, scopeRoot)
          case t =>
            errors = errors ++ Seq(SemanticError(s"[error] invalid expr of type ${t.toLowerCase} expected $x",
              root.location))
        })
    }
    root.children.foreach(validateSubtreeIsOnlyOfTypeX(x, _, scopeRoot))
  }

  def testTypeMatch(root: ASTNode, x: String, scopeRoot: SymbolTable): Unit = {
    val idType = deriveTypeFromID(root.value, scopeRoot)
    idType.fold(
      errors = errors ++ Seq(SemanticError(s"[error] ${root.value} has no derived type", root.location))
    )( t => {
        if (t.equals(x)) {
          println(s"[debug] derived type ${idType.getOrElse("Null")} of ${root.value}")
        } else {
          errors = errors ++ Seq(SemanticError(s"[error] invalid expr of type ${idType.getOrElse("Null")}" +
            s" expected $x", root.location))
        }
      }
    )
  }

  def deriveTypeFromID(identifier: String, scopeRoot: SymbolTable): Option[String] = {
    var derivedType: Option[String] = None
    scopeRoot.symbols.foreach(f => {
        if (f.name.equals(identifier)){
          derivedType = Some(f.dataType)
        }else if(derivedType.isEmpty){
          derivedType = f.link.fold(derivedType)(deriveTypeFromID(identifier, _))
        }
      })
    derivedType
  }
}

object SymbolTableGenerator {
  case class SemanticError(message: String, location: Location)
}
