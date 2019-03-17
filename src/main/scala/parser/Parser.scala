package parser

import tokenizer.Token
import tokenizer.Token._

class Parser {

}

object Parser {

  var lookahead: Token = OPERATOR("")
  var tokenIterator: Iterator[Token] = Iterator()
  var root: ASTNode = _

  case class ParseResult(tree: ASTNode, errors: Seq[String])

  def parse(tokens: Seq[Token]): ParseResult = {
    root = new ASTNode("prog")
    tokenIterator = tokens.iterator
    lookahead = tokenIterator.next()

    if(prog(root)) {
      ParseResult(root, Seq.empty)
    }else{
      ParseResult(root, Seq.empty)
    }
  }

  private def m(t: Token, parent: ASTNode): Boolean = {
    var result = lookahead == t

    t match {
      case ID(_) =>
        lookahead match {
          case ID(_) => result = true
          case _ =>
        }
      case INTEGER(_) =>
        lookahead match {
          case INTEGER(_) => result = true
          case _ =>
        }
      case FLOAT(_) =>
        lookahead match {
          case FLOAT(_) => result = true
          case _ =>
        }
      case _ =>
    }

    if(result){
      parent.addChild(new ASTNode(lookahead.value))
      if(tokenIterator.hasNext) {
        lookahead = tokenIterator.next()
      } else {
        lookahead = TERMINATION("")
      }
    }
    result
  }

  private def prog(parent: ASTNode): Boolean = {
    var error = false
    if(classDeclWrapper(parent) && funcDefWrapper(parent) && m(RESERVED("main"), parent)
      && funcBody(parent) && m(PUNCTUATION(";"), parent) && m(TERMINATION(""), parent)) {
      println("prog -> classDeclWrapper funcDefWrapper main funcBody ;")
      error = true
    }
    !error
  }

  private def classDeclWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("classDeclWrapper")
    lookahead match {
      case RESERVED("class") =>
        if (classDecl(root) && classDeclWrapper(root)) {
          println("classDeclWrapper -> classDecl classDeclWrapper")
        } else {
          error = true
        }
      case RESERVED("float") | RESERVED("integer") | ID(_) | RESERVED("main") =>
        println("classDeclWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def classDecl(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("classDecl")
    lookahead match {
      case RESERVED("class") =>
        if (m(RESERVED("class"), root) && m(ID(""), root) && optionalIDExt(root) && m(PUNCTUATION("{"), root)
          && genericDeclWrapper(root) && m(PUNCTUATION("}"), root) && m(PUNCTUATION(";"), root)){
          println("classDecl -> class id optionalIDExt { genericDeclWrapper } ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def funcDefWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("funcDefWrapper")
    lookahead match {
      case RESERVED("float") | RESERVED("integer") | ID(_) =>
        if (funcDef(root) && funcDefWrapper(root)) {
          println("funcDefWrapper -> funcDef funcDefWrapper")
        } else {
          error = true
        }
      case RESERVED("main") =>
        println("funcDefWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def funcDef(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("funcDef")
    lookahead match {
      case RESERVED("float") | RESERVED("integer") | ID(_) =>
        if (funcHead(root) && funcBody(root) && m(PUNCTUATION(";"), root)) {
          println("funcDef -> funcHead funcBody")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def funcBody(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("funcBody")
    lookahead match {
      case PUNCTUATION("{") =>
        if (m(PUNCTUATION("{"), root) && statementWrapper(root) && m(PUNCTUATION("}"), root)) {
          println("funcBody -> { statementWrapper }")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def optionalIDExt(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("optionalIDExt")
    lookahead match {
      case PUNCTUATION(":") =>
        if (m(PUNCTUATION(":"), root) && m(ID(""), root) && idWrapper(root)) {
          println("optionalIDExt -> : id idWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("{") =>
        println("optionalIDExt -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def idWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idWrapper")
    lookahead match {
      case PUNCTUATION(",") =>
        if (idEntity(root) && idWrapper(root)) {
          println("idWrapper -> idEntity idWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("{") =>
        println("idWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def idEntity(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idEntity")
    lookahead match {
      case PUNCTUATION(",") =>
        if (m(PUNCTUATION(","), root) && m(ID(""), root)) {
          println("idEntity -> , id")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def genericDeclWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("genericDeclWrapper")
    lookahead match {
      case RESERVED("integer") | ID(_) | RESERVED("float") =>
        if (genericDecl(root) && genericDeclWrapper(root)) {
          println("genericDeclWrapper -> genericDecl genericDeclWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
        println("genericDeclWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def genericDecl(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("genericDecl")
    lookahead match {
      case RESERVED("integer") | ID(_) | RESERVED("float") =>
        if (`type`(root) && m(ID(""), root) && genericExtension(root) && m(PUNCTUATION(";"), root)) {
          println("genericDecl -> type id genericExtension ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def genericExtension(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("genericExtension")
    lookahead match {
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && fParams(root) && m(PUNCTUATION(")"), root)) {
          println("genericExtension -> ( fParams )")
        } else {
          error = true
        }
      case PUNCTUATION(";") | PUNCTUATION("=") | PUNCTUATION("[") =>
        if (arraySizeWrapper(root)) {
          println("genericExtension -> arraySizeWrapper")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def funcHead(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("funcHead")
    lookahead match {
      case RESERVED("float") | RESERVED("integer") | ID(_) =>
        if (`type`(root) && optionalIDSR(root) && m(PUNCTUATION("("), root)
            && fParams(root) && m(PUNCTUATION(")"), root)) {
          println("funcHead -> type optionalIDSR ( fParams )")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def optionalIDSR(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("optionalIDSR")
    lookahead match {
      case ID(_) =>
        if (m(ID(""), root) && optionalIDSRExt(root)) {
          println("optionalIDSR -> id optionalIDSRExt")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def optionalIDSRExt(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("optionalIDSRExt")
    lookahead match {
      case PUNCTUATION("::") =>
        if (m(PUNCTUATION("::"), root) && m(ID(""), root)) {
          println("optionalIDSRExt -> :: id")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        println("optionalIDSRExt -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def `type`(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("type")
    lookahead match {
      case RESERVED("float") =>
        if (m(RESERVED("float"), root)) {
          println("type -> float")
        } else {
          error = true
        }
      case RESERVED("integer") =>
        if (m(RESERVED("integer"), root)) {
          println("type -> integer")
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID(""), root)) {
          println("type -> id")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def fParams(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("fParams")
    lookahead match {
      case RESERVED("float") | ID(_) | RESERVED("integer") =>
        if (`type`(root) && m(ID(""), root) && arraySizeWrapper(root) && fParamsTailWrapper(root)) {
          println("fParams -> type id arraySizeWrapper fParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("fParams -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def fParamsTailWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("fParamsTailWrapper")
    lookahead match {
      case PUNCTUATION(",") =>
        if (fParamsTail(root) && fParamsTailWrapper(root)) {
          println("fParamsTailWrapper -> fParamsTail fParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("fParamsTailWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def fParamsTail(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("fParamsTail")
    lookahead match {
      case PUNCTUATION(",") =>
        if (m(PUNCTUATION(","), root) && `type`(root) && m(ID(""), root) && arraySizeWrapper(root)) {
          println("fParamsTail -> , type id arraySizeWrapper")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def aParams(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("aParams")
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | ID(_) | INTEGER(_) | OPERATOR("!") =>
        if (expr(root) && aParamsTailWrapper(root)) {
          println("aParams -> expr aParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("aParams -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def aParamsTailWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("aParamsTailWrapper")
    lookahead match {
      case PUNCTUATION(",") =>
        if (aParamsTail(root) && aParamsTailWrapper(root)) {
          println("aParamsTailWrapper -> aParamsTail aParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("aParamsTailWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def aParamsTail(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("aParamsTail")
    lookahead match {
      case PUNCTUATION(",") =>
        if (m(PUNCTUATION(","), root) && expr(root)) {
          println("aParamsTail -> , expr")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def statementWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("statementWrapper")
    lookahead match {
      case RESERVED("float") | RESERVED("for") | RESERVED("if") | RESERVED("read")
        | RESERVED("return") | RESERVED("write") | RESERVED("integer") | ID(_) =>
        if (statement(root) && statementWrapper(root)) {
          println("statementWrapper -> statement statementWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
        println("statementWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def assignStatAndVar(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("assignStatAndVar")
    lookahead match {
      case RESERVED("integer") =>
        if (m(RESERVED("integer"), root) && m(ID(""), root) && genericExtension(root) && assignOp(root)
          && expr(root) && m(PUNCTUATION(";"), root)) {
          println("assignStatAndVar -> integer id genericExtension assignOp expr ;")
        } else {
          error = true
        }
      case RESERVED("float") =>
        if (m(RESERVED("float"), root) && m(ID(""), root) && genericExtension(root) && assignOp(root)
          && expr(root) && m(PUNCTUATION(";"), root)) {
          println("assignStatAndVar -> float id genericExtension assignOp expr ;")
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID(""), root) && statOrVarExt(root) && assignOp(root) && expr(root) && m(PUNCTUATION(";"), root)) {
          println("assignStatAndVar -> id statOrVarExt assignOp expr ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def statOrVarExt(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("statOrVarExt")
    lookahead match {
      case PUNCTUATION(".") | PUNCTUATION("[") =>
        if (indiceWrapper(root) && indiceExtInt(root)) {
          println("statOrVarExt -> indiceWrapper indiceExtInt")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) &&
          m(PUNCTUATION("."), root) && variableIdnestWrapper(root)) {
          println("statOrVarExt -> ( aParams ) . variableIdnestWrapper")
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID(""), root) && genericExtension(root) && m(PUNCTUATION(";"), root)) {
          println("statOrVarExt -> id genericExtension ;")
        } else {
          error = true
        }
      case PUNCTUATION("=") =>
        println("statOrVarExt -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def indiceExtInt(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("indiceExtInt")
    lookahead match {
      case PUNCTUATION(".") =>
        if (m(PUNCTUATION("."), root) && variableIdnestWrapper(root)) {
          println("indiceExtInt -> . variableIdnestWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
        println("indiceExtInt -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def statement(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("statement")
    lookahead match {
      case RESERVED("float") | ID(_) | RESERVED("integer") =>
        if (assignStatAndVar(root)) {
          println("statement -> assignStatAndVar")
        } else {
          error = true
        }
      case RESERVED("if") =>
        if(m(RESERVED("if"), root) && m(PUNCTUATION("("), root) && expr(root) && m(PUNCTUATION(")"), root)
          && m(RESERVED("then"), root) && statBlock(root) && m(RESERVED("else"), root) && statBlock(root)
          && m(PUNCTUATION(";"), root)) {
          println("statement -> if ( expr ) then statBlock else statBlock ;")
        } else {
          error = true
        }
      case RESERVED("for") =>
        if(m(RESERVED("for"), root) && m(PUNCTUATION("("), root) && `type`(root) && m(ID(""), root) &&
          assignOp(root) && expr(root) && m(PUNCTUATION(";"), root) && relExpr(root) && m(PUNCTUATION(";"), root)
          && assignStat(root) && m(PUNCTUATION(")"), root) && statBlock(root) && m(PUNCTUATION(";"), root)){
          println("statement -> for ( type id assignOp expr ; relExpr ; assignStat ) statBlock ;")
        } else {
          error = true
        }
      case RESERVED("read") =>
        if(m(RESERVED("read"), root) && m(PUNCTUATION("("), root) && variableIdnestWrapper(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
          println("statement -> read ( variable ) ;")
        } else {
          error = true
        }
      case RESERVED("write") =>
        if(m(RESERVED("write"), root) && m(PUNCTUATION("("), root) && variableIdnestWrapper(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
          println("statement -> write ( variable ) ;")
        } else {
          error = true
        }
      case RESERVED("return") =>
        if(m(RESERVED("return"), root) && m(PUNCTUATION("("), root) && variableIdnestWrapper(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
          println("statement -> return ( variable ) ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def arraySizeWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("arraySizeWrapper")
    lookahead match {
      case PUNCTUATION("[") =>
        if (arraySize(root) && arraySizeWrapper(root)) {
          println("arraySizeWrapper -> arraySize arraySizeWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") | PUNCTUATION(",") | PUNCTUATION(";") | PUNCTUATION("=") =>
        println("arraySizeWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def arraySize(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("arraySize")
    lookahead match {
      case PUNCTUATION("[") =>
        if (m(PUNCTUATION("["), root) && m(INTEGER(""), root) && m(PUNCTUATION("]"), root)) {
          println("arraySize -> [ intNum ]")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def assignStat(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("assignStat")
    lookahead match {
      case PUNCTUATION("=") | ID(_) =>
        if (variableIdnestWrapper(root) && assignOp(root) && expr(root)) {
          println("assignStat -> variable assignOp expr")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def expr(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("expr")
    lookahead match {
      case PUNCTUATION(",") | OPERATOR("!") | FLOAT(_) | INTEGER(_) | ID(_) =>
        if (arithExpr(root) && exprExt(root)) {
          println("expr -> arithExpr exprExt")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def exprExt(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("exprExt")
    lookahead match {
      case PUNCTUATION("=") | OPERATOR(">") | OPERATOR(">=")
           | OPERATOR("<=") | OPERATOR("<") | OPERATOR("!=") =>
        if (relOp(root) && arithExpr(root)) {
          println("exprExt -> relOp arithExpr")
        } else {
          error = true
        }
      case PUNCTUATION("(") | FLOAT(_) | INTEGER(_) | OPERATOR("!") =>
        if (term(root)) {
          println("exprExt -> term")
        } else {
          error = true
        }
      case PUNCTUATION(",") | PUNCTUATION(")") | PUNCTUATION(";") =>
          println("exprExt -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def statBlock(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("statBlock")
    lookahead match {
      case PUNCTUATION("{") =>
        if (m(PUNCTUATION("{"), root) && statementWrapper(root) && m(PUNCTUATION("}"), root)) {
          println("statBlock -> { statementWrapper }")
        } else {
          error = true
        }
      case RESERVED("float") | RESERVED("for") | RESERVED("if") | RESERVED("read")
           | RESERVED("return") | RESERVED("write") | RESERVED("integer") | ID(_)
           | PUNCTUATION("{") =>
        if (statement(root)) {
          println("statBlock -> statement")
        } else {
          error = true
        }
      case PUNCTUATION(";") | RESERVED("else") =>
        println("statBlock -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def relExpr(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("relExpr")
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | INTEGER(_) | OPERATOR("!") | ID(_) =>
        if (arithExpr(root) && relOp(root) && arithExpr(root)) {
          println("relExpr -> arithExpr relOp arithExpr")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def arithExpr(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("arithExpr")
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | INTEGER(_) | OPERATOR("!") | ID(_) =>
        if (term(root) && arithExprPrime(root)) {
          println("arithExpr -> term arithExprPrime")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def arithExprPrime(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("arithExprPrime")
    lookahead match {
      case OPERATOR("+") | OPERATOR("-") | OPERATOR("||") =>
        if (addOp(root) && term(root) && arithExprPrime(root)) {
          println("arithExprPrime -> addOp term arithExprPrime")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | PUNCTUATION(",") | PUNCTUATION(";") |
          PUNCTUATION("]") | PUNCTUATION("=") | FLOAT(_) | INTEGER(_) | OPERATOR(">=") |
          OPERATOR("<=") | OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") |
          OPERATOR("!") =>
        println("arithExprPrime -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def variableIdnestWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("variableIdnestWrapper")
    lookahead match {
      case ID(_) =>
        if(variableIdnest(root) && variableIdnestWrapper(root)){
          println("variableIdnestWrapper -> variableIdnest variableIdnestWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") | PUNCTUATION("=") =>
        println("variableIdnestWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def variableIdnest(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("variableIdnest")
    lookahead match {
      case ID(_) =>
        if(m(ID(""), root) && variableIdnest(root)){
          println("variableIdnest -> id variableIdnest")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def relOp(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("relOp")
    lookahead match {
      case PUNCTUATION("==") =>
        if(m(PUNCTUATION("="), root)){
          println("relOp -> eq")
        } else {
          error = true
        }
      case PUNCTUATION("<>") =>
        if(m(OPERATOR("!="), root)){
          println("relOp -> neq")
        } else {
          error = true
        }
      case OPERATOR("<") =>
        if(m(OPERATOR("<"), root)){
          println("relOp -> lt")
        } else {
          error = true
        }
      case OPERATOR(">") =>
        if(m(OPERATOR(">"), root)){
          println("relOp -> gt")
        } else {
          error = true
        }
      case PUNCTUATION("<=") =>
        if(m(PUNCTUATION("<="), root)){
          println("relOp -> leq")
        } else {
          error = true
        }
      case PUNCTUATION(">=") =>
        if(m(PUNCTUATION(">="), root)){
          println("relOp -> geq")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def addOp(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("addOp")
    lookahead match {
      case OPERATOR("+") =>
        if (m(OPERATOR("+"), root)) {
          println("addOp -> +")
        } else {
          error = true
        }
      case OPERATOR("-") =>
        if (m(OPERATOR("-"), root)) {
          println("addOp -> -")
        } else {
          error = true
        }
      case OPERATOR("||") =>
        if (m(OPERATOR("||"), root)) {
          println("addOp -> or")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def multOp(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("multOp")
    lookahead match {
      case OPERATOR("*") =>
        if (m(OPERATOR("*"), root)) {
          println("multOp -> *")
        } else {
          error = true
        }
      case OPERATOR("/") =>
        if (m(OPERATOR("/"), root)) {
          println("multOp -> /")
        } else {
          error = true
        }
      case OPERATOR("&&") =>
        if (m(OPERATOR("&&"), root)) {
          println("multOp -> and")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def assignOp(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("assignOp")
    lookahead match {
      case PUNCTUATION("=") =>
        if (m(PUNCTUATION("="), root)) {
          println("assignOp -> =")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def term(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("term")
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | ID(_) | INTEGER(_) | OPERATOR("!") =>
        if (factor(root) && termPrime(root)) {
          println("term -> factor termPrime")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def termPrime(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("termPrime")
    lookahead match {
      case OPERATOR("*") | OPERATOR("/") | OPERATOR("&&") =>
        if (multOp(root) && factor(root) && termPrime(root)) {
          println("termPrime -> multOp factor termPrime")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | PUNCTUATION(",") |
           PUNCTUATION(";") | OPERATOR("-") | OPERATOR("]") | PUNCTUATION("=") | FLOAT(_) |
           INTEGER(_) | OPERATOR(">=") | OPERATOR("||") | OPERATOR("<=") |
           OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") | OPERATOR("!") =>
        println("termPrime -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def variableAndFunctionCall(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("variableAndFunctionCall")
    lookahead match {
      case ID(_) =>
        if (m(ID(""), root) && T2(root)) {
          println("variableAndFunctionCall -> id T2")
        } else {
          error = true
        }
    }
    parent.addChild(root)
    !error
  }

  private def T2(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("T2")
    lookahead match {
      case PUNCTUATION("[") =>
        if (indice(root) && indiceWrapper(root) && T3(root)) {
          println("T2 -> indice indiceWrapper T3")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) && T3(root)) {
          println("T2 -> ( aParams ) T3")
        } else {
          error = true
        }
      case ID(_) =>
        if (variableAndFunctionCall(root)) {
          println("T2 -> variableAndfunctionCall")
        } else {
          error = true
        }
    }
    parent.addChild(root)
    !error
  }

  private def T3(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("T3")
    lookahead match {
      case PUNCTUATION(".") =>
        if (m(PUNCTUATION("."), root)) {
          println("T3 -> .")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | OPERATOR("*") |
           PUNCTUATION(",") | OPERATOR("-") | PUNCTUATION(";") | OPERATOR("]") |
           PUNCTUATION("=") | FLOAT(_) | OPERATOR("&&") |
           INTEGER(_) | OPERATOR(">=") | OPERATOR("||") | OPERATOR("<=") |
           OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") | OPERATOR("!") =>
        println("T3 -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def factor(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("factor")
    lookahead match {
      case ID(_) =>
        if (variableAndFunctionCall(root)) {
          println("factor -> variableAndfunctionCall")
        } else {
          error = true
        }
      case INTEGER(_) =>
        if (m(INTEGER(""), root)) {
          println("factor -> intNum")
        } else {
          error = true
        }
      case FLOAT(_) =>
        if (m(FLOAT(""), root)) {
          println("factor -> floatNum")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && arithExpr(root) && m(PUNCTUATION(")"), root)) {
          println("factor -> ( arithExpr )")
        } else {
          error = true
        }
      case PUNCTUATION("!") =>
        if (m(PUNCTUATION("!"), root) && factor(root)) {
          println("factor -> not factor")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def idnestWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idnestWrapper")
    lookahead match {
      case ID(_) =>
        if (idnest(root) && idnestWrapper(root)) {
          println("idnestWrapper -> idnest idnestWrapper")
        } else {
          error = true
        }
      case _ =>
        println("idnestWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
    }
    parent.addChild(root)
    !error
  }

  private def idnest(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idnest")
    lookahead match {
      case ID(_) =>
        if (m(ID(""), root) && idnestExt(root)) {
          println("idnest -> id idnestExt")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def idnestExt(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idnestExt")
    lookahead match {
      case PUNCTUATION(".") | PUNCTUATION("[") =>
        if (indiceWrapper(root) && m(PUNCTUATION("."), root)) {
          println("idnestExt -> indiceWrapper .")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) && m(PUNCTUATION("."), root)) {
          println("idnestExt -> ( aParams ) .")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def indiceWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("indiceWrapper")
    lookahead match {
      case PUNCTUATION("[") =>
        if (indice(root) && indiceWrapper(root)) {
          println("indiceWrapper -> indice indiceWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | OPERATOR("*") |
           PUNCTUATION(",") | OPERATOR("-") | PUNCTUATION(";") | OPERATOR("]") |
           PUNCTUATION("=") | FLOAT(_) | OPERATOR("&&") |
           INTEGER(_) | OPERATOR(">=") | OPERATOR("||") | OPERATOR("<=") |
           OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") | OPERATOR("!") =>
        println("indiceWrapper -> EPSILON")
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def indice(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("indice")
    lookahead match {
      case PUNCTUATION("[") =>
        if (m(PUNCTUATION("["), root) && arithExpr(root) && m(PUNCTUATION("]"), root)) {
          println("indiceWrapper -> [ arithExpr ]")
        } else {
          error = true
        }
    }
    parent.addChild(root)
    !error
  }
}
