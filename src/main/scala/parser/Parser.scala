package parser

import tokenizer.Token
import tokenizer.Token._

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
    var metadata: Option[String] = None

    t match {
      case ID(_) =>
        lookahead match {
          case ID(_) =>
            metadata = Some("ID")
            result = true
          case _ =>
        }
      case INTEGER(_) =>
        lookahead match {
          case INTEGER(_) =>
            metadata = Some("INTEGER")
            result = true
          case _ =>
        }
      case FLOAT(_) =>
        lookahead match {
          case FLOAT(_) =>
            metadata = Some("FLOAT")
            result = true
          case _ =>
        }
      case _ =>
    }

    if(result){
      parent.addChild(new ASTNode(lookahead.value, metadata))
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
        } else {
          error = true
        }
      case RESERVED("float") | RESERVED("integer") | ID(_) | RESERVED("main") =>
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
        } else {
          error = true
        }
      case RESERVED("main") =>
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
        } else {
          error = true
        }
      case PUNCTUATION("{") =>
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
        } else {
          error = true
        }
      case PUNCTUATION("{") =>
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
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
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
        } else {
          error = true
        }
      case PUNCTUATION(";") | PUNCTUATION("=") | PUNCTUATION("[") =>
        if (arraySizeWrapper(root)) {
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
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
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
        } else {
          error = true
        }
      case RESERVED("integer") =>
        if (m(RESERVED("integer"), root)) {
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID(""), root)) {
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
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
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
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
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
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
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
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
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
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
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
        if (m(RESERVED("integer"), root) && m(ID(""), root) && genericExtension(root) && optionalAssignOp(root)) {
        } else {
          error = true
        }
      case RESERVED("float") =>
        if (m(RESERVED("float"), root) && m(ID(""), root) && genericExtension(root) && optionalAssignOp(root)) {
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID(""), root) && statOrVarExt(root) && optionalAssignOp(root)) {
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def optionalAssignOp(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("optionalAssignOp")
    lookahead match {
      case PUNCTUATION("=") =>
        if (assignOp(root) && expr(root) && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case PUNCTUATION(";") =>
        if (m(PUNCTUATION(";"), root)) {
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
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) &&
          m(PUNCTUATION("."), root) && variableIdnestWrapper(root)) {
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID(""), root) && genericExtension(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("=") | PUNCTUATION(";") =>
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
        } else {
          error = true
        }
      case PUNCTUATION("}") | PUNCTUATION("=") =>
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
        } else {
          error = true
        }
      case RESERVED("if") =>
        if(m(RESERVED("if"), root) && m(PUNCTUATION("("), root) && expr(root) && m(PUNCTUATION(")"), root)
          && m(RESERVED("then"), root) && statBlock(root) && m(RESERVED("else"), root) && statBlock(root)
          && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case RESERVED("for") =>
        if(m(RESERVED("for"), root) && m(PUNCTUATION("("), root) && `type`(root) && m(ID(""), root) &&
          assignOp(root) && expr(root) && m(PUNCTUATION(";"), root) && relExpr(root) && m(PUNCTUATION(";"), root)
          && assignStat(root) && m(PUNCTUATION(")"), root) && statBlock(root) && m(PUNCTUATION(";"), root)){
        } else {
          error = true
        }
      case RESERVED("read") =>
        if(m(RESERVED("read"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case RESERVED("write") =>
        if(m(RESERVED("write"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case RESERVED("return") =>
        if(m(RESERVED("return"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
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
        } else {
          error = true
        }
      case PUNCTUATION(")") | PUNCTUATION(",") | PUNCTUATION(";") | PUNCTUATION("=") =>
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
      case PUNCTUATION("(") | OPERATOR("+") | OPERATOR("-") | OPERATOR("!") | FLOAT(_) | INTEGER(_) | ID(_) =>
        if (arithExpr(root) && exprExt(root)) {
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
      case PUNCTUATION("=") | PUNCTUATION(">") | PUNCTUATION(">=") |
           PUNCTUATION("<=") | PUNCTUATION("<") | PUNCTUATION("<>") =>
        if (relOp(root) && arithExpr(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(") | FLOAT(_) | INTEGER(_) | OPERATOR("!") =>
        if (term(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(",") | PUNCTUATION(")") | PUNCTUATION(";") =>
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
        } else {
          error = true
        }
      case RESERVED("float") | RESERVED("for") | RESERVED("if") | RESERVED("read")
           | RESERVED("return") | RESERVED("write") | RESERVED("integer") | ID(_)
           | PUNCTUATION("{") =>
        if (statement(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(";") | RESERVED("else") =>
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
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | PUNCTUATION(",") | PUNCTUATION(";") |
          PUNCTUATION("]") | PUNCTUATION("=") | FLOAT(_) | INTEGER(_) | PUNCTUATION(">=") |
          PUNCTUATION("<=") | PUNCTUATION("<") | PUNCTUATION(">") | ID(_) | PUNCTUATION("<>") |
          OPERATOR("!") =>
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
        } else {
          error = true
        }
      case PUNCTUATION(")") | PUNCTUATION("=") =>
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
        if(m(ID(""), root) && indiceWrapper(root)){
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
        if(m(PUNCTUATION("=="), root)){
        } else {
          error = true
        }
      case PUNCTUATION("<>") =>
        if(m(PUNCTUATION("<>"), root)){
        } else {
          error = true
        }
      case PUNCTUATION("<") =>
        if(m(PUNCTUATION("<"), root)){
        } else {
          error = true
        }
      case PUNCTUATION(">") =>
        if(m(PUNCTUATION(">"), root)){
        } else {
          error = true
        }
      case PUNCTUATION("<=") =>
        if(m(PUNCTUATION("<="), root)){
        } else {
          error = true
        }
      case PUNCTUATION(">=") =>
        if(m(PUNCTUATION(">="), root)){
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
        } else {
          error = true
        }
      case OPERATOR("-") =>
        if (m(OPERATOR("-"), root)) {
        } else {
          error = true
        }
      case OPERATOR("||") =>
        if (m(OPERATOR("||"), root)) {
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
        } else {
          error = true
        }
      case OPERATOR("/") =>
        if (m(OPERATOR("/"), root)) {
        } else {
          error = true
        }
      case OPERATOR("&&") =>
        if (m(OPERATOR("&&"), root)) {
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
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | PUNCTUATION(",") |
           PUNCTUATION(";") | OPERATOR("-") | PUNCTUATION("]") | PUNCTUATION("=") | FLOAT(_) |
           INTEGER(_) | PUNCTUATION(">=") | OPERATOR("||") | PUNCTUATION("<=") |
           PUNCTUATION("<") | PUNCTUATION(">") | ID(_) | PUNCTUATION("<>") | OPERATOR("!") =>
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
        if (m(ID(""), root) && T3(root) && T2(root)) {
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
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) && T3(root)) {
        } else {
          error = true
        }
      case ID(_) =>
        if (variableAndFunctionCall(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(")") | OPERATOR("+") | OPERATOR("*") | PUNCTUATION(",") | OPERATOR("-") |
           OPERATOR("/") | PUNCTUATION(";") | PUNCTUATION("]") | OPERATOR("&&") | PUNCTUATION("==") |
           PUNCTUATION(">=") | OPERATOR("||") | PUNCTUATION("<=") | PUNCTUATION("<") |
           PUNCTUATION(">") | PUNCTUATION("<>") =>
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
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
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | OPERATOR("*") |
           PUNCTUATION(",") | OPERATOR("-") | PUNCTUATION(";") | PUNCTUATION("]") | PUNCTUATION("[") |
           PUNCTUATION("=") | OPERATOR("&&") | OPERATOR("/") |
           PUNCTUATION(">=") | OPERATOR("||") | PUNCTUATION("<=") |
           PUNCTUATION("<") | PUNCTUATION(">") | ID(_) | PUNCTUATION("<>") =>
        root.addChild(new ASTNode("EPSILON"))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def sign(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("sign")
    lookahead match {
      case OPERATOR("+") =>
        if (m(OPERATOR("+"), root)) {
        } else {
          error = true
        }
      case OPERATOR("-") =>
        if (m(OPERATOR("-"), root)) {
        } else {
          error = true
        }
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
        } else {
          error = true
        }
      case INTEGER(_) =>
        if (m(INTEGER(""), root)) {
        } else {
          error = true
        }
      case FLOAT(_) =>
        if (m(FLOAT(""), root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && arithExpr(root) && m(PUNCTUATION(")"), root)) {
        } else {
          error = true
        }
      case OPERATOR("!") =>
        if (m(OPERATOR("!"), root) && factor(root)) {
        } else {
          error = true
        }
      case OPERATOR("+") | OPERATOR("-") =>
        if (sign(root) && factor(root)) {
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
        } else {
          error = true
        }
      case _ =>
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
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) && m(PUNCTUATION("."), root)) {
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
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | OPERATOR("*") |
           PUNCTUATION(",") | OPERATOR("-") | PUNCTUATION(";") | PUNCTUATION("]") |
           PUNCTUATION("=") | FLOAT(_) | OPERATOR("&&") | PUNCTUATION(".") |
           INTEGER(_) | PUNCTUATION(">=") | OPERATOR("||") | PUNCTUATION("<=") |
           PUNCTUATION("<") | PUNCTUATION(">") | ID(_) | PUNCTUATION("<>") | OPERATOR("!") =>
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
        } else {
          error = true
        }
    }
    parent.addChild(root)
    !error
  }
}
