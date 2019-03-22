package parser

import tokenizer.Token
import tokenizer.Token._

object Parser {

  var lookahead: Token = OPERATOR("", Location(0,0))
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
    var result = lookahead.getClass == t.getClass && lookahead.value == t.value
    var metadata: Option[String] = None

    t match {
      case ID(_, _) =>
        lookahead match {
          case ID(_, _) =>
            metadata = Some("ID")
            result = true
          case _ =>
        }
      case INTEGER(_, _) =>
        lookahead match {
          case INTEGER(_, _) =>
            metadata = Some("INTEGER")
            result = true
          case _ =>
        }
      case FLOAT(_, _) =>
        lookahead match {
          case FLOAT(_, _) =>
            metadata = Some("FLOAT")
            result = true
          case _ =>
        }
      case _ =>
    }

    if(result){
      parent.addChild(new ASTNode(lookahead.value, lookahead.location, metadata))
      if(tokenIterator.hasNext) {
        lookahead = tokenIterator.next()
      } else {
        lookahead = TERMINATION("", Location(0,0))
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
    val root = new ASTNode("classDeclWrapper", loc = lookahead.location)
    lookahead match {
      case RESERVED("class", _) =>
        if (classDecl(root) && classDeclWrapper(root)) {
        } else {
          error = true
        }
      case RESERVED("float", _) | RESERVED("integer", _) | ID(_, _) | RESERVED("main", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def classDecl(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("classDecl", loc = lookahead.location)
    lookahead match {
      case RESERVED("class", _) =>
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
    val root = new ASTNode("funcDefWrapper", loc = lookahead.location)
    lookahead match {
      case RESERVED("float", _) | RESERVED("integer", _) | ID(_, _) =>
        if (funcDef(root) && funcDefWrapper(root)) {
        } else {
          error = true
        }
      case RESERVED("main", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def funcDef(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("funcDef", loc = lookahead.location)
    lookahead match {
      case RESERVED("float", _) | RESERVED("integer", _) | ID(_, _) =>
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
    val root = new ASTNode("funcBody", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("{", _) =>
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
    val root = new ASTNode("optionalIDExt", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(":", _) =>
        if (m(PUNCTUATION(":"), root) && m(ID(""), root) && idWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("{", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def idWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idWrapper", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(",", _) =>
        if (idEntity(root) && idWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("{", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def idEntity(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idEntity", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(",", _) =>
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
    val root = new ASTNode("genericDeclWrapper", loc = lookahead.location)
    lookahead match {
      case RESERVED("integer", _) | ID(_, _) | RESERVED("float", _) =>
        if (genericDecl(root) && genericDeclWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("}", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def genericDecl(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("genericDecl", loc = lookahead.location)
    lookahead match {
      case RESERVED("integer", _) | ID(_, _) | RESERVED("float", _) =>
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
    val root = new ASTNode("genericExtension", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && fParams(root) && m(PUNCTUATION(")"), root)) {
        } else {
          error = true
        }
      case PUNCTUATION(";", _) | PUNCTUATION("=", _) | PUNCTUATION("[", _) =>
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
    val root = new ASTNode("funcHead", loc = lookahead.location)
    lookahead match {
      case RESERVED("float", _) | RESERVED("integer", _) | ID(_, _) =>
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
    val root = new ASTNode("optionalIDSR", loc = lookahead.location)
    lookahead match {
      case ID(_, _) =>
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
    val root = new ASTNode("optionalIDSRExt", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("::", _) =>
        if (m(PUNCTUATION("::"), root) && m(ID(""), root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def `type`(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("type", loc = lookahead.location)
    lookahead match {
      case RESERVED("float", _) =>
        if (m(RESERVED("float"), root)) {
        } else {
          error = true
        }
      case RESERVED("integer", _) =>
        if (m(RESERVED("integer"), root)) {
        } else {
          error = true
        }
      case ID(_, _) =>
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
    val root = new ASTNode("fParams", loc = lookahead.location)
    lookahead match {
      case RESERVED("float", _) | ID(_, _) | RESERVED("integer", _) =>
        if (`type`(root) && m(ID(""), root) && arraySizeWrapper(root) && fParamsTailWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(")", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def fParamsTailWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("fParamsTailWrapper", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(",", _) =>
        if (fParamsTail(root) && fParamsTailWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(")", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def fParamsTail(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("fParamsTail", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(",", _) =>
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
    val root = new ASTNode("aParams", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("(", _) | FLOAT(_, _) | ID(_, _) | INTEGER(_, _) | OPERATOR("!", _) =>
        if (expr(root) && aParamsTailWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(")", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def aParamsTailWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("aParamsTailWrapper", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(",", _) =>
        if (aParamsTail(root) && aParamsTailWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(")", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def aParamsTail(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("aParamsTail", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(",", _) =>
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
    val root = new ASTNode("statementWrapper", loc = lookahead.location)
    lookahead match {
      case RESERVED("float", _) | RESERVED("for", _) | RESERVED("if", _) | RESERVED("read", _)
        | RESERVED("return", _) | RESERVED("write", _) | RESERVED("integer", _) | ID(_, _) =>
        if (statement(root) && statementWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("}", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def assignStatAndVar(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("assignStatAndVar", loc = lookahead.location)
    lookahead match {
      case RESERVED("integer", _) =>
        if (m(RESERVED("integer"), root) && m(ID(""), root) && genericExtension(root) && optionalAssignOp(root)) {
        } else {
          error = true
        }
      case RESERVED("float", _) =>
        if (m(RESERVED("float"), root) && m(ID(""), root) && genericExtension(root) && optionalAssignOp(root)) {
        } else {
          error = true
        }
      case ID(_, _) =>
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
    val root = new ASTNode("optionalAssignOp", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("=", _) =>
        if (assignOp(root) && expr(root) && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case PUNCTUATION(";", _) =>
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
    val root = new ASTNode("statOrVarExt", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(".", _) | PUNCTUATION("[", _) =>
        if (indiceWrapper(root) && indiceExtInt(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) &&
          m(PUNCTUATION("."), root) && variableIdnestWrapper(root)) {
        } else {
          error = true
        }
      case ID(_, _) =>
        if (m(ID(""), root) && genericExtension(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("=", _) | PUNCTUATION(";", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def indiceExtInt(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("indiceExtInt", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(".", _) =>
        if (m(PUNCTUATION("."), root) && variableIdnestWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("}", _) | PUNCTUATION("=", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def statement(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("statement", loc = lookahead.location)
    lookahead match {
      case RESERVED("float", _) | ID(_, _) | RESERVED("integer", _) =>
        if (assignStatAndVar(root)) {
        } else {
          error = true
        }
      case RESERVED("if", _) =>
        if(m(RESERVED("if"), root) && m(PUNCTUATION("("), root) && expr(root) && m(PUNCTUATION(")"), root)
          && m(RESERVED("then"), root) && statBlock(root) && m(RESERVED("else"), root) && statBlock(root)
          && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case RESERVED("for", _) =>
        if(m(RESERVED("for"), root) && m(PUNCTUATION("("), root) && `type`(root) && m(ID(""), root) &&
          assignOp(root) && expr(root) && m(PUNCTUATION(";"), root) && relExpr(root) && m(PUNCTUATION(";"), root)
          && assignStat(root) && m(PUNCTUATION(")"), root) && statBlock(root) && m(PUNCTUATION(";"), root)){
        } else {
          error = true
        }
      case RESERVED("read", _) =>
        if(m(RESERVED("read"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case RESERVED("write", _) =>
        if(m(RESERVED("write"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
        } else {
          error = true
        }
      case RESERVED("return", _) =>
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
    val root = new ASTNode("arraySizeWrapper", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("[", _) =>
        if (arraySize(root) && arraySizeWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(")", _) | PUNCTUATION(",", _) | PUNCTUATION(";", _) | PUNCTUATION("=", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def arraySize(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("arraySize", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("[", _) =>
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
    val root = new ASTNode("assignStat", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("=", _) | ID(_, _) =>
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
    val root = new ASTNode("expr", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("(", _) | OPERATOR("+", _) | OPERATOR("-", _) | OPERATOR("!", _) |
           FLOAT(_, _) | INTEGER(_, _) | ID(_, _) =>
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
    val root = new ASTNode("exprExt", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("=", _) | PUNCTUATION(">", _) | PUNCTUATION(">=", _) |
           PUNCTUATION("<=", _) | PUNCTUATION("<", _) | PUNCTUATION("<>", _) =>
        if (relOp(root) && arithExpr(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | FLOAT(_, _) | INTEGER(_, _) | OPERATOR("!", _) =>
        if (term(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(",", _) | PUNCTUATION(")", _) | PUNCTUATION(";", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def statBlock(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("statBlock", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("{", _) =>
        if (m(PUNCTUATION("{"), root) && statementWrapper(root) && m(PUNCTUATION("}"), root)) {
        } else {
          error = true
        }
      case RESERVED("float", _) | RESERVED("for", _) | RESERVED("if", _) | RESERVED("read", _)
           | RESERVED("return", _) | RESERVED("write", _) | RESERVED("integer", _) | ID(_, _)
           | PUNCTUATION("{", _) =>
        if (statement(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(";", _) | RESERVED("else", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def relExpr(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("relExpr", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("(", _) | FLOAT(_, _) | INTEGER(_, _) | OPERATOR("!", _) | ID(_, _) =>
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
    val root = new ASTNode("arithExpr", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("(", _) | FLOAT(_, _) | INTEGER(_, _) | OPERATOR("!", _) | ID(_, _) =>
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
    val root = new ASTNode("arithExprPrime", loc = lookahead.location)
    lookahead match {
      case OPERATOR("+", _) | OPERATOR("-", _) | OPERATOR("||", _) =>
        if (addOp(root) && term(root) && arithExprPrime(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | PUNCTUATION(",", _) | PUNCTUATION(";", _) |
          PUNCTUATION("]", _) | PUNCTUATION("=", _) | FLOAT(_, _) | INTEGER(_, _) | PUNCTUATION(">=", _) |
          PUNCTUATION("<=", _) | PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) |
          OPERATOR("!", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def variableIdnestWrapper(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("variableIdnestWrapper", loc = lookahead.location)
    lookahead match {
      case ID(_, _) =>
        if(variableIdnest(root) && variableIdnestWrapper(root)){
        } else {
          error = true
        }
      case PUNCTUATION(")", _) | PUNCTUATION("=", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def variableIdnest(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("variableIdnest", loc = lookahead.location)
    lookahead match {
      case ID(_, _) =>
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
    val root = new ASTNode("relOp", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("==", _) =>
        if(m(PUNCTUATION("=="), root)){
        } else {
          error = true
        }
      case PUNCTUATION("<>", _) =>
        if(m(PUNCTUATION("<>"), root)){
        } else {
          error = true
        }
      case PUNCTUATION("<", _) =>
        if(m(PUNCTUATION("<"), root)){
        } else {
          error = true
        }
      case PUNCTUATION(">", _) =>
        if(m(PUNCTUATION(">"), root)){
        } else {
          error = true
        }
      case PUNCTUATION("<=", _) =>
        if(m(PUNCTUATION("<="), root)){
        } else {
          error = true
        }
      case PUNCTUATION(">=", _) =>
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
    val root = new ASTNode("addOp", loc = lookahead.location)
    lookahead match {
      case OPERATOR("+", _) =>
        if (m(OPERATOR("+"), root)) {
        } else {
          error = true
        }
      case OPERATOR("-", _) =>
        if (m(OPERATOR("-"), root)) {
        } else {
          error = true
        }
      case OPERATOR("||", _) =>
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
    val root = new ASTNode("multOp", loc = lookahead.location)
    lookahead match {
      case OPERATOR("*", _) =>
        if (m(OPERATOR("*"), root)) {
        } else {
          error = true
        }
      case OPERATOR("/", _) =>
        if (m(OPERATOR("/"), root)) {
        } else {
          error = true
        }
      case OPERATOR("&&", _) =>
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
    val root = new ASTNode("assignOp", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("=", _) =>
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
    val root = new ASTNode("term", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("(", _) | FLOAT(_, _) | ID(_, _) | INTEGER(_, _) | OPERATOR("!", _) =>
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
    val root = new ASTNode("termPrime", loc = lookahead.location)
    lookahead match {
      case OPERATOR("*", _) | OPERATOR("/", _) | OPERATOR("&&", _) =>
        if (multOp(root) && factor(root) && termPrime(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | OPERATOR("+", _) | PUNCTUATION(",", _) |
           PUNCTUATION(";", _) | OPERATOR("-", _) | PUNCTUATION("]", _) | PUNCTUATION("=", _) | FLOAT(_, _) |
           INTEGER(_, _) | PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) |
           PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) | OPERATOR("!", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def variableAndFunctionCall(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("variableAndFunctionCall", loc = lookahead.location)
    lookahead match {
      case ID(_, _) =>
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
    val root = new ASTNode("T2", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("[", _) =>
        if (indice(root) && indiceWrapper(root) && T3(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) && T3(root)) {
        } else {
          error = true
        }
      case ID(_, _) =>
        if (variableAndFunctionCall(root)) {
        } else {
          error = true
        }
      case PUNCTUATION(")", _) | OPERATOR("+", _) | OPERATOR("*", _) | PUNCTUATION(",", _) | OPERATOR("-", _) |
           OPERATOR("/", _) | PUNCTUATION(";", _) | PUNCTUATION("]", _) | OPERATOR("&&", _) | PUNCTUATION("==", _) |
           PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) | PUNCTUATION("<", _) |
           PUNCTUATION(">", _) | PUNCTUATION("<>", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def T3(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("T3", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(".", _) =>
        if (m(PUNCTUATION("."), root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | OPERATOR("+", _) | OPERATOR("*", _) |
           PUNCTUATION(",", _) | OPERATOR("-", _) | PUNCTUATION(";", _) | PUNCTUATION("]", _) | PUNCTUATION("[", _) |
           PUNCTUATION("=", _) | OPERATOR("&&", _) | OPERATOR("/", _) |
           PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) |
           PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def sign(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("sign", loc = lookahead.location)
    lookahead match {
      case OPERATOR("+", _) =>
        if (m(OPERATOR("+"), root)) {
        } else {
          error = true
        }
      case OPERATOR("-", _) =>
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
    val root = new ASTNode("factor", loc = lookahead.location)
    lookahead match {
      case ID(_, _) =>
        if (variableAndFunctionCall(root)) {
        } else {
          error = true
        }
      case INTEGER(_, _) =>
        if (m(INTEGER(""), root)) {
        } else {
          error = true
        }
      case FLOAT(_, _) =>
        if (m(FLOAT(""), root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && arithExpr(root) && m(PUNCTUATION(")"), root)) {
        } else {
          error = true
        }
      case OPERATOR("!", _) =>
        if (m(OPERATOR("!"), root) && factor(root)) {
        } else {
          error = true
        }
      case OPERATOR("+", _) | OPERATOR("-", _) =>
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
    val root = new ASTNode("idnestWrapper", loc = lookahead.location)
    lookahead match {
      case ID(_, _) =>
        if (idnest(root) && idnestWrapper(root)) {
        } else {
          error = true
        }
      case _ =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
    }
    parent.addChild(root)
    !error
  }

  private def idnest(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("idnest", loc = lookahead.location)
    lookahead match {
      case ID(_, _) =>
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
    val root = new ASTNode("idnestExt", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(".", _) | PUNCTUATION("[", _) =>
        if (indiceWrapper(root) && m(PUNCTUATION("."), root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
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
    val root = new ASTNode("indiceWrapper", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("[", _) =>
        if (indice(root) && indiceWrapper(root)) {
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | OPERATOR("+", _) | OPERATOR("*", _) |
           PUNCTUATION(",", _) | OPERATOR("-", _) | PUNCTUATION(";", _) | PUNCTUATION("]", _) |
           PUNCTUATION("=", _) | FLOAT(_, _) | OPERATOR("&&", _) | PUNCTUATION(".", _) |
           INTEGER(_, _) | PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) |
           PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) | OPERATOR("!", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def indice(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("indice", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION("[", _) =>
        if (m(PUNCTUATION("["), root) && arithExpr(root) && m(PUNCTUATION("]"), root)) {
        } else {
          error = true
        }
    }
    parent.addChild(root)
    !error
  }
}
