package parser

import java.io.{File, PrintWriter}

import tokenizer.Token
import tokenizer.Token._

object Parser {

  var lookahead: Token = OPERATOR("", Location(0,0))
  var tokenIterator: Iterator[Token] = Iterator()
  var root: ASTNode = _
  var writer: PrintWriter = _

  case class ParseResult(tree: ASTNode, errors: Seq[String])

  def parse(tokens: Seq[Token], programName: String): ParseResult = {
    root = new ASTNode("prog")
    tokenIterator = tokens.iterator
    lookahead = tokenIterator.next()

    writer = new PrintWriter(new File(s"output/$programName-productions.txt"))

    if(prog(root)) {
      writer.close()
      ParseResult(root, Seq.empty)
    }else{
      writer.close()
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
    }else{
      println(s"[error] syntax error expected ${t.getClass.getSimpleName} got ${lookahead.value} instead " +
        s"@ L${lookahead.location.row}:${lookahead.location.col}")
    }
    result
  }

  private def prog(parent: ASTNode): Boolean = {
    var error = false
    if(classDeclWrapper(parent) && funcDefWrapper(parent) && m(RESERVED("main"), parent)
      && funcBody(parent) && m(PUNCTUATION(";"), parent) && m(TERMINATION(""), parent)) {
      writer.write("prog -> classDeclWrapper funcDefWrapper main funcBody ;\n")
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
          writer.write("classDeclWrapper -> classDecl classDeclWrapper\n")
        } else {
          error = true
        }
      case RESERVED("float", _) | RESERVED("integer", _) | ID(_, _) | RESERVED("main", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("classDeclWrapper -> EPSILON\n")
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
          writer.write("classDecl -> class id optionalIDExt { genericDeclWrapper } ;\n")
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
          writer.write("funcDefWrapper -> funcDef funcDefWrapper\n")
        } else {
          error = true
        }
      case RESERVED("main", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("funcDefWrapper -> EPSILON\n")
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
          writer.write("funcDef -> funcHead funcBody ;\n")
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
          writer.write("funcBody -> { statementWrapper }\n")
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
          writer.write("optionalIDExt -> : id idWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION("{", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("optionalIDExt -> EPSILON\n")
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
          writer.write("idWrapper -> idEntity idWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION("{", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("idWrapper -> EPSILON\n")
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
          writer.write("idEntity -> , id\n")
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
          writer.write("genericDeclWrapper -> genericDecl genericDeclWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION("}", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("genericDeclWrapper -> EPSILON\n")
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
          writer.write("genericDecl -> type id genericExtension ;\n")
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
          writer.write("genericExtension -> ( fParams )\n")
        } else {
          error = true
        }
      case PUNCTUATION(";", _) | PUNCTUATION("=", _) | PUNCTUATION("[", _) =>
        if (arraySizeWrapper(root)) {
          writer.write("genericExtension -> arraySizeWrapper\n")
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
          writer.write("funcHead -> type optionalIDSR ( fParams )\n")
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
          writer.write("optionalIDSR -> id optionalIDSRExt\n")
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
          writer.write("optionalIDSRExt -> :: id\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("optionalIDSRExt -> EPSILON\n")
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
          writer.write("type -> float\n")
        } else {
          error = true
        }
      case RESERVED("integer", _) =>
        if (m(RESERVED("integer"), root)) {
          writer.write("type -> integer\n")
        } else {
          error = true
        }
      case ID(_, _) =>
        if (m(ID(""), root)) {
          writer.write("type -> id\n")
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
          writer.write("fParamsTailWrapper -> fParamsTail fParamsTailWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION(")", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("fParamsTailWrapper -> EPSILON\n")
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
          writer.write("fParamsTail -> , type id arraySizeWrapper\n")
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
          writer.write("aParams -> expr aParamsTailWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION(")", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("aParams -> EPSILON\n")
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
          writer.write("aParamsTailWrapper -> aParamsTail aParamsTailWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION(")", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("aParamsTailWrapper -> EPSILON\n")
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
          writer.write("aParamsTail -> , expr\n")
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
          writer.write("statementWrapper -> statement statementWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION("}", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("statementWrapper -> EPSILON\n")
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
          writer.write("assignStatAndVar -> integer id genericExtension optionalAssignOp\n")
        } else {
          error = true
        }
      case RESERVED("float", _) =>
        if (m(RESERVED("float"), root) && m(ID(""), root) && genericExtension(root) && optionalAssignOp(root)) {
          writer.write("assignStatAndVar -> float id genericExtension optionalAssignOp\n")
        } else {
          error = true
        }
      case ID(_, _) =>
        if (m(ID(""), root) && statOrVarExt(root) && optionalAssignOp(root)) {
          writer.write("assignStatAndVar -> id statOrVarExt optionalAssignOp\n")
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
          writer.write("optionalAssignOp -> assignOp expr ;\n")
        } else {
          error = true
        }
      case PUNCTUATION(";", _) =>
        if (m(PUNCTUATION(";"), root)) {
          writer.write("optionalAssignOp -> ;\n")
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
          writer.write("statOrVarExt -> indiceWrapper indiceExtInt\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) &&
          optionalDotAndIdnest(root)) {
          writer.write("statOrVarExt -> ( aParams ) optionalDotAndIdnest\n")
        } else {
          error = true
        }
      case ID(_, _) =>
        if (m(ID(""), root) && genericExtension(root)) {
          writer.write("statOrVarExt -> id genericExtension\n")
        } else {
          error = true
        }
      case PUNCTUATION("=", _) | PUNCTUATION(";", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("statOrVarExt -> EPSILON\n")
      case _ =>
        error = true
    }
    parent.addChild(root)
    !error
  }

  private def optionalDotAndIdnest(parent: ASTNode): Boolean = {
    var error = false
    val root = new ASTNode("optionalDotAndIdnest", loc = lookahead.location)
    lookahead match {
      case PUNCTUATION(".", _) =>
        if (m(PUNCTUATION("."), root) && variableIdnestWrapper(root)) {
          writer.write("optionalDotAndIdnest -> . variableIdnestWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION(";", _) | PUNCTUATION("=", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("optionalDotAndIdnest -> EPSILON\n")
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
          writer.write("indiceExtInt -> . variableIdnestWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION("}", _) | PUNCTUATION("=", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("indiceExtInt -> EPSILON\n")
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
          writer.write("statement -> assignStatAndVar\n")
        } else {
          error = true
        }
      case RESERVED("if", _) =>
        if(m(RESERVED("if"), root) && m(PUNCTUATION("("), root) && expr(root) && m(PUNCTUATION(")"), root)
          && m(RESERVED("then"), root) && statBlock(root) && m(RESERVED("else"), root) && statBlock(root)
          && m(PUNCTUATION(";"), root)) {
          writer.write("statement -> if ( expr ) then statBlock else statBlock ;\n")
        } else {
          error = true
        }
      case RESERVED("for", _) =>
        if(m(RESERVED("for"), root) && m(PUNCTUATION("("), root) && `type`(root) && m(ID(""), root) &&
          assignOp(root) && expr(root) && m(PUNCTUATION(";"), root) && relExpr(root) && m(PUNCTUATION(";"), root)
          && assignStat(root) && m(PUNCTUATION(")"), root) && statBlock(root) && m(PUNCTUATION(";"), root)){
          writer.write("statement -> for ( type id assignOp expr ; relExpr ; assignStat ) statBlock ;\n")
        } else {
          error = true
        }
      case RESERVED("read", _) =>
        if(m(RESERVED("read"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
          writer.write("statement -> read ( expr ) ;\n")
        } else {
          error = true
        }
      case RESERVED("write", _) =>
        if(m(RESERVED("write"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
          writer.write("statement -> write ( expr ) ;\n")
        } else {
          error = true
        }
      case RESERVED("return", _) =>
        if(m(RESERVED("return"), root) && m(PUNCTUATION("("), root) && expr(root)
          && m(PUNCTUATION(")"), root) && m(PUNCTUATION(";"), root)) {
          writer.write("statement -> return ( expr ) ;\n")
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
          writer.write("arraySizeWrapper -> arraySize arraySizeWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION(")", _) | PUNCTUATION(",", _) | PUNCTUATION(";", _) | PUNCTUATION("=", _) =>
        root.addChild(new ASTNode("EPSILON", loc = lookahead.location))
        writer.write("arraySizeWrapper -> EPSILON\n")
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
          writer.write("arraySize -> [ integer ]\n")
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
          writer.write("assignStat -> variableIdnestWrapper assignOp expr\n")
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
          writer.write("expr -> arithExpr exprExt\n")
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
      case PUNCTUATION("==", _) | PUNCTUATION(">", _) | PUNCTUATION(">=", _) |
           PUNCTUATION("<=", _) | PUNCTUATION("<", _) | PUNCTUATION("<>", _) =>
        if (relOp(root) && arithExpr(root)) {
          writer.write("exprExt -> relOp arithExpr\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | FLOAT(_, _) | INTEGER(_, _) | OPERATOR("!", _) =>
        if (term(root)) {
          writer.write("exprExt -> term\n")
        } else {
          error = true
        }
      case PUNCTUATION(",", _) | PUNCTUATION(")", _) | PUNCTUATION(";", _) =>
        writer.write("exprExt -> EPSILON\n")
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
          writer.write("statBlock -> { statementWrapper }\n")
        } else {
          error = true
        }
      case RESERVED("float", _) | RESERVED("for", _) | RESERVED("if", _) | RESERVED("read", _)
           | RESERVED("return", _) | RESERVED("write", _) | RESERVED("integer", _) | ID(_, _)
           | PUNCTUATION("{", _) =>
        if (statement(root)) {
          writer.write("statBlock -> statement\n")
        } else {
          error = true
        }
      case PUNCTUATION(";", _) | RESERVED("else", _) =>
        writer.write("statBlock -> EPSILON\n")
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
          writer.write("relExpr -> arithExpr relOp arithExpr\n")
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
          writer.write("arithExpr -> term arithExprPrime\n")
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
          writer.write("arithExprPrime -> addOp term arithExprPrime\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | PUNCTUATION(",", _) | PUNCTUATION(";", _) |
          PUNCTUATION("]", _) | PUNCTUATION("==", _) | FLOAT(_, _) | INTEGER(_, _) | PUNCTUATION(">=", _) |
          PUNCTUATION("<=", _) | PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) |
          OPERATOR("!", _) =>
        writer.write("arithExprPrime -> EPSILON\n")
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
          writer.write("variableIdnestWrapper -> variableIdnest variableIdnestWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION(")", _) | PUNCTUATION("=", _) =>
        writer.write("variableIdnestWrapper -> EPSILON\n")
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
          writer.write("variableIdnest -> id indiceWrapper\n")
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
          writer.write("relOp -> ==\n")
        } else {
          error = true
        }
      case PUNCTUATION("<>", _) =>
        if(m(PUNCTUATION("<>"), root)){
          writer.write("relOp -> <>\n")
        } else {
          error = true
        }
      case PUNCTUATION("<", _) =>
        if(m(PUNCTUATION("<"), root)){
          writer.write("relOp -> <\n")
        } else {
          error = true
        }
      case PUNCTUATION(">", _) =>
        if(m(PUNCTUATION(">"), root)){
          writer.write("relOp -> >\n")
        } else {
          error = true
        }
      case PUNCTUATION("<=", _) =>
        if(m(PUNCTUATION("<="), root)){
          writer.write("relOp -> <=\n")
        } else {
          error = true
        }
      case PUNCTUATION(">=", _) =>
        if(m(PUNCTUATION(">="), root)){
          writer.write("relOp -> >=\n")
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
          writer.write("addOp -> +\n")
        } else {
          error = true
        }
      case OPERATOR("-", _) =>
        if (m(OPERATOR("-"), root)) {
          writer.write("addOp -> -\n")
        } else {
          error = true
        }
      case OPERATOR("||", _) =>
        if (m(OPERATOR("||"), root)) {
          writer.write("addOp -> ||\n")
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
          writer.write("multOp -> *\n")
        } else {
          error = true
        }
      case OPERATOR("/", _) =>
        if (m(OPERATOR("/"), root)) {
          writer.write("multOp -> /\n")
        } else {
          error = true
        }
      case OPERATOR("&&", _) =>
        if (m(OPERATOR("&&"), root)) {
          writer.write("multOp -> &&\n")
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
          writer.write("assignOp -> =\n")
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
          writer.write("term -> factor termPrime\n")
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
          writer.write("termPrime -> multOp factor termPrime\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | OPERATOR("+", _) | PUNCTUATION(",", _) |
           PUNCTUATION(";", _) | OPERATOR("-", _) | PUNCTUATION("]", _) | PUNCTUATION("==", _) | FLOAT(_, _) |
           INTEGER(_, _) | PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) |
           PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) | OPERATOR("!", _) =>
        writer.write("termPrime -> EPSILON\n")
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
          writer.write("variableAndFunctionCall -> id T3 T2\n")
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
          writer.write("T2 -> indice indiceWrapper T3\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) && T3(root)) {
          writer.write("T2 -> ( aParams ) T3\n")
        } else {
          error = true
        }
      case ID(_, _) =>
        if (variableAndFunctionCall(root)) {
          writer.write("T2 -> variableAndFunctionCall\n")
        } else {
          error = true
        }
      case PUNCTUATION(")", _) | OPERATOR("+", _) | OPERATOR("*", _) | PUNCTUATION(",", _) | OPERATOR("-", _) |
           OPERATOR("/", _) | PUNCTUATION(";", _) | PUNCTUATION("]", _) | OPERATOR("&&", _) | PUNCTUATION("==", _) |
           PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) | PUNCTUATION("<", _) |
           PUNCTUATION(">", _) | PUNCTUATION("<>", _) =>
        writer.write("T2 -> EPSILON\n")
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
          writer.write("T3 -> .\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | OPERATOR("+", _) | OPERATOR("*", _) |
           PUNCTUATION(",", _) | OPERATOR("-", _) | PUNCTUATION(";", _) | PUNCTUATION("]", _) | PUNCTUATION("[", _) |
           PUNCTUATION("=", _) | OPERATOR("&&", _) | OPERATOR("/", _) |
           PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) |
           PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) =>
        writer.write("T3 -> EPSILON\n")
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
          writer.write("sign -> +\n")
        } else {
          error = true
        }
      case OPERATOR("-", _) =>
        if (m(OPERATOR("-"), root)) {
          writer.write("sign -> -\n")
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
          writer.write("factor -> variableAndFunctionCall\n")
        } else {
          error = true
        }
      case INTEGER(_, _) =>
        if (m(INTEGER(""), root)) {
          writer.write("factor -> integer\n")
        } else {
          error = true
        }
      case FLOAT(_, _) =>
        if (m(FLOAT(""), root)) {
          writer.write("factor -> float\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && arithExpr(root) && m(PUNCTUATION(")"), root)) {
          writer.write("factor -> ( arithExpr )\n")
        } else {
          error = true
        }
      case OPERATOR("!", _) =>
        if (m(OPERATOR("!"), root) && factor(root)) {
          writer.write("factor -> ! factor\n")
        } else {
          error = true
        }
      case OPERATOR("+", _) | OPERATOR("-", _) =>
        if (sign(root) && factor(root)) {
          writer.write("factor -> sign factor\n")
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
          writer.write("idnestWrapper -> idnest idnestWrapper\n")
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
          writer.write("idnest -> id idnestExt\n")
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
          writer.write("idnestExt -> indiceWrapper .\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) =>
        if (m(PUNCTUATION("("), root) && aParams(root) && m(PUNCTUATION(")"), root) && m(PUNCTUATION("."), root)) {
          writer.write("idnestExt -> ( aParams ) .\n")
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
          writer.write("indiceWrapper -> idice indiceWrapper\n")
        } else {
          error = true
        }
      case PUNCTUATION("(", _) | PUNCTUATION(")", _) | OPERATOR("+", _) | OPERATOR("*", _) |
           PUNCTUATION(",", _) | OPERATOR("-", _) | PUNCTUATION(";", _) | PUNCTUATION("]", _) |
           PUNCTUATION("=", _) | FLOAT(_, _) | OPERATOR("&&", _) | PUNCTUATION(".", _) |
           INTEGER(_, _) | PUNCTUATION(">=", _) | OPERATOR("||", _) | PUNCTUATION("<=", _) |
           PUNCTUATION("<", _) | PUNCTUATION(">", _) | ID(_, _) | PUNCTUATION("<>", _) | OPERATOR("!", _) =>
        writer.write("indiceWrapper -> EPSILON\n")
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
          writer.write("indice -> [ arithExpr ]\n")
        } else {
          error = true
        }
    }
    parent.addChild(root)
    !error
  }
}
