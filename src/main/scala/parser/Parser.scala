package parser

import tokenizer.Token
import tokenizer.Token._

class Parser {

}

object Parser {

  var lookahead: Token = OPERATOR("")
  var tokenIterator: Iterator[Token] = Iterator()

  case class ParseResult(productions: Seq[String], errors: Seq[String])

  def parse(tokens: Seq[Token]): ParseResult = {

    var error = false
    tokenIterator = tokens.iterator
    lookahead = tokenIterator.next()

    if(prog()) {
      ParseResult(Seq.empty, Seq.empty)
    }else{
      ParseResult(Seq.empty, Seq.empty)
    }
  }

  private def m(t: Token): Boolean = {
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
      lookahead = tokenIterator.next()
    }
    result
  }

  private def prog(): Boolean = {
    var error = false
    if(classDeclWrapper() && funcDefWrapper() && m(RESERVED("main"))
      && funcBody() && m(PUNCTUATION(";"))) {
      error = true
    }
    !error
  }

  private def classDeclWrapper(): Boolean = {
    var error = false
    lookahead match {
      case RESERVED("class") =>
        if (classDecl() && classDeclWrapper()) {
          println("classDeclWrapper -> classDecl classDeclWrapper")
        } else {
          error = true
        }
      case FLOAT(_) | INTEGER(_) | ID(_) | RESERVED("main") =>
        println("classDeclWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def classDecl(): Boolean = {
    var error = false
    lookahead match {
      case RESERVED("class") =>
        if (m(RESERVED("class")) && m(ID("")) && optionalIDExt() && m(PUNCTUATION("{"))
          && genericDeclWrapper() && m(PUNCTUATION("}")) && m(PUNCTUATION(";"))){
          println("classDecl -> class id optionalIDExt { genericDeclWrapper } ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def funcDefWrapper(): Boolean = {
    var error = false
    lookahead match {
      case FLOAT(_) | INTEGER(_) | ID(_) =>
        if (funcDef() && funcDefWrapper()) {
          println("funcDefWrapper -> funcDef funcDefWrapper")
        } else {
          error = true
        }
      case RESERVED("main") =>
        println("funcDefWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def funcDef(): Boolean = {
    var error = false
    lookahead match {
      case FLOAT(_) | INTEGER(_) | ID(_) =>
        if (funcHead() && funcBody() && m(PUNCTUATION(";"))) {
          println("funcDef -> funcHead funcBody")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def funcBody(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("{") =>
        if (m(PUNCTUATION("{")) && statementWrapper() && m(PUNCTUATION("}"))) {
          println("funcBody -> { statementWrapper }")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def optionalIDExt(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(":") =>
        if (m(PUNCTUATION(":")) && m(ID("")) && idWrapper()) {
          println("optionalIDExt -> : id idWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("{") =>
        println("optionalIDExt -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def idWrapper(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(",") =>
        if (idEntity() && idWrapper()) {
          println("idWrapper -> idEntity idWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("{") =>
        println("idWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def idEntity(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(",") =>
        if (m(PUNCTUATION(",")) && m(ID(""))) {
          println("idEntity -> , id")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def genericDeclWrapper(): Boolean = {
    var error = false
    lookahead match {
      case FLOAT(_) | ID(_) | INTEGER(_) =>
        if (genericDecl() && genericDeclWrapper()) {
          println("genericDeclWrapper -> genericDecl genericDeclWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
        println("genericDeclWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def genericDecl(): Boolean = {
    var error = false
    lookahead match {
      case FLOAT(_) | ID(_) | INTEGER(_) =>
        if (`type`() && m(ID("")) && genericExtension() && m(PUNCTUATION(";"))) {
          println("genericDecl -> type id genericExtension ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def genericExtension(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("(")) && fParams() && m(PUNCTUATION(")"))) {
          println("genericExtension -> ( fParams )")
        } else {
          error = true
        }
      case PUNCTUATION(";") | OPERATOR("=") | PUNCTUATION("[") =>
        if (arraySizeWrapper()) {
          println("genericExtension -> arraySizeWrapper")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def funcHead(): Boolean = {
    var error = false
    lookahead match {
      case FLOAT(_) | ID(_) | INTEGER(_) =>
        if (`type`() && optionalIDSR() && m(PUNCTUATION("("))
            && fParams() && m(PUNCTUATION(")"))) {
          println("funcHead -> type optionalIDSR ( fParams )")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def optionalIDSR(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if (m(ID("")) && optionalIDSRExt()) {
          println("optionalIDSR -> id optionalIDSRExt")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def optionalIDSRExt(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if (m(PUNCTUATION(":")) && m(ID(""))) {
          println("optionalIDSRExt -> sr id")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        println("optionalIDSRExt -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def `type`(): Boolean = {
    var error = false
    lookahead match {
      case RESERVED("float") =>
        if (m(RESERVED("float"))) {
          println("type -> float")
        } else {
          error = true
        }
      case RESERVED("integer") =>
        if (m(RESERVED("integer"))) {
          println("type -> integer")
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID(""))) {
          println("type -> id")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def fParams(): Boolean = {
    var error = false
    lookahead match {
      case FLOAT(_) | ID(_) | INTEGER(_) =>
        if (`type`() && m(ID("")) && arraySizeWrapper() && fParamsTailWrapper()) {
          println("fParams -> type id arraySizeWrapper fParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("fParams -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def fParamsTailWrapper(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(",") =>
        if (fParamsTail() && fParamsTailWrapper()) {
          println("fParamsTailWrapper -> fParamsTail fParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("fParamsTailWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def fParamsTail(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(",") =>
        if (m(PUNCTUATION(",")) && `type`() && m(ID("")) && arraySizeWrapper()) {
          println("fParamsTail -> , type id arraySizeWrapper")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def aParams(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | ID(_) | INTEGER(_) | OPERATOR("!") =>
        if (expr() && aParamsTailWrapper()) {
          println("aParams -> expr aParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("aParams -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def aParamsTailWrapper(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(",") =>
        if (aParamsTail() && aParamsTailWrapper()) {
          println("aParamsTailWrapper -> aParamsTail aParamsTailWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") =>
        println("aParamsTailWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def aParamsTail(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(",") =>
        if (m(PUNCTUATION(",")) && expr()) {
          println("aParamsTail -> , expr")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def statementWrapper(): Boolean = {
    var error = false
    lookahead match {
      case RESERVED("float") | RESERVED("for") | RESERVED("if") | RESERVED("read")
        | RESERVED("return") | RESERVED("write") | RESERVED("integer") | ID(_) =>
        if (statement() && statementWrapper()) {
          println("statementWrapper -> statement statementWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
        println("statementWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def assignStatAndVar(): Boolean = {
    var error = false
    lookahead match {
      case RESERVED("integer") =>
        if (m(RESERVED("integer")) && m(ID("")) && genericExtension() && assignOp()
          && expr() && m(PUNCTUATION(";"))) {
          println("assignStatAndVar -> integer id genericExtension assignOp expr ;")
        } else {
          error = true
        }
      case RESERVED("float") =>
        if (m(RESERVED("float")) && m(ID("")) && genericExtension() && assignOp()
          && expr() && m(PUNCTUATION(";"))) {
          println("assignStatAndVar -> float id genericExtension assignOp expr ;")
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID("")) && statOrVarExt() && assignOp() && expr() && m(PUNCTUATION(";"))) {
          println("assignStatAndVar -> id statOrVarExt assignOp expr ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def statOrVarExt(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(".") | PUNCTUATION("[") =>
        if (indiceWrapper() && indiceExtInt()) {
          println("statOrVarExt -> indiceWrapper indiceExtInt")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("(")) && aParams() && m(PUNCTUATION(")")) &&
          m(PUNCTUATION(".")) && variableIdnestWrapper()) {
          println("statOrVarExt -> ( aParams ) . variableIdnestWrapper")
        } else {
          error = true
        }
      case ID(_) =>
        if (m(ID("")) && genericExtension() && m(PUNCTUATION(";"))) {
          println("statOrVarExt -> id genericExtension ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def indiceExtInt(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(".") =>
        if (m(PUNCTUATION(".")) && variableIdnestWrapper()) {
          println("indiceExtInt -> . variableIdnestWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("}") =>
        println("indiceExtInt -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def statement(): Boolean = {
    var error = false
    lookahead match {
      case RESERVED("float") | ID(_) | RESERVED("integer") =>
        if (assignStatAndVar()) {
          println("statement -> assignStatAndVar")
        } else {
          error = true
        }
      case RESERVED("if") =>
        if(m(RESERVED("if")) && m(PUNCTUATION("(")) && expr() && m(PUNCTUATION(")"))
          && m(RESERVED("then")) && statBlock() && m(RESERVED("else")) && statBlock()
          && m(PUNCTUATION(";"))) {
          println("statement -> if ( expr ) then statBlock else statBlock ;")
        } else {
          error = true
        }
      case RESERVED("for") =>
        if(m(RESERVED("for")) && m(PUNCTUATION("(")) && `type`() && m(ID("")) &&
          assignOp() && expr() && m(PUNCTUATION(";")) && relExpr() && m(PUNCTUATION(";"))
          && assignStat() && m(PUNCTUATION(")")) && statBlock() && m(PUNCTUATION(";"))){
          println("statement -> for ( type id assignOp expr ; relExpr ; assignStat ) statBlock ;")
        } else {
          error = true
        }
      case RESERVED("read") =>
        if(m(RESERVED("read")) && m(PUNCTUATION("(")) && variableIdnestWrapper()
          && m(PUNCTUATION(")")) && m(PUNCTUATION(";"))) {
          println("statement -> read ( variable ) ;")
        } else {
          error = true
        }
      case RESERVED("write") =>
        if(m(RESERVED("write")) && m(PUNCTUATION("(")) && variableIdnestWrapper()
          && m(PUNCTUATION(")")) && m(PUNCTUATION(";"))) {
          println("statement -> write ( variable ) ;")
        } else {
          error = true
        }
      case RESERVED("return") =>
        if(m(RESERVED("return")) && m(PUNCTUATION("(")) && variableIdnestWrapper()
          && m(PUNCTUATION(")")) && m(PUNCTUATION(";"))) {
          println("statement -> return ( variable ) ;")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def arraySizeWrapper(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("[") =>
        if (arraySize() && arraySizeWrapper()) {
          println("arraySizeWrapper -> arraySize arraySizeWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") | PUNCTUATION(",") | PUNCTUATION(";") | OPERATOR("=") =>
        println("arraySizeWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def arraySize(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("[") =>
        if (m(PUNCTUATION("[")) && m(INTEGER("")) && m(PUNCTUATION("]"))) {
          println("arraySize -> [ intNum ]")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def assignStat(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("=") | ID(_) =>
        if (variableIdnestWrapper() && assignOp() && expr()) {
          println("assignStat -> variable assignOp expr")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def expr(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(",") | OPERATOR("!") | FLOAT(_) | INTEGER(_) | ID(_) =>
        if (arithExpr() && exprExt()) {
          println("expr -> arithExpr exprExt")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def exprExt(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("=") | OPERATOR(">") | OPERATOR(">=")
           | OPERATOR("<=") | OPERATOR("<") | OPERATOR("!=") =>
        if (relOp() && arithExpr()) {
          println("exprExt -> relOp arithExpr")
        } else {
          error = true
        }
      case PUNCTUATION("(") | FLOAT(_) | INTEGER(_) | OPERATOR("!") =>
        if (term()) {
          println("exprExt -> term")
        } else {
          error = true
        }
      case PUNCTUATION(",") | PUNCTUATION(")") | PUNCTUATION(";") =>
          println("exprExt -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def statBlock(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("{") =>
        if (m(PUNCTUATION("{")) && statementWrapper() && m(PUNCTUATION("}"))) {
          println("statBlock -> { statementWrapper }")
        } else {
          error = true
        }
      case RESERVED("float") | RESERVED("for") | RESERVED("if") | RESERVED("read")
           | RESERVED("return") | RESERVED("write") | RESERVED("integer") | ID(_)
           | PUNCTUATION("{") =>
        if (statement()) {
          println("statBlock -> statement")
        } else {
          error = true
        }
      case PUNCTUATION(";") | RESERVED("else") =>
        println("statBlock -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def relExpr(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | INTEGER(_) | OPERATOR("!") | ID(_) =>
        if (arithExpr() && relOp() && arithExpr()) {
          println("relExpr -> arithExpr relOp arithExpr")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def arithExpr(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | INTEGER(_) | OPERATOR("!") | ID(_) =>
        if (term() && arithExprPrime()) {
          println("arithExpr -> term arithExprPrime")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def arithExprPrime(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("+") | OPERATOR("-") | OPERATOR("||") =>
        if (addOp() && term() && arithExprPrime()) {
          println("arithExprPrime -> addOp term arithExprPrime")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | PUNCTUATION(",") | PUNCTUATION(";") |
          PUNCTUATION("]") | OPERATOR("=") | FLOAT(_) | INTEGER(_) | OPERATOR(">=") |
          OPERATOR("<=") | OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") |
          OPERATOR("!") =>
        println("arithExprPrime -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def variableIdnestWrapper(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if(variableIdnest() && variableIdnestWrapper()){
          println("variableIdnestWrapper -> variableIdnest variableIdnestWrapper")
        } else {
          error = true
        }
      case PUNCTUATION(")") | OPERATOR("=") =>
        println("variableIdnestWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def variableIdnest(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if(m(ID("")) && variableIdnest()){
          println("variableIdnest -> id variableIdnest")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def relOp(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("=") =>
        if(m(OPERATOR("="))){
          println("relOp -> eq")
        } else {
          error = true
        }
      case OPERATOR("!=") =>
        if(m(OPERATOR("!="))){
          println("relOp -> neq")
        } else {
          error = true
        }
      case OPERATOR("<") =>
        if(m(OPERATOR("<"))){
          println("relOp -> lt")
        } else {
          error = true
        }
      case OPERATOR(">") =>
        if(m(OPERATOR(">"))){
          println("relOp -> gt")
        } else {
          error = true
        }
      case OPERATOR("<=") =>
        if(m(OPERATOR("<="))){
          println("relOp -> leq")
        } else {
          error = true
        }
      case OPERATOR(">=") =>
        if(m(OPERATOR(">="))){
          println("relOp -> geq")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def addOp(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("+") =>
        if (m(OPERATOR("+"))) {
          println("addOp -> +")
        } else {
          error = true
        }
      case OPERATOR("-") =>
        if (m(OPERATOR("-"))) {
          println("addOp -> -")
        } else {
          error = true
        }
      case OPERATOR("||") =>
        if (m(OPERATOR("||"))) {
          println("addOp -> or")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def multOp(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("*") =>
        if (m(OPERATOR("*"))) {
          println("multOp -> *")
        } else {
          error = true
        }
      case OPERATOR("/") =>
        if (m(OPERATOR("/"))) {
          println("multOp -> /")
        } else {
          error = true
        }
      case OPERATOR("&&") =>
        if (m(OPERATOR("&&"))) {
          println("multOp -> and")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def assignOp(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("=") =>
        if (m(OPERATOR("="))) {
          println("assignOp -> =")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def term(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("(") | FLOAT(_) | ID(_) | INTEGER(_) | OPERATOR("!") =>
        if (factor() && termPrime()) {
          println("term -> factor termPrime")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def termPrime(): Boolean = {
    var error = false
    lookahead match {
      case OPERATOR("*") | OPERATOR("/") | OPERATOR("&&") =>
        if (multOp() && factor() && termPrime()) {
          println("termPrime -> multOp factor termPrime")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | PUNCTUATION(",") |
           PUNCTUATION(";") | OPERATOR("-") | OPERATOR("]") | OPERATOR("=") | FLOAT(_) |
           INTEGER(_) | OPERATOR(">=") | OPERATOR("||") | OPERATOR("<=") |
           OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") | OPERATOR("!") =>
        println("termPrime -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def variableAndfunctionCall(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if (m(ID("")) && T2()) {
          println("variableAndfunctionCall -> id T2")
        } else {
          error = true
        }
    }
    !error
  }

  private def T2(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("[") =>
        if (indice() && indiceWrapper() && T3()) {
          println("T2 -> indice indiceWrapper T3")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("(")) && aParams() && m(PUNCTUATION(")")) && T3()) {
          println("T2 -> ( aParams ) T3")
        } else {
          error = true
        }
      case ID(_) =>
        if (variableAndfunctionCall()) {
          println("T2 -> variableAndfunctionCall")
        } else {
          error = true
        }
    }
    !error
  }

  private def T3(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(".") =>
        if (m(PUNCTUATION("."))) {
          println("T3 -> .")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | OPERATOR("*") |
           PUNCTUATION(",") | OPERATOR("-") | PUNCTUATION(";") | OPERATOR("]") |
           OPERATOR("=") | FLOAT(_) | OPERATOR("&&") |
           INTEGER(_) | OPERATOR(">=") | OPERATOR("||") | OPERATOR("<=") |
           OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") | OPERATOR("!") =>
        println("T3 -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def factor(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if (variableAndfunctionCall()) {
          println("factor -> variableAndfunctionCall")
        } else {
          error = true
        }
      case INTEGER(_) =>
        if (m(INTEGER(""))) {
          println("factor -> intNum")
        } else {
          error = true
        }
      case FLOAT(_) =>
        if (m(FLOAT(""))) {
          println("factor -> floatNum")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("(")) && arithExpr() && m(PUNCTUATION(")"))) {
          println("factor -> ( arithExpr )")
        } else {
          error = true
        }
      case PUNCTUATION("!") =>
        if (m(PUNCTUATION("!")) && factor()) {
          println("factor -> not factor")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def idnestWrapper(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if (idnest() && idnestWrapper()) {
          println("idnestWrapper -> idnest idnestWrapper")
        } else {
          error = true
        }
      case _ =>
        println("idnestWrapper -> EPSILON")
    }
    !error
  }

  private def idnest(): Boolean = {
    var error = false
    lookahead match {
      case ID(_) =>
        if (m(ID("")) && idnestExt()) {
          println("idnest -> id idnestExt")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def idnestExt(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION(".") | PUNCTUATION("[") =>
        if (indiceWrapper() && m(PUNCTUATION("."))) {
          println("idnestExt -> indiceWrapper .")
        } else {
          error = true
        }
      case PUNCTUATION("(") =>
        if (m(PUNCTUATION("(")) && aParams() && m(PUNCTUATION(")")) && m(PUNCTUATION("."))) {
          println("idnestExt -> ( aParams ) .")
        } else {
          error = true
        }
      case _ =>
        error = true
    }
    !error
  }

  private def indiceWrapper(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("[") =>
        if (indice() && indiceWrapper()) {
          println("indiceWrapper -> indice indiceWrapper")
        } else {
          error = true
        }
      case PUNCTUATION("(") | PUNCTUATION(")") | OPERATOR("+") | OPERATOR("*") |
           PUNCTUATION(",") | OPERATOR("-") | PUNCTUATION(";") | OPERATOR("]") |
           OPERATOR("=") | FLOAT(_) | OPERATOR("&&") |
           INTEGER(_) | OPERATOR(">=") | OPERATOR("||") | OPERATOR("<=") |
           OPERATOR("<") | OPERATOR(">") | ID(_) | OPERATOR("!=") | OPERATOR("!") =>
        println("indiceWrapper -> EPSILON")
      case _ =>
        error = true
    }
    !error
  }

  private def indice(): Boolean = {
    var error = false
    lookahead match {
      case PUNCTUATION("[") =>
        if (m(PUNCTUATION("[")) && arithExpr() && m(PUNCTUATION("]"))) {
          println("indiceWrapper -> [ arithExpr ]")
        } else {
          error = true
        }
    }
    !error
  }
}
