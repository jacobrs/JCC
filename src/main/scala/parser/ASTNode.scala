package parser

import tokenizer.Token.Location

class ASTNode(v: String, loc: Location = Location(0,0), metadata: Option[String] = None) {

  val value: String = v
  var children: Seq[ASTNode] = Seq.empty
  val location: Location = loc

  def addChild(child: ASTNode): Unit ={
    children = children ++ Seq(child)
  }

  def print(level: Integer = 0): String = {
    var output = ""
    for (_ <- 0 to level){
      output += "  "
    }
    output += metadata.getOrElse("") + value + "\n"
    children.foreach(child => output += child.print(level+1))
    output
  }
}
