package parser

class ASTNode(v: String) {

  val value: String = v
  var children: Seq[ASTNode] = Seq.empty

  def addChild(child: ASTNode): Unit ={
    children = children ++ Seq(child)
  }

  def print(level: Integer = 0): String = {
    var output = ""
    for (_ <- 0 to level){
      output += "  "
    }
    output += value + "\n"
    children.foreach(child => output += child.print(level+1))
    output
  }
}
