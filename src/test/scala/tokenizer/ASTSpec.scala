package tokenizer

import org.scalatest.{Matchers, WordSpec}
import parser.ASTNode

class ASTSpec extends WordSpec with Matchers  {

  "AST data structure" should {

    "Create a default prog root node" in {
      val tree = new ASTNode("prog")
      tree.value should be("prog")
    }

    "Support appending additional children" in {
      val tree = new ASTNode("prog")
      val sampleChild = new ASTNode("main")
      tree.addChild(sampleChild)

      tree.value should be("prog")
      tree.children.head.value should be("main")
    }

    "Should create mutable nodes" in {
      val tree = new ASTNode("prog")
      mutateNode(tree)
      tree.children.size should be(1)
    }
  }

  private def mutateNode(node: ASTNode): Unit = {
    node.addChild(new ASTNode("mutatedChild"))
  }

}
