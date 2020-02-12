package canon

import ir._
import fragment.Fragment
import scala.collection.mutable.ListBuffer

class Canonicalizer(val main_ir : ir.Stmt, val fragments : List[Fragment]) {

  def transform() : (ir.Stmt, List[Fragment]) = {
    println("Calling transform on main IR Node")
    val transformed_main = main_ir.transform()
    println("Transformed main is as follows")
    transformed_main.prettyPrint(0)
    val transformed_fragments = ListBuffer[Fragment]()
    for (frag <- fragments) {
      println("About to call transform on the following fragment")
      frag.body.prettyPrint(0)
      val transformed_fragment = frag.transform()
      println("transformed fragment body is:")
      transformed_fragment.body.prettyPrint(0)
      transformed_fragments += transformed_fragment
    }
    println("Just returned from calling transform() on all fragments")
    (transformed_main, transformed_fragments.toList)
  }


  def linearize(node : ir.IR_Node) : ir.IR_Node = {
    // Basic idea: We will transform statements of the form Seq(Seq(a,b), c) => Seq(a, Seq(b,v))
    // To do that we pattern match on the node to look for Seq(Seq(..),..) nodes. Then recurse on
    // left node of Seq. This will return a tuple of nodes, the first element contains the node
    // that we can move out of the Seq, and the second returns the rest of the statement
    // We will then move the inner Seq to the right node and recurse on the resulting node again

    // I think I need a helper function to pull out the left node, this shouldn't be handled by
    // a recursive call

    def process_right_node(node : ir.Stmt) : ir.Stmt = {
      val linearized_node = linearize(node)
      linearized_node match {

      }
    }

    node match {
      case Seq(Seq(x, y), z) => {
        // Recurse on x
        val linearized_left_node = linearize(node.asInstanceOf[ir.Seq].left)
        // Recurse on updated right side
        val linearized_right = process_right_node(Seq(keep_in, z))
        Seq(move_out)
      }
      case Seq(x,y) => {
        // x is not Seq, so we can move x out
        // Note: Don't have to recurse on y here, since this will be done in the outer
        // recursive call
        (x, y)
      }
    }
  }
}
