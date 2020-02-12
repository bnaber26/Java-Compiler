package canon

import ir._
import fragment.Fragment
import scala.collection.mutable.ListBuffer

class Canonicalizer(val main_ir : ir.Stmt, val fragments : List[Fragment]) {

  def transform() : (ir.Stmt, List[Fragment]) = {
    val transformed_main = main_ir.transform()
    val transformed_fragments = ListBuffer[Fragment]()
    for (frag <- fragments) {
      frag.body.prettyPrint(0)
      val transformed_fragment = frag.transform()
      transformed_fragment.body.prettyPrint(0)
      transformed_fragments += transformed_fragment
    }
    (transformed_main, transformed_fragments.toList)
  }


  def linearize(node : ir.Stmt) : ir.Stmt= {
    // Basic idea: We will transform statements of the form Seq(Seq(a,b), c) => Seq(a, Seq(b,v))
    // To do that we pattern match on the node to look for Seq(Seq(..),..) nodes. Then recurse on
    // left node of Seq. This will return a tuple of nodes, the first element contains the node
    // that we can move out of the Seq, and the second returns the rest of the statement
    // We will then move the inner Seq to the right node and recurse on the resulting node again

    def process_left_node(node : ir.Stmt) : (ir.Stmt, Option[ir.Stmt]) = {
      node match {
        case Seq(Seq(x, y), z) => {
          // call recursively on node.left
          val (move_out, left_rest) = process_left_node(node.asInstanceOf[ir.Seq].left)
          left_rest match {
            case Some(r) => (move_out, Some(Seq(r, z)))
            case None => (move_out, Some(z))
          }
        }
        case Seq(x, y) => {
          (x, Some(y))
        }
        case x => {
          (x, None)
        }
      }
    }

    node match {
      case Seq(Seq(x, y), z) => {
        // Recurse on x
        val (moved_out, rest) = process_left_node(node.asInstanceOf[Seq].left)

        // Need to linearize new right node
        val updated_right_node = rest match {
          case Some(rest) => {
            val temp_node = Seq(rest, z)
            //temp_node.prettyPrint(0)
            linearize(Seq(rest, z))
          }
          case None => linearize(z)
        }

        Seq(moved_out, updated_right_node)
      }
      case Seq(x, y) => Seq(x, linearize(y))
      case x => x
    }
  }

  def get_statement_list(node : ir.IR_Node) : ir.StmtList = {

    def extract_stmts(node : ir.Stmt, stmts_list : List[ir.Stmt]) : List[ir.Stmt] = {
      node match {
        case Seq(x, y) => extract_stmts(y, stmts_list :+ x)
        case x => stmts_list :+ x
      }
    }

    val stmts = List[ir.Stmt]()
    node match {
      case Eseq(s,_) => {
        val linearized_stmt = linearize(s)
        ir.StmtList(extract_stmts(linearized_stmt, stmts))
      }
      case x : ir.Stmt => {
        val linearized_stmt = linearize(x)
        ir.StmtList(extract_stmts(linearized_stmt, stmts))
      }
      case _ => throw new Exception("Cannot create a StmtList from ir.Exp")
    }
  }
}
