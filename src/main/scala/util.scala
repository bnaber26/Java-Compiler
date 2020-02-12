package util

import ir.{IR_Node}
import fragment.{Fragment}

object Util {

  def print_ir_tree(tree : IR_Node) = {
    tree.prettyPrint(0)
  }

  def print_program(main_code : IR_Node, fragments : List[Fragment]) = {
    println("main code: ")
    print_ir_tree(main_code)
    for (frag <- fragments) {
      println("\n fragment label: %s".format(frag.get_label_name))
      println("fragment intermediate code for body:")
      print_ir_tree(frag.get_body)
    }
  }

}
