package canon

import ir._
import fragment.Fragment
import block.BasicBlock
import scala.collection.mutable.{ListBuffer, Set}

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

  def get_basic_blocks(stmts : ir.StmtList) : List[BasicBlock] = {
    // Basic idea: Iterate through StmtList creating a new Block as soon as a CJump or Jump
    // instruction is encountered
    // Each Block must start with a Label, if any found Block doesn't start with a Label, we
    // will insert a new Label

    def extract_blocks() : ListBuffer[BasicBlock] = {
      var blocks = ListBuffer[BasicBlock]()
      var current_block_stmts = ListBuffer[ir.Stmt]()

      for (stmt <- stmts) {
        stmt match {
            case (ir.CJump(_,_,_,_,_) | ir.Jump(_)) => {
                current_block_stmts += stmt
                blocks += BasicBlock(StmtList(current_block_stmts.toList))
                current_block_stmts = ListBuffer[ir.Stmt]()
            }
            case ir.Label(_) => {
                // Start a new Block. Note: Without this if statement we would add empty blocks
                if (!current_block_stmts.isEmpty) {
                    blocks += BasicBlock(StmtList(current_block_stmts.toList))
                }
                current_block_stmts = ListBuffer(stmt)
                }
                case _ => {
                  current_block_stmts += stmt
            }
        }
      }

      // Add the last block
      if (!current_block_stmts.isEmpty) {
        blocks += BasicBlock(StmtList(current_block_stmts.toList))
      }
      blocks
    }

    def insert_jumps_and_labels_into_blocks(blocks : ListBuffer[BasicBlock]) : ListBuffer[BasicBlock] = {
        var i = blocks.length - 1
        for (block<- blocks.reverse) {
            val last_statement = block.get_last_statement
            last_statement match {
                case (ir.CJump(_,_,_,_,_) | ir.Jump(_)) => {}
                case _ => {
                        if (i < blocks.length - 1) {

                        // Note: Last Block doesn't need to have an ending Jump instruction
                        // For all other blocks we need to insert a Jump to the label of the
                        // following block

                        assert(blocks(i+1).get_first_statement.isInstanceOf[ir.Label])

                        val next_label : temp.Label = blocks(i+1).get_first_statement.
                            asInstanceOf[ir.Label].get_temp_label
                        val jump_instruction = ir.Jump(next_label)
                        blocks(i) = block.append_statement(jump_instruction)
                    }
                    else {
                        // Last block always jumps to Label "done"
                        val last_jump = ir.Jump(temp.Label("done"))
                        blocks(i) = block.append_statement(last_jump)
                    }
                }
            }

            val first_statement = block.get_first_statement
            first_statement match {
                case Label(_) => {}
                case _ => {
                // Need to insert a Label as first statement of Block
                blocks(i) = block.prepend_statement(ir.Label(temp.Label()))
                }
            }
        i -= 1
      }
      blocks
    }

    def order_blocks(blocks : ListBuffer[BasicBlock]) : ListBuffer[BasicBlock] = {
      // Basic idea: In each iteration we start with some block that hasn't been added to
      // result List. If its last statement is a CJump we will then add the block
      // whose first statement corresponds to the false label of the CJump. We keep
      // adding blocks this way until we reach a deadend.
      // We then select another block from remaining set of blocks, note that it's guaranteed
      // by induction that the Block doesn't start with a label that corresponds to the false
      // label of a CJump of a block that has already been added, but it can end with a CJump
      // to a block that has already been added, in which case we need to insert it before
      // that block in the result list

      var ordered_blocks = ListBuffer[BasicBlock]()

      val remaining_block_set = Set[BasicBlock]()
      blocks.map(x => remaining_block_set.add(x))

      var current_block = remaining_block_set.head
      while (remaining_block_set.nonEmpty) {

        remaining_block_set.remove(current_block)

        current_block.get_jump match {
          case CJump(_,_,_,_, false_label) => {
            // Check whether any block in ordered_blocks contains false_label
            ordered_blocks.find(_.get_label.get_temp_label == false_label) match {
              case Some(b) => {
                // Add current_block before b in ordered_blocks list
                val index = ordered_blocks.indexOf(b)
                val (left, right) = ordered_blocks.splitAt(index)
                ordered_blocks = (left :+ current_block) ++ right

                if (remaining_block_set.nonEmpty) {
                  current_block = remaining_block_set.head
                }
              }
              case None => {
                ordered_blocks += current_block

                current_block = remaining_block_set.find(_.get_label.get_temp_label == false_label) match {
                  case Some(b) => b
                  case None => {
                    if (false_label.get_name != "ArrayBoundsException") {
                      throw new Exception("No block that begins with false_label of CJump exists")
                    }
                    if (remaining_block_set.nonEmpty) {
                      current_block = remaining_block_set.head
                    }
                    current_block
                  }
                }
              }
            }
          }
          case Jump(next_label) => {
            ordered_blocks += current_block
            current_block = remaining_block_set.find(_.get_label.get_temp_label == next_label) match {
              case Some(b) => b
              case None => {
                if (remaining_block_set.nonEmpty) {
                    current_block = remaining_block_set.head
                }
                current_block
                }
            }
          }
          case _ => throw new Exception("get_jump from Block must return either CJump or Jump instance")
        }
      }
      ordered_blocks
    }

    val blocks = extract_blocks()
    val updated_blocks = insert_jumps_and_labels_into_blocks(blocks)
    val ordered_blocks = order_blocks(updated_blocks)

    ordered_blocks.toList
  }
}
