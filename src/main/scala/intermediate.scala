package ir

import scala.collection.mutable.ListBuffer

object IR_Node {

  def commute(a : Stmt, b : Exp) : Boolean = {
    is_not_op(a) || b.isInstanceOf[Const] || b.isInstanceOf[Name]
  }

  def is_not_op(node : Stmt) : Boolean = {
    node match {
      case NoOp() => true
      case _ => false
    }
  }

  def reorder_stmt(stmt : Stmt) : Stmt = {
    val StmtsExpList(s, exps) = stmt.get_children.transform()
    Seq(s, stmt.build(exps)).simplify()
  }

  def reorder_exp(exp : Exp) : Eseq = {
    val StmtsExpList(s, exps) = exp.get_children.transform()
    Eseq(s, exp.build(exps))
  }
}

abstract class IR_Node {
  import IR_Node.{reorder_exp, reorder_stmt}

  def children : ExpList

  def get_children : ExpList = children

  def prettyPrint(indent : Int)

  def transform() : IR_Node

  def build(exps : ExpList) : IR_Node
}

abstract class Exp extends IR_Node {
  def transform() : Eseq = {
    IR_Node.reorder_exp(this)
  }

  def build(exps : ExpList) : Exp
}

case class Const(val value : Int) extends Exp {

  def children = ExpList(List[Exp]())

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Const(%d)".format(value))
  }

  override def build(exps : ExpList) = {
    this
  }
}
case class Name(val label : temp.Label) extends Exp {

  def children = ExpList(List[Exp]())

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Name(%s)".format(label.toString))
  }

  override def build(exps : ExpList) = {
    this
  }
}

case class Temp(val temporary : temp.Temp) extends Exp {

  def children = ExpList(List[Exp]())

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Temp(%s)".format(temporary.toString))
  }

  override def build(exps : ExpList) = {
    this
  }
}

case class BinOp(val op : String, left : Exp, right : Exp) extends Exp {

  def children = ExpList(List(left, right))

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "BinOp(%s)".format(op))
    left.prettyPrint(indent + 1)
    right.prettyPrint(indent + 1)
  }

  override def build(exps : ExpList) = {
    BinOp(op, exps.get_list.head, exps.get_list.tail.head)
  }
}

case class Mem(val exp : Exp) extends Exp {

  def children = ExpList(List(exp))

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Mem")
    exp.prettyPrint(indent + 1)
  }

  override def build(exps : ExpList) = {
    Mem(exps.get_list.head)
  }
}

case class Call(val func : Name, val args : ExpList) extends Exp {

  def children = ExpList(func::args.get_list)

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Call")
    func.prettyPrint(indent + 1)
    args.prettyPrint(indent + 1)
  }

  override def build(exps : ExpList) = {
    assert(exps.get_list.head.isInstanceOf[Name])
    Call(exps.get_list.head.asInstanceOf[Name], ExpList(exps.get_list.tail))
  } 
}

case class Eseq(val stmt : Stmt, val exp : Exp) extends Exp {

  def children = ExpList(List(exp))

  override def get_children = throw new Exception("get_children not implemented for Eseq")

  override def build(exps : ExpList) = throw new Exception("build not implemented for Eseq")

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Eseq")
    stmt.prettyPrint(indent + 1)
    exp.prettyPrint(indent + 1)
  }

  override def transform() : Eseq = {
    val transformed_stmt = stmt.transform()
    val new_exp = exp.transform()
    Eseq(Seq(transformed_stmt, new_exp.stmt).simplify(), new_exp.exp)
  }
}

// This is a node that calls system call for memory allocation and generates
// an address to the allocated block => Need to be able to handle BinOp with Alloc
// and possibly other nodes
case class AllocObject(val class_size: Int) extends Exp {

  def children = ExpList(List[Exp]())

  override def build(exps : ExpList) = this

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "AllocObject")
  }

}

case class AllocArray(val length : Exp) extends Exp {

  def children = ExpList(List[Exp](length))

  override def build(exps : ExpList) = AllocArray(exps.get_list.head)
  
  override def prettyPrint(indent : Int) = {
    println(" "*indent + "AllocArray")
    length.prettyPrint(indent + 1)
  }

}

case class NoOp() extends Stmt{

  def children = ExpList(List[Exp]())

  override def build(exps : ExpList) = this

  override def prettyPrint(indent : Int) = {println(" "*indent + "NoOp")}

}

case class This() extends Exp {

  def children = ExpList(List[Exp]())

  override def build(exps : ExpList) = this

  override def prettyPrint(indent: Int): Unit = {
    println(" "*indent + "This")
  }

}


abstract class Stmt extends IR_Node {
  override def transform() = {
    IR_Node.reorder_stmt(this)
  }

  def build(exps : ExpList) : Stmt
}

case class Move(val dst : Exp, val src : Exp) extends Stmt {

  def children = ExpList(List(dst, src))

  override def get_children = {
    dst match {
      case Mem(addr) => ExpList(List(addr, src))
      case _ => ExpList(List(src))
    }
  }

  override def build(exps : ExpList) = {
    dst match {
      case Mem(addr) => Move(Mem(exps.get_list.head), exps.get_list.tail.head)
      case _ => Move(dst, exps.get_list.head)
    }
  }

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Move")
    dst.prettyPrint(indent + 1)
    src.prettyPrint(indent + 1)
  }

  override def transform() = {

    // Do not call transform_children if we have a Move(Temp, Call) instance, because
    // we recursively move Call instances to the top by simplifying them to
    // Eseq(Move(Temp, Call), Temp). So we need to make sure not to recurse indefinitely here
    children.get_list match {
      case List(t @ Temp(_), c @ Call(_,_)) => IR_Node.reorder_stmt(MoveCall(t,c))
      case List(Eseq(s, e), x) => Seq(s, Move(e, x)).transform()
      case x => IR_Node.reorder_stmt(this)
    }
  }
}

case class MoveCall(t : Temp, c : Call) extends Stmt {
  def children = ExpList(t::c.children.get_list)

  override def build(exps : ExpList) = {
    Move(exps.get_list.head, c.build(ExpList(exps.get_list.tail)))
  }

  override def prettyPrint(indent: Int): Unit = {
    println(" "*indent + "MoveCall")
    t.prettyPrint(indent + 1)
    c.prettyPrint(indent + 1)
  }
}

case class ExpCall(c : Call) extends Stmt {
  def children = c.children 

  override def build(exps : ExpList) = {
    Expression(c.build(exps))
  }

  override def prettyPrint(indent: Int): Unit = {
    println(" "*indent + "ExpCall")
    c.prettyPrint(indent + 1)
  }

}

// This is just for conversions from Exp to Stmt
case class Expression(val exp : Exp) extends Stmt {

  def children = ExpList(List(exp))

  override def build(exps : ExpList) = Expression(exps.get_list.head)

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Expression")
    exp.prettyPrint(indent + 1)
  }

  override def transform() = {
    exp match {
      case c @ Call(_,_) => IR_Node.reorder_stmt(ExpCall(c))
      case s => IR_Node.reorder_stmt(this)
    }
  }
}

case class Jump(target : temp.Label) extends Stmt {

  def children = ExpList(List[Exp]())

  override def build(exps : ExpList) = this

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Jump to %s".format(target.toString))
  }


}

case class CJump(val rel_op : String, val left : Exp, val right : Exp,
    val if_true : temp.Label, val if_false : temp.Label) extends Stmt {

  def children = ExpList(List(left, right))

  override def build(exps : ExpList) = {
    CJump(rel_op, exps.get_list.head, exps.get_list.tail.head, if_true, if_false)
  }

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "CJump(%s) to true: %s and false: %s".format(rel_op,
      if_true.toString, if_false.toString))
    left.prettyPrint(indent + 1)
    right.prettyPrint(indent + 1)
  }
}

case class Seq(val left : Stmt, val right : Stmt) extends Stmt {

  def children = ExpList(List[Exp]())

  override def get_children = throw new Exception("get_children not implemented for Ir.Seq")

  override def build(exps : ExpList) = throw new Exception("build not implemented for Ir.Seq")

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Seq")
    left match {
      case Seq(_,_) => left.prettyPrint(indent + 1)
      case _ => left.prettyPrint(indent + 1)
    }
    right match {
      case Seq(_,_) => right.prettyPrint(indent + 1)
      case _ => right.prettyPrint(indent + 1)
    }
  }

  override def transform() = {
    val left_transformed = left.transform()
    val right_transformed = right.transform()
    Seq(left_transformed, right_transformed).simplify()
  }

  def simplify() : Stmt = {
    if (IR_Node.is_not_op(left)) right
    else if (IR_Node.is_not_op(right)) left
    else this
  }
}

case class Label(val label : temp.Label) extends Stmt {

  def children = ExpList(List[Exp]())

  override def build(exps : ExpList) = this

  override def get_children = throw new Exception("get_children not implemented for Ir.Label")

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Label: %s".format(label.toString))
  }

  def get_temp_label = label

  override def transform() = this
}

case class PrintNode(val exp : Exp) extends Stmt {

  def children = ExpList(List(exp))

  override def build(exps : ExpList) = PrintNode(exps.get_list.head)

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Print")
    exp.prettyPrint(indent + 1)
  }

  override def transform() = this
}

case class ArrayBoundsException(val label : temp.Label) extends Stmt {

  def children = ExpList(List[Exp]())

  override def build(exps : ExpList) = this

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "ArrayBoundsException")
  }

  override def transform() = this
}


abstract class IrList extends IR_Node {
    def get_list : List[IR_Node]
}

case class ExpList(val exps: List[Exp]) extends IrList {

  def children = ExpList(exps)

  def prepend(exp : Exp) : ExpList = {
    ExpList(List(exp) ++ exps)
  }

  override def get_list = exps

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "ExpList")
    for (child <- exps) {
      child.prettyPrint(indent + 1)
    }
  }

  override def transform() : StmtsExpList = {

    // Note: Should implement head and tail on ExpList itself, so I don't have to
    // create ExpList instances here

    if (children.get_list.isEmpty) {
      StmtsExpList(NoOp(), ExpList(List[Exp]()))
    }
    else {
        children.get_list.head match {
          case x @ Call(_,_) => {
            val t = temp.Temp()
            val head_result = Eseq(Move(Temp(t), x), Temp(t))
            ExpList(head_result::children.get_list.tail).transform()
        }
        case x => {
          val head_result : Eseq = x.transform()
          val tail_result = ExpList(children.get_list.tail).transform()
          if (IR_Node.commute(tail_result.stmts, head_result.exp)) {
            StmtsExpList(Seq(head_result.stmt, tail_result.stmts).simplify(),
              ExpList(head_result.exp::tail_result.exps.get_list));
          }
          else {
            val temporary = temp.Temp()
            StmtsExpList(Seq(head_result.stmt, Seq(Move(Temp(temporary), head_result.exp),
              tail_result.stmts).simplify()).simplify(),
                ExpList(Temp(temporary)::tail_result.exps.get_list))
          }
        }
      }
    }
  }

  override def build(exps : ExpList) = throw new Exception("build is not implemented for ExpList")
}
case class StmtList(val stmts : List[Stmt]) extends IrList {

  def children = ExpList(List[Exp]()) 

  override def get_list = stmts 

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "StmtList")
    for (child <- stmts) {
      child.prettyPrint(indent + 1)
    }
  }

  override def transform() = throw new Exception("transform() not implemented for StmtList")

  override def build(exps : ExpList) = throw new Exception("build is not implemented for StmtsList")

  def foreach(stmt : Stmt => Unit) : Unit = {
    stmts.foreach(stmt)
  }

  def ::(stmt : Stmt) : StmtList = {
    StmtList(stmt :: stmts)
  }

  def :+(stmt : Stmt) : StmtList = {
    StmtList(stmts :+ stmt)
  }

  def head : Stmt = stmts.head

  def last : Stmt = stmts.last

}

case class StmtsExpList(stmts : Stmt, exps : ExpList) extends IR_Node {
  def children = exps

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "StmtsExpList")
    stmts.prettyPrint(indent + 1)
    exps.prettyPrint(indent + 1)
  }

  override def build(exps : ExpList) = throw new Exception("build is not implemented for StmtsExpList")

  override def transform() = throw new Exception("transform is not implemented for StmtsExpList")
}

