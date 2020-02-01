package ir

abstract class IR_Node {
  def prettyPrint(indent : Int)
}

abstract class Exp extends IR_Node
case class Const(val value : Int) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Const(%d)".format(value))
  }
}
case class Name(val label : temp.Label) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Name(%s)".format(label.toString))
  }
}

case class Temp(val temporary : temp.Temp) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Temp(%s)".format(temporary.toString))
  }
}

case class BinOp(val op : String, left : Exp, right : Exp) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "BinOp(%s)".format(op))
    left.prettyPrint(indent + 1)
    right.prettyPrint(indent + 1)
  }
}

case class Mem(val exp : Exp) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Mem")
    exp.prettyPrint(indent + 1)
  }
}

case class Call(val func : Name, val args : ExpList) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Call")
    func.prettyPrint(indent + 1)
    args.prettyPrint(indent + 1)
  }
}

case class Eseq(val stmt : Stmt, val exp : Exp) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Eseq")
    stmt.prettyPrint(indent + 1)
    exp.prettyPrint(indent + 1)
  }
}

// This is a node that calls system call for memory allocation and generates
// an address to the allocated block => Need to be able to handle BinOp with Alloc
// and possibly other nodes
case class AllocObject(val class_size: Int) extends Exp {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "AllocObject")
  }
}

case class AllocArray(val length : Exp) extends Exp {
  
  override def prettyPrint(indent : Int) = {
    println(" "*indent + "AllocArray")
  }
}

case class NoOp() extends Exp{

  override def prettyPrint(indent : Int) = {}
}

case class This() extends Exp {
  override def prettyPrint(indent: Int): Unit = {
    println(" "*indent + "This")
  }
}


abstract class Stmt extends IR_Node

case class Move(val dst : Exp, val src : Exp) extends Stmt {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Move")
    dst.prettyPrint(indent + 1)
    src.prettyPrint(indent + 1)
  }
}

// This is just for conversions from Exp to Stmt
case class Expression(val exp : Exp) extends Stmt {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Expression")
    exp.prettyPrint(indent + 1)
  }
}

case class Jump(target : temp.Label) extends Stmt {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Jump to %s".format(target.toString))
  }
}

case class CJump(val rel_op : String, val left : Exp, val right : Exp,
    val if_true : temp.Label, val if_false : temp.Label) extends Stmt {


  override def prettyPrint(indent : Int) = {
    println(" "*indent + "CJump(%s) to true: %s and false: %s".format(rel_op,
      if_true.toString, if_false.toString))
    left.prettyPrint(indent + 1)
    right.prettyPrint(indent + 1)
  }
}

case class Seq(val left : Stmt, val right : Stmt) extends Stmt {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Seq")
    left match {
      case Seq(_,_) => left.prettyPrint(indent)
      case _ => left.prettyPrint(indent + 1)
    }
    right match {
      case Seq(_,_) => right.prettyPrint(indent)
      case _ => right.prettyPrint(indent + 1)
    }
  }
}

case class Label(val label : temp.Label) extends Stmt {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Label: %s".format(label.toString))
  }
}

case class PrintNode(val exp : Exp) extends Stmt {
  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Print")
    exp.prettyPrint(indent + 1)
  }

}

case class ArrayBoundsException(val label : temp.Label) extends Stmt {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "ArrayBoundsException")
  }
}

case class Print(val exp : Exp) extends Stmt {

  override def prettyPrint(indent : Int) = {
    println(" "*indent + "Print")
    exp.prettyPrint(indent + 1)
  }
}

abstract class IrList {
  def prettyPrint(indent : Int)
}

case class ExpList(val exps: List[Exp]) extends IrList {
  def prepend(exp : Exp) : ExpList = {
    ExpList(List(exp) ++ exps)
  }


  override def prettyPrint(indent : Int) = {
    println(" "*indent + "ExpList")
    for (child <- exps) {
      child.prettyPrint(indent + 1)
    }
  }
}
case class StmtList(val stmts : List[Stmt]) extends IrList {


  override def prettyPrint(indent : Int) = {
    println(" "*indent + "StmtList")
    for (child <- stmts) {
      child.prettyPrint(indent + 1)
    }
  }
}

