package translate

import ir._
import temp.{Temp, Label}

abstract class ExpTranslate{
  def gen_Ex() : ir.Exp
  def gen_Nx() : ir.Stmt
  def gen_Cx(t : Label, f : Label) : Stmt
}

class Expression(val exp : ir.Exp) extends ExpTranslate{
  def gen_Ex() : ir.Exp = {
    exp
  }

  def gen_Nx() : ir.Stmt = {
    // Just a conversion to ir.Stmt, that's the whole purpose of ir.Expression class
    ir.Expression(exp)
  }
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

  def gen_Cx(t : temp.Label, f : temp.Label) : ir.Stmt = {
    ir.CJump("==", exp, ir.Const(0), f, t)
  }

}

class NoResult(val stmt : ir.Stmt) extends ExpTranslate{
  def gen_Ex() : ir.Exp = throw new Exception("cannot convert NoResult to Expression")
  def gen_Nx() : ir.Stmt = stmt
  def gen_Cx(t : Label, f : Label) : ir.Stmt = throw new Exception("gen_Cx not defined for NoResult")

}

abstract class CondExpression() extends ExpTranslate {
  override def gen_Ex() : ir.Exp = {
    val result = temp.Temp()
    val true_label = temp.Label()
    val false_label = temp.Label()

    // We use the label result to store either Const(1) or Const(0) in
    // First store 1 in result, then test the conditional.
    // After the condition we set the false label in which we set result
    // to Const(0), then later we set the true label
    ir.Eseq(ir.Seq(ir.Move(ir.Temp(result), ir.Const(1)),
      ir.Seq(gen_Cx(true_label, false_label),
        ir.Seq(ir.Label(false_label), ir.Seq(ir.Move(ir.Temp(result),
          ir.Const(0)), ir.Label(true_label))))),
      ir.Temp(result))

  }

  override def gen_Nx() : ir.Stmt = {
    // We will just execute un_Cx and jump to a join label
    val l = Label()
    ir.Seq(gen_Cx(l,l), ir.Label(l))
  }

  def gen_Cx(t : temp.Label, f : temp.Label) : ir.Stmt

}



class RelCx(op : String, left : ExpTranslate, right :ExpTranslate) extends CondExpression{
  override def gen_Cx(t : temp.Label, f : temp.Label) : Stmt = {
    // Note: We only have LT comparator in language
    ir.CJump(op, left.gen_Ex(), right.gen_Ex(), t, f)
  }
}

class IfThenElseCx(cond : RelCx, first : ExpTranslate,
          second : ExpTranslate) extends CondExpression {

  val true_label = temp.Label()
  val false_label = temp.Label()
  val join_label = temp.Label()

  override def gen_Cx(true_part : temp.Label, false_part : temp.Label) : Stmt = {

    val then_stmt = Seq(ir.Label(true_label),
      Seq(first.gen_Cx(true_part, false_part), ir.Jump(join_label)))
    val else_stmt = Seq(ir.Label(false_label),
      Seq(second.gen_Cx(true_part, false_part), ir.Label(join_label)))

    Seq(cond.gen_Cx(true_label, false_label), Seq(then_stmt, Seq(else_stmt, ir.Label(join_label))))
  }
}

class ArrayLookupExp(val arr : ir.Exp, val index : ir.Exp) extends Expression(
  ir.Mem(ir.BinOp("+", ir.BinOp("+", arr, ir.Const(4)),
    ir.BinOp("*", index, ir.Const(4)))))

