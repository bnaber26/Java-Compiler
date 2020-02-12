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
    println("Inside reorder_stmt with Stmt:")
    stmt.prettyPrint(0)
    println("about to call transform on children list")
    println("children list is:")
    for ((child, i) <- stmt.get_children.get_list.zipWithIndex) {
      println("Child %d".format(i))
      child.prettyPrint(0)
    }
    val StmtsExpList(s, exps) = stmt.get_children.transform()
    println("Just returned from calling transform() on children of stmt:")
    stmt.prettyPrint(0)
    println("returned statement part of StmtsExpList is:")
    s.prettyPrint(0)
    println("returned ExpList part of StmtsExpList is:")
    exps.prettyPrint(0)
    println("Calling build on this expression list yields:")
    stmt.build(exps).prettyPrint(0)
    val return_value = Seq(s, stmt.build(exps)).simplify()
    println("return IR_Node of reorder_stmt:")
    return_value.prettyPrint(0)
    return_value
  }

  def reorder_exp(exp : Exp) : Eseq = {
    println("Inside reorder_exp with Exp:")
    exp.prettyPrint(0)
    println("about to call transform on children list")
    val StmtsExpList(s, exps) = exp.get_children.transform()
    val test = exp.build(exps)
    println("statement of returned Eseq is:")
    println(s)
    s.prettyPrint(0)
    println("exp of returned Eseq is:")
    println(test)
    test.prettyPrint(0)
    
    val return_value = Eseq(s, exp.build(exps))
    println("return IR_Node of reorder_exp:")
    return_value.prettyPrint(0)
    return_value
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
    println("Inside transform of Exp, about to call reorder_exp")
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

  /*
  override def transform() = {
    this
  }
   */

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

  /*
  override def transform() = {
    transform_children() match {
      case List(Eseq(s @ _, e1 @ _), e2 @ _) => Eseq(s, BinOp(op, e1, e2))
      case List(e1 @ _, Eseq(s @ _, e2 @ _)) => {
        val t = temp.Temp()
        Eseq(Move(Temp(t), e1), Eseq(s, BinOp(op, Temp(t), e2))) 
      }
      case List(e1 @ _, e2 @ _) => BinOp(op, e1, e2)
      case _ => throw new Exception("unexpected result returned from transform_children in BinOp")
    }
  }
   */

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

  /*
  override def transform(): IR_Node = {
    transform_children() match {
      case List(Eseq(s @ _, e @ _)) => Eseq(s, Mem(e))
      case List(e @ _) => Mem(e)
      case _ => throw new Exception("unexpected result returned form transform_children in Mem")
    }
  }
   */

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

  /*
  override def transform() = {
    val transformed_children = transform_children()
    val result = temp.Temp()
    Eseq(Move(Temp(result), Call(func, ExpList(transformed_children))), Temp(result))
  }
   */

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
    println("Inside transform() of Eseq")
    println("about to call transform on stmt")
    val transformed_stmt = stmt.transform()
    println("Just returned from calling tansform on stmt of following Eseq")
    this.prettyPrint(0)
    println("transformed stmt:")
    transformed_stmt.prettyPrint(0)
    println("about to call transform on exp")
    val new_exp = exp.transform()
    println("Just returned from calling tansform on exp of following Eseq")
    this.prettyPrint(0)
    println("transformed exp:")
    new_exp.prettyPrint(0)
    Eseq(Seq(transformed_stmt, new_exp.stmt).simplify(), new_exp.exp)
    /*
    new_exp match {
      case Eseq(s, e) => {
        println("Calling transform recursively on Eseq")
        Eseq(Seq(transformed_stmt, s).simplify(), e)
      }
      case _ => {
        println("returning Eseq(transformed_stmt, new_exp)")
        Eseq(transformed_stmt, new_exp)
      }
    }
     */
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

    println("Inside transform() of Move")

    // Do not call transform_children if we have a Move(Temp, Call) instance, because
    // we recursively move Call instances to the top by simplifying them to
    // Eseq(Move(Temp, Call), Temp). So we need to make sure not to recurse indefinitely here
    children.get_list match {
      case List(t @ Temp(_), c @ Call(_,_)) => {
        println("children list of Form List(Temp, Call), about to call reorder_stmt")
        val return_value = IR_Node.reorder_stmt(MoveCall(t,c))
        println("Just returned from calling reorder_stmt on following MoveCall")
        this.prettyPrint(0)
        println("reordered statement is:")
        return_value.prettyPrint(0)
        return_value
      }
      case List(Eseq(s, e), x) => {
        println("children list of Form List(Eseq(..),_), about to call transform on Seq")
        val return_value = Seq(s, Move(e, x)).transform()
        println("Just returned from calling reorder_stmt on following MoveCall")
        this.prettyPrint(0)
        println("reordered statement is:")
        return_value.prettyPrint(0)
        return_value
      }
      case x => {
        println("About to call reorder_stmt on this")
        val return_value = IR_Node.reorder_stmt(this)
        println("Just returned from calling reorder_stmt on following MoveCall")
        this.prettyPrint(0)
        println("reordered statement is:")
        return_value.prettyPrint(0)
        return_value
      }
        /*
        Seq(
        transform_children() match {
            case List(t @ Temp(_), Eseq(s @ _, e @ _)) => Eseq(s, Move(t,e))
            case List(Mem(Eseq(s @ _, e1 @ _)), e2 @ _) => Eseq(s, Move(Mem(e1), e2))
            case List(Mem(e1 @ _), Eseq(s @ _, e2 @ _)) => {
                if (commute(e1, s)) Eseq(s, Move(Mem(e1), e2))
                else Move(e1, Eseq(s, e2))
            }
            case List(e1 @ _, e2 @ _) => Move(e1, e2)
            case _ => throw new Exception("unexpected result returned form transform_children in Move")
        }
      }
         */
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
    println("Inside transform of Expression")
    exp match {
      case c @ Call(_,_) => {
        println("exp is a Call, about to call reorder_stmt")
        val return_value = IR_Node.reorder_stmt(ExpCall(c))
        println("Just returned from calling reorder_stmt on following Expression")
        this.prettyPrint(0)
        println("reordered statement is:")
        return_value.prettyPrint(0)
        return_value
      }
      case s => {
        println("about to call reorder_stmt on this")
        val return_value = IR_Node.reorder_stmt(this)
        println("Just returned from calling reorder_stmt on following Expression")
        this.prettyPrint(0)
        println("reordered statement is:")
        return_value.prettyPrint(0)
        return_value

      }
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

   /*
  override def transform = {
    transform_children() match {
      case List(Eseq(s @ _, e1 @ _), e2 @ _) => Seq(s, CJump(op, e1, e2, if_true, if_false))
      case List(e1 @ _, Eseq(s @ _, e2 @ _)) => {
        val t = temp.Temp()
        Seq(Move(Temp(t), e1), Seq(s, CJump(op, Temp(t), e2, if_true, if_false)))
      }
      case List(e1 @ _, e2 @ _) => CJump(op, e1, e2, if_true, if_false)
      case _ => throw new Exception("unexpected result returned form transform_children in CJump")

    }
  }
   */
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
    println("Inside transform of Seq")
    println("left is:")
    left.prettyPrint(0)
    println("right is:")
    right.prettyPrint(0)
    println("Calling transform on left stmt")
    val left_transformed = left.transform()
    println("Just returned from calling transform on left of following Seq")
    this.prettyPrint(0)
    println("reordered statement is:")
    println("left_transformed is:")
    left_transformed.prettyPrint(0)
    println("Calling transform on right stmt")
    val right_transformed = right.transform()
    println("Just returned from calling transform on right of following Seq")
    this.prettyPrint(0)
    println("reordered statement is:")
    println("right_transformed is:")
    right_transformed.prettyPrint(0)
    println("Calling simplify on transformed Seq")
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

    println("Inside transform() of ExpList")
    println("children list: ")
    for ((exp, i) <- children.get_list.zipWithIndex) {
      println("child %d".format(i))
      exp.prettyPrint(0)
    }
    if (children.get_list.isEmpty) {
      println("Children list is empty, returning (NoOP, ExpList())")
      StmtsExpList(NoOp(), ExpList(List[Exp]()))
    }
    else {
      println("children list not empty")
        children.get_list.head match {
          case x @ Call(_,_) => {
            println("head is Call Node")
            val t = temp.Temp()
            val head_result = Eseq(Move(Temp(t), x), Temp(t))
            println("head result is:")
            head_result.prettyPrint(0)
            
            println("children tail list is as follows:")
            println(children.get_list.tail)

            println("About to call transform on head_result::children.tail")
            val return_value = ExpList(head_result::children.get_list.tail).transform()
            println("About to return from calling transform() on the following ExpList:")
            this.prettyPrint(0)
            println("returned value is:")
            return_value.prettyPrint(0)
            return_value
        }
        case x => {
          println("head is not a Call node")
          println("head is:")
          x.prettyPrint(0)
          println("about to call transform on head")
          val head_result : Eseq = x.transform()
          println("Just returned from calling transform() on head of following ExpList")
          this.prettyPrint(0)
          println("head result is:")
          head_result.prettyPrint(0)

          println("About to call transform() on tail() of ExpList")
          val tail_result = ExpList(children.get_list.tail).transform()
          println("Just returned from transform() call of tail of following ExpList")
          this.prettyPrint(0)
          println("transformed tail result is:")
          tail_result.prettyPrint(0)

          println("testing for commuting")
          println("tail_result.stmts is:")
          tail_result.stmts.prettyPrint(0)
          println("head_result.exp is:")
          head_result.exp.prettyPrint(0)

          if (IR_Node.commute(tail_result.stmts, head_result.exp)) {
            println("tail_result.stmts and head_result.exp commute")
            val return_value = StmtsExpList(Seq(head_result.stmt, tail_result.stmts).simplify(),
              ExpList(head_result.exp::tail_result.exps.get_list));
            println("About to return the following:")
            return_value.prettyPrint(0)
            return_value
          }
          else {
            println("tail_result.stmts and head_result.exp dont't commute")
            val temporary = temp.Temp()
            val return_value = StmtsExpList(Seq(head_result.stmt, Seq(Move(Temp(temporary), head_result.exp),
              tail_result.stmts).simplify()).simplify(),
                ExpList(Temp(temporary)::tail_result.exps.get_list))
            println("About to return the following:")
            return_value.prettyPrint(0)
            return_value
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

