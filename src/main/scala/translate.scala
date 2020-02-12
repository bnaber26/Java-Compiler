package translate

import ir._
import temp.{Temp, Label}

abstract class ExpTranslate{
  def gen_Ex() : ir.Exp
  def gen_Nx() : ir.Stmt
  def gen_Cx(t : Label, f : Label) : Stmt
}

class Expression(val exp : ir.Exp) extends ExpTranslate{
  override def gen_Ex() : ir.Exp = {
    exp
  }

  override def gen_Nx() : ir.Stmt = {
    // Just a conversion to ir.Stmt, that's the whole purpose of ir.Expression class
    ir.Expression(exp)
  }

  override def gen_Cx(true_label : temp.Label, false_label : temp.Label) : ir.Stmt = {
    // Test if expression equals 0, if it does jump to right label, else to left label
    CJump("==", exp, Const(0), false_label, true_label)
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

