package block

import scala.collection.mutable.ListBuffer

case class BasicBlock(val stmts : ir.StmtList) {

  def get_label : ir.Label = {
    assert(stmts.head.isInstanceOf[ir.Label])
    stmts.head.asInstanceOf[ir.Label]
  }

  def get_jump : ir.Stmt = {
    assert(stmts.last.isInstanceOf[ir.CJump] || stmts.last.isInstanceOf[ir.Jump])
    stmts.last
  }

  def prepend_statement(stmt : ir.Stmt) : BasicBlock = {
    BasicBlock(stmt :: stmts)
  }

  def append_statement(stmt : ir.Stmt) : BasicBlock = {
    BasicBlock(stmts :+ stmt)
  }

  def get_first_statement : ir.Stmt = stmts.head

  def get_last_statement : ir.Stmt = stmts.last

  def prettyPrint() = {
    println("Block. Statements List for Block")
    stmts.prettyPrint(0)
    println()
  }

}
