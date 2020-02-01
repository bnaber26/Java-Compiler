package node

import scala.collection.Iterator

abstract class Node

case class Program(main : MainNode, decl_list : ClassDeclList) extends Node

case class MainNode(id1 : IdNode, id2: IdNode, stmt: StmtNode) extends Node 

sealed abstract class ClassDecl(val id : IdNode, val vars_decl_list : VarDeclList,
         val method_decls_list : MethodDeclsList) extends Node
case class ClassDeclSimple(override val id : IdNode, override val vars_decl_list : VarDeclList,
        override val method_decls_list : MethodDeclsList) extends ClassDecl(id, vars_decl_list, method_decls_list)

case class ClassDeclExtends(override val id : IdNode, id2 : IdNode, override val vars_decl_list : VarDeclList,
        override val method_decls_list : MethodDeclsList) extends ClassDecl(id, vars_decl_list, method_decls_list) 


abstract class DeclNode
case class VarDecl(t : TypeNode, id : IdNode) extends DeclNode {
  def get_name() : String = id.s
}

case class MethodDecl(t : TypeNode, id : IdNode, param_list : ParamList,
  vars : VarDeclList, stmts : StmtList, exp : ExpNode) extends DeclNode {
  def get_name : String = id.get_name()
}

case class ParamNode(t : TypeNode, id : IdNode) extends DeclNode {
  def get_name : String = id.get_name()
}


abstract class TypeNode extends Node
case class IntArrayType() extends TypeNode

case class BooleanType() extends TypeNode

case class IntegerType() extends TypeNode

case class IdentifierType(name : String) extends TypeNode 


sealed abstract class StmtNode extends Node
case class BlockNode(stmts : StmtList) extends StmtNode

case class IfNode(exp : ExpNode, stmt1 : StmtNode, stmt2 : StmtNode) extends StmtNode 

case class WhileNode(exp : ExpNode, stmt : StmtNode) extends StmtNode

case class PrintNode(exp : ExpNode) extends StmtNode

case class AssignNode(id : IdNode, exp : ExpNode) extends StmtNode

case class ArrayAssignNode(id : IdNode, exp1 : ExpNode, exp2 : ExpNode) extends StmtNode 


sealed abstract class ExpNode(val line_num : Int) extends Node
case class AndNode(exp1 : ExpNode, exp2 : ExpNode, override val line_num : Int) extends ExpNode(line_num)

case class LessThanNode(exp1 : ExpNode, exp2 : ExpNode, override val line_num : Int) extends ExpNode(line_num) 

case class PlusNode(exp1 : ExpNode, exp2 : ExpNode, override val line_num : Int) extends ExpNode(line_num) 

case class MinusNode(exp1 : ExpNode, exp2 : ExpNode, override val line_num : Int) extends ExpNode(line_num) 

case class TimesNode(exp1 : ExpNode, exp2 : ExpNode, override val line_num : Int) extends ExpNode(line_num)

case class ArrayLookupNode(exp1 : ExpNode, exp2 : ExpNode,
  override val line_num : Int) extends ExpNode(line_num) {
  def get_arr_exp() : ExpNode = exp1
  def get_index_exp() : ExpNode = exp2
}


case class ArrayLengthNode(exp : ExpNode, override val line_num : Int) extends ExpNode(line_num) 

case class CallNode(exp : ExpNode, id : IdNode, exp_list : ExpList,
  override val line_num : Int) extends ExpNode(line_num)

case class IntLiteralNode(i : Int, override val line_num : Int) extends ExpNode(line_num) 

case class TrueNode(override val line_num : Int) extends ExpNode(line_num) 

case class FalseNode(override val line_num : Int) extends ExpNode(line_num)

case class IdExpNode(s : String, override val line_num : Int) extends ExpNode(line_num) {
  def get_name : String = s
}

case class ThisNode(override val line_num : Int) extends ExpNode(line_num) 

case class ArrayNode(exp : ExpNode, override val line_num : Int) extends ExpNode(line_num) 

case class ObjectNode(id : IdNode, override val line_num : Int) extends ExpNode(line_num) {
  def get_name : String = id.get_name()
}

case class NotNode(exp : ExpNode, override val line_num : Int) extends ExpNode(line_num) 


case class IdNode(s : String, line_num : Int) extends Node {
  def get_name() : String = s
}

// TO-DO: DEFINE AN ITERATOR FOR EACH OF THESE LIST CLASSES AND USE THEM!!!


case class ClassDeclList(decls : List[ClassDecl]) extends Node

// ATTRIBUTE OF EXP_LIST SHOULDNT BE CALLED DECLS!!!
case class ExpList(decls : List[ExpNode])extends Node


case class ParamList(decls : List[ParamNode])extends Node


case class MethodDeclsList(decls : List[MethodDecl]) extends Node


case class StmtList(decls : List[StmtNode]) extends Node


case class VarDeclList(decls : List[VarDecl]) extends Node



