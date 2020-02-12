package frame

import node.{MethodDecl, ParamList, ParamNode, StmtNode, Node}
import ir.{Exp, Move}
import codegen._
import symbol._
import temp.{Temp, Label}
import scala.collection.mutable.{HashMap, ListBuffer}

abstract class Frame {
  val name : Label
  val params : ListBuffer[String]
  // We use SymbolTable here to account for variable name duplication in nested blocks
  val name_to_temp : HashMap[SymbolTable, HashMap[String, temp.Temp]]
  val word_size : Int

  def add_temp(var_name : String, symbol_table : SymbolTable) : temp.Temp
  def get_temp(var_name : String, symbol_table : SymbolTable) : temp.Temp
  def add_block(block_table : BlockSymbolTable) 

  def get_name_to_temp_for_table(table : SymbolTable) : HashMap[String, temp.Temp] = {
    name_to_temp(table)
  }

  def get_name : String = name.get_name
}

class Mips(val symbol_table : MethodSymbolTable) extends Frame {
  // Note: Frames will be created on each function call. The instructions
  // for a method declaration will be created prior to that
  // What we will need to do here is write a function that creates the
  // code for the prologue and epilogue, store the label to the translated
  // function body, map the arg expressions to their corresponding
  // Temp instances (can use the symbol table for that), create the move
  // statements to move the args into those Temps (i.e. registers and on stack)
  // and then jump to the prologue

  // NOTE: WHEN I GENERATE THE CODE FOR THE METHOD, I NEED TO USE code_label in method symbol table
  // as LABEL

  val name = symbol_table.code_label
  val params = ListBuffer[String]()
  val fp : Temp = temp.Temp("$fp")
  val sp : Temp = temp.Temp("$sp")
  val word_size = 4

  // This map is used to account for variables with the same names in blocks, which have
  // their own symbol tables
  val name_to_temp = HashMap[SymbolTable, HashMap[String, temp.Temp]](
    symbol_table -> HashMap[String, temp.Temp]())

  def return_value_register() : Temp = {
    // This Temp needs to correspond to $r4
    Temp("$r4")
  }

  def frame_pointer : temp.Temp = fp

  def stack_pointer : temp.Temp = sp

  /*
  def alloc_params(params : List[params : ParamList]) : HashMap[symbol.Node, Access] = {
    // first four parameters into registers (InReg instances)
    // other parameters onto stack (InFrame instances)
    val (in_regs, on_stack)  = params.decls.splitAt(4)
    val param_to_access = mutable.HashMap[symbol.Node, Access]()

    // Registers 4 to 7 for first four parameters
    in_regs.zipWithIndex.map((p,i) => param_to_access(p) = InReg(4 + i))
  }
   */

  def add_params(params_list : node.ParamList, symbol_table : SymbolTable) = {
    for (param <- params_list.decls) {
      val param_name = param.get_name
      add_param(param_name, symbol_table)
      params += param_name
    }
  }

  def add_param(param_name : String, symbolTable : SymbolTable) : temp.Temp = {
    assert(symbolTable.asInstanceOf[MethodSymbolTable].param_names.contains(param_name))

    val tempVar = temp.Temp()
    name_to_temp.get(symbolTable) match {
      case Some(tab) => {
        // add var_name to Temp instance mapping
        tab(param_name) = tempVar
        tempVar
      }
      case _ => throw new Exception("no valid symbol table in add_param()")
    }
  }

  def add_local_vars(var_decls : node.VarDeclList, symbol_table : SymbolTable) = {
    println("about to add local_vars to stack frame")
    for (var_decl <- var_decls.decls) {
      add_temp(var_decl.get_name(), symbol_table)
    }
  }

  def add_temp(var_name : String, sym_tab : SymbolTable) : temp.Temp = {
    println("about to add temp for variable %s to name_to_temp".format(var_name))
    val temp_var = temp.Temp()
    name_to_temp.get(sym_tab) match {
      case Some(tab) => {tab(var_name) = temp_var; temp_var}
      case None => throw new Exception("symbol table not in name_to_temp in stack_frame")
    }
  }

  def get_temp(var_name : String, sym_tab : SymbolTable) : temp.Temp = {
    name_to_temp.get(sym_tab) match {
      case Some(table) => {
        table.get(var_name) match {
          case Some(t) => t
          case None => {
            sym_tab.parent_env match {
              case Some(env) => get_temp(var_name, env)
              case None => throw new Exception("No Temp instance for variable %s".format(var_name))
            }
          }
        }
      }
      case None => throw new Exception("No symbol table in name_to_temp in stack_frame for variable %s".format(
      var_name))
    }
  }

  override def add_block(block : BlockSymbolTable) = {
    name_to_temp(block) = HashMap[String, temp.Temp]()
  }

  

}
