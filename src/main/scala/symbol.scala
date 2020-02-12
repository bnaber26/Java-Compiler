package symbol

import scala.collection.mutable.{HashMap, Set, ListBuffer}
import temp.{Temp, Label}

class VarInfo(val var_type : String, val var_size : Int){
  def get_type : String = var_type
  def get_size : Int = var_size
}
class ClassVarInfo(var_type : String, var_size : Int, val offset : Int) extends
    VarInfo(var_type, var_size) {
  def get_offset : Int = offset
}

class VarInfoMap {
  val var_to_info = HashMap[String, VarInfo]()

  def add(var_name : String, info : VarInfo) = {
    var_to_info(var_name) = info
  }

  def get(var_name : String) : Option[VarInfo] = {
    var_to_info.get(var_name)
  }

  def contains(var_name : String) : Boolean = {
    var_to_info.contains(var_name)
  }

  def values = var_to_info.values 

  def print = {
    println(var_to_info)
  }
}

class ClassVarInfoMap extends VarInfoMap {

  override def get(var_name : String) : Option[ClassVarInfo] = {
    var_to_info.get(var_name) match {
      case Some(info) => Some(info.asInstanceOf[ClassVarInfo])
      case None => None
    }
  }
}


class ClassSymbolTablesCollection(val tables : HashMap[String, ClassSymbolTable]) {
  def get_class_table(name : String) : ClassSymbolTable = {
    tables.get(name) match {
      case Some(t) => t
      case None => throw new Exception("no symbol table for class %s".format(name))
    }
  }

  def update_table(class_name : String, updatedTable : ClassSymbolTable) = {
    tables(class_name) = updatedTable
  }
}

trait SymbolTable {
  val parent_env : Option[SymbolTable]
  val vars_to_info = new VarInfoMap()
  val vars_values_table = new HashMap[String, Any]()
  val vars_to_temps = new HashMap[String, Temp]()
  val arr_to_length = new HashMap[String, Int]()

  val type_to_size = HashMap("int" -> 4, "bool" -> 4, "int[]" -> 4)

  // def add_method(name : String, symbol_table : SymbolTable)
  // def add_block(block_table : BlockSymbolTable)
  // def get_class_name() : String

  def print_table : Unit

  // Should probably use these as default methods in SymbolTable superclass
  def lookup_var_type(name : String) : Option[String] = {
    // Need to recursively walk the parent environment in case
    // we don't find the var_name in this Block's hash_map
    vars_to_info.get(name) match {
      case Some(t) => Some(t.get_type)
      case None => {
        parent_env match {
          case Some(p) => p.lookup_var_type(name)
          case None => None
        }
      }
    }
  }

  def lookup_vars_size(name : String) : Int = {
    vars_to_info.get(name) match {
      case Some(t) => t.get_size
      case None => {
        parent_env match {
          case Some(p) => p.lookup_vars_size(name)
          case None => throw new Exception("no size for variable of type %s".format(name))
        }
      }
    }
  }

  def lookup_var_value(name : String) : Any = {
    // Need to recursively walk the parent environment in case
    // we don't find the var_name in this Block's hash_map
    vars_values_table.get(name) match {
      case Some(t) => Some(t)
      case None => {
        parent_env match {
          case Some(p) => p.lookup_var_value(name)
          case None => None
        }
      }
    }
  }

  def lookup_temp(name : String) : Option[Temp] = {
    val temp_name = create_name(name)
    vars_to_temps.get(temp_name) match {
      case Some(t) => Some(t)
      case None => {
        parent_env match {
          case Some(par) => par.lookup_temp(name)
          case None => None
        }
      }
    }
  }

  def lookup_arr_length(arr_name : String) : Int = {
    arr_to_length.get(arr_name) match {
      case Some(l) => l
      case None => throw new Exception("Array %s not in arr_to_length table".format(arr_name))
    }
  }

  def add_arr_length(arr_name : String, length : Int) = {
    arr_to_length(arr_name) = length
  }

  def create_name(old_name : String) : String 

  def add_temp(name : String) : Temp = {
    val temp_name = create_name(name)
    val temp_var = Temp()
    vars_to_temps(temp_name) = temp_var
    temp_var
  }

  def add_var_type(name: String, var_type : String) = {
    if (vars_to_info.contains(name)) {
      // throw a type error here
      throw new Exception("variable %s already declared in scope".format(name))
    }

    val var_size = get_size(var_type)
    vars_to_info.add(name ,new VarInfo(var_type, var_size))
  }

  def add_var_value(name : String, value : Any) = {
    vars_values_table(name) = value
  }

  def get_size(type_name : String) : Int = {
    vars_to_info.get(type_name) match {
      case Some(info) => info.get_size
      case None => 4                 // must be an object reference
    }
  }

  def get_block(block_number : Int) : (SymbolTable, Int)

  def get_block_number : Int 

  def set_block_number(block_number : Int)

  def update_block_table(block_number : Int, new_table : BlockSymbolTable)

  def get_class_name : String

  def get_method_name : String

  def get_name_prefix : String

  def get_number_for_block(block : BlockSymbolTable) : Int

}

class ClassSymbolTable(name : String, val parent_env : Option[SymbolTable] = None) extends SymbolTable{
  val class_name = name
  val insertion_order = new ListBuffer[String]
  override val vars_to_info = new ClassVarInfoMap()
  val method_tables = HashMap[String, MethodSymbolTable]()
  var superclass = ""

  override def get_class_name : String = {
    class_name
  }

  override def print_table : Unit = {
    println("Class Symbol Table of class %s".format(name))
    println("method tables:")
    println(method_tables)
    println("vars to info")
    println(vars_to_info)
  }

  def get_fields_in_order : ListBuffer[String] = {
    insertion_order
  }

  def get_fields_in_reverse : ListBuffer[String] = {
    insertion_order.reverse
  }

  def get_parent_class_table : ClassSymbolTable = {
    parent_env match {
      case Some(t) => {assert(t.isInstanceOf[ClassSymbolTable]); t.asInstanceOf[ClassSymbolTable]}
      case None => throw new Exception("class symbol table has no parent class table")
    }
  }

  override def create_name(old_name: String): String = {
    class_name + "$" + old_name
  }

  def add_superclass(class_name : String) = {
    superclass = class_name
  }

  def get_superclass_name() : String = {
    superclass
  }

  def add_method(name : String, symbol_table : MethodSymbolTable) = {
    if (method_tables.contains(name)) {
      // throw some type error here 
    }
    method_tables(name) = symbol_table
  }

  def get_method_table(name : String) : MethodSymbolTable = {
    method_tables(name)
  }

  def lookup_vars_offset(name : String) : Option[Int] = {
    vars_to_info.get(name) match {
      case Some(t) => Some(t.get_offset)
      case None => {
        parent_env match {
          case Some(p) => p.asInstanceOf[ClassSymbolTable].lookup_vars_offset(name)
          case None => None
        }
      }
    }
  }

  override def add_var_type(name: String, var_type : String) = {
    if (vars_to_info.contains(name)) {
      // throw a type error here
      throw new Exception("variable %s already declared in scope".format(name))
    }
    insertion_order += name
    val var_size = get_size(var_type)
    val offset = get_offset(name)
    vars_to_info.add(name, new ClassVarInfo(var_type, var_size, offset))
  }

  def get_offset(name : String) : Int = {
    val insertion_index = insertion_order.indexOf(name) 
    // Get sizes of elements inserted prior to variable "name" and sum those up
    insertion_order.take(insertion_index).map(lookup_vars_size(_)).sum
  }

  def get_class_size : Int = {
    vars_to_info.values.map(x => x.get_size).sum
  }

  def updateMethodTable(method_name : String, newTable : MethodSymbolTable) = {
    method_tables(method_name) = newTable
  }

  override def get_block(block_number : Int) : (SymbolTable, Int) = {
    throw new Exception("get_block not implemented for ClassSymolTable")
  }

  override def get_block_number : Int = throw new Exception("get_block_number not implemented for ClassSymolTable")

  override def set_block_number(block_number : Int) = throw new Exception(
    "set_block_number not implemented for ClassSymolTable")


  override def update_block_table(block_number : Int, new_table : BlockSymbolTable) =
    throw new Exception("update_block_table not implemented for ClassSymbolTable")

  override def get_method_name : String = throw new Exception("get_method_name not implemented for ClassSymbolTable")

  override def get_name_prefix : String = get_class_name

  override def get_number_for_block(block : BlockSymbolTable) : Int =
    throw new Exception("get_number_for_block not implemented")
}

class MethodSymbolTable(val name : String, val parent_env : Option[ClassSymbolTable] = None) extends SymbolTable{
  val param_types = new HashMap[String, String]()
  val param_values = new HashMap[String, Any]()
  val param_names = new ListBuffer[String]()
  val blocks = new ListBuffer[BlockSymbolTable]()

  val code_label = parent_env match {
    case Some(p) => new temp.Label(p.get_class_name + "$" + name)
    case None => new temp.Label(name)
  }
  var return_type = ""
  var return_value : Any = new Object()
  var current_block_number = 0

  override def print_table : Unit = {
    println("Method Symbol Table of method %s$%s".format(get_class_name, name))
    parent_env match {
      case Some(env) => println("parent is %s".format(env.get_class_name))
      case None => throw new Exception("no parent symbol table for method symbol table")
    }
    println("vars_to_temp")
    println(vars_to_temps)
  }

  override def get_class_name : String = {
    parent_env match {
      case Some(parent_table) => {
        parent_table match{
          case table : ClassSymbolTable => table.get_class_name
          case _ => throw new Exception("MethodSymbolTable has no parent environment")
        }
        
      }
      case None => throw new Exception("MethodSymbolTable has to parent environment")
    }
  }

  def get_block(block_number : Int) : (SymbolTable, Int) = {
    (blocks(block_number), block_number + 1)
  }

  def get_block_number : Int = current_block_number

  def set_block_number(new_block_number : Int) = current_block_number = new_block_number

  def create_name(old_name: String): String = {
    val class_name = get_class_name
    class_name + "$" + name + "$" + old_name
  }

  def add_param_type(param_name : String, param_type : String) = {
    // Invariant:
    // order in which add_param_type is called on parameteres corresponds to
    // order of parameters in method declaration!!!

    if (param_types.contains(param_name)) {
      // need to throw a type error here
    }
    param_names += param_name
    param_types(param_name) = param_type
  }

  override def lookup_var_type(name : String) : Option[String] = {
    val var_type = super.lookup_var_type(name)
    var_type match {
      case Some(t) => Some(t)
      case None => {
        param_types.get(name) match {
          case Some(t) => Some(t)
          case _ => None
        }
      }
    }
  }

  def add_param_value(param_name : String, param_value : Any) = {
    param_values(param_name) = param_value
  }


  def add_return_type(return_t : String) = {
    if (return_type.nonEmpty) {
      // throw some type error
    }
    return_type = return_t
  }

  def add_return_value(return_v : Any) = {
    return_value = return_v
  }

  def add_block(block : BlockSymbolTable) = {
    blocks += block
  }

  def get_param_names() : List[String] = {
    param_names.toList
  }

  def get_param_insert_index(name : String) : Int = {
    param_names.indexOf(name)
  }

  /*
  def get_number_local_vars : Int = {

    // get local vars in all blocks
    val local_vars_in_blocks = blocks.map(b => b.get_number_local_vars).sum
  }
   */

  def get_number_params : Int = param_names.length

  override def update_block_table(block_number : Int, new_table : BlockSymbolTable) = {
    blocks(block_number) = new_table
  }

  override def get_method_name : String = name

  override def get_name_prefix : String = {
    val prefix = parent_env match {
      case Some(env) => env.get_name_prefix
      case _ => throw new Exception("MethodSymbolTable doesn't have a parent symbol table")
    }
    prefix + "$" + name
  }

  override def get_number_for_block(block : BlockSymbolTable) : Int = {
    blocks.indexOf(block)
  }

}

class BlockSymbolTable(val parent_env : Option[SymbolTable]) extends SymbolTable {
  var current_block_number = 0
  val blocks = new ListBuffer[BlockSymbolTable]()

  def add_block(block : BlockSymbolTable) = {
    blocks += block
  }

  def get_block(block_number : Int) : (SymbolTable, Int) = {
    (blocks(block_number), block_number + 1)
  }

  // def get_number_local_vars : Int = 

  def get_block_number : Int = current_block_number

  def set_block_number(new_block_number : Int) = current_block_number = new_block_number

  override def update_block_table(block_number : Int, new_table : BlockSymbolTable) = {
    blocks(block_number) = new_table
  }

  override def get_class_name : String = {
    parent_env match {
      case Some(parent_table) => parent_table.get_class_name
      case None => throw new Exception("BlockSymbolTable has no parent environment to get class name from")
    }
  }

  def create_name(old_name : String) : String = {
    get_name_prefix + "$" + old_name
  }

  def get_own_block_number : Int = {
    parent_env match {
      case Some(env) => env.get_number_for_block(this)
      case None => throw new Exception("Block Symbol Table doesn't have a parent table")
    }
  }

  override def get_number_for_block(block : BlockSymbolTable) : Int = {
    blocks.indexOf(block)
  }

  override def get_name_prefix : String = {
    val (prefix, own_block_number) = parent_env match {
      case Some(env) => {
        (env.get_name_prefix, env.get_number_for_block(this))
      }
      case None => throw new Exception("No parent symbol table from which to get prefix of name")
    }
    prefix + "Block$" + own_block_number.toString
  }

  override def get_method_name : String = {
    parent_env match {
      case Some(env) => env.get_method_name
      case None => throw new Exception("BlockSymbolTable doesn't have a parent symbol table")
    }
  }

  override def print_table = {
    println("Block Symbol table")
    println("vars_to_temps")
    println(vars_to_temps)
  }

}

