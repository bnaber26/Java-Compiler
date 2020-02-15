package typecheck

import node._
import parser.AST
import symbol.{SymbolTable, ClassSymbolTable, MethodSymbolTable, BlockSymbolTable}
import scala.collection.mutable.{HashMap}

class TypeChecker(val syntax_tree : AST) {
  val class_symbol_tables = new HashMap[String, ClassSymbolTable]()
  var main_symbol_table : Option[MethodSymbolTable] = None
  create_symbol_tables()


  def get_symbol_tables = class_symbol_tables

  def get_main_symbol_table = main_symbol_table match {
    case Some(tab) => tab
    case None => throw new Exception("Main Symbol Table doesn't exist")
  }

  def add_class(name : String, table : ClassSymbolTable) = {
    if (class_symbol_tables.contains(name)) {
      // throw a type error here
    }
    class_symbol_tables(name) = table
  }

  def create_symbol_tables() = {
    eval_program((syntax_tree.program_node))
  }

  def type_check() : Boolean = {
    // Need a check method for each node class, then check whether
    // expressions correspond to parameter types and variable types
    check_ast(syntax_tree)
  }

  def output_type_error(actual : String, expected : String, line_num : Int) = {
    println("Type error on line %d. Found type %s, but expected type %s".format(
                 line_num, actual, expected))
  }

  def eval_program(program : Program) = {
    eval_main(program.main)
    eval_class_decls_list(program.decl_list)
  }

  def eval_main(main : MainNode) = {
    val method_name = eval_id(main.id1)
    val args_array_name = eval_id(main.id2)

    // Main doesn't need a ClassSymbolTable, only a MethodSymbolTable
    val main_table = new MethodSymbolTable(method_name)

    main_table.add_param_type(args_array_name, "String[]")
    main_table.add_return_type("void")

    main_symbol_table = Some(main_table)

    eval_stmt(main.stmt, main_table)
  }

  def eval_class_decls_list(class_decls : ClassDeclList) = {
    class_decls.decls.map(eval_class_decl(_))
  }

  def eval_class_decl(class_decl : ClassDecl) = {
    class_decl match {
      case ClassDeclSimple(id, vars_decl, method_decls) => {
        val class_name = eval_id(id)

        // Create a new class symbol table for this class
        val class_table = new ClassSymbolTable(class_name)

        // Add class variable declarations to symbol table
        eval_var_decls_list(vars_decl, class_table)

        // Add method declarations to symbol table
        eval_method_decls_list(method_decls, class_table)

        // Add class symbol table to global symbol table
        class_symbol_tables(class_name) = class_table
      }
      case ClassDeclExtends(id1, id2, vars_decl, method_decls) => {
        val class_name = eval_id(id1)
        val superclass_name = eval_id(id2)

        // Create a new class symbol table for this class
        val class_table = new ClassSymbolTable(class_name)

        class_table.add_superclass(superclass_name)

        // Add class variable declarations to symbol table
        eval_var_decls_list(vars_decl, class_table)

        // Add method declarations to symbol table
        eval_method_decls_list(method_decls, class_table)

        // Add class symbol table to global symbol table
        class_symbol_tables(class_name) = class_table
      }
    }
  }

  def eval_var_decls_list(var_decls : VarDeclList, symbol_table : SymbolTable) = {
    var_decls.decls.map(x => eval_var_decl(x, symbol_table))
  }

  def eval_var_decl(var_decl: VarDecl, symbol_table : SymbolTable) = {
    val var_type = eval_type(var_decl.t)
    val var_name = eval_id(var_decl.id)

    symbol_table.add_var_type(var_name, var_type)
  }

  def eval_method_decls_list(method_decls : MethodDeclsList, class_symbol_table : ClassSymbolTable) = {
    method_decls.decls.map(x => eval_method_decl(x, class_symbol_table))
  }

  def eval_method_decl(method_decl : MethodDecl, class_symbol_table : ClassSymbolTable) = {
    val return_type = eval_type(method_decl.t)
    val name = eval_id(method_decl.id)

    val method_symbol_table = new MethodSymbolTable(name, Some(class_symbol_table))

    eval_param_list(method_decl.param_list, method_symbol_table)
    eval_var_decls_list(method_decl.vars, method_symbol_table)
    eval_stmts_list(method_decl.stmts, method_symbol_table)
    method_symbol_table.add_return_type(return_type)

    class_symbol_table.add_method(name, method_symbol_table)
  }

  def eval_param_list(param_list : ParamList, method_symbol_table : MethodSymbolTable) = {
    // Invariant:
    // eval_param must be called in the same order in which parameters are declared in
    // method declaration 
    param_list.decls.map(x => eval_param(x, method_symbol_table))
  }

  def eval_param(param : ParamNode, method_symbol_table : MethodSymbolTable) = {
    val param_type = eval_type(param.t)
    val param_name = eval_id(param.id)

    method_symbol_table.add_param_type(param_name, param_type)
  }

  def eval_stmts_list(stmts : StmtList, symbol_table : SymbolTable) = {
    stmts.decls.map(x => eval_stmt(x, symbol_table))
  }

  def eval_stmt(stmt: StmtNode, symbol_table : SymbolTable) : Unit = {
    
    stmt match {
      case BlockNode(stmts) => {
        val block_symbol_table = new BlockSymbolTable(Some(symbol_table))
        eval_stmts_list(stmts, block_symbol_table)
        add_block_symbol_table(symbol_table, block_symbol_table)
      }
      case IfNode(exp, stmt1, stmt2) => {
        val block_symbol_table_then = new BlockSymbolTable(Some(symbol_table))
        val block_symbol_table_else = new BlockSymbolTable(Some(symbol_table))

        // Note: Don't have to evaluate the exp here, only in second pass

        // ACTUALLY NEED TO CREATE TWO BLOCK SYMBOL TABLES HERE, ONE FOR THEN-CLAUSE
        // AND ONE FOR ELSE-CLAUSE
        
        eval_stmt(stmt1, block_symbol_table_then)
        eval_stmt(stmt2, block_symbol_table_else)

        add_block_symbol_table(symbol_table, block_symbol_table_then)
        add_block_symbol_table(symbol_table, block_symbol_table_else)
      }
      case WhileNode(exp, stmt) => {
        val block_symbol_table = new BlockSymbolTable(Some(symbol_table))

        eval_stmt(stmt, block_symbol_table)

        add_block_symbol_table(symbol_table, block_symbol_table)
      }
      case AssignNode(id, exp) => {
        // record the length of Arrays in symbol table
        exp match {
          case ArrayNode(length_exp, _) => {
            val length = length_exp match {
              case IntLiteralNode(i, _) => {
                // Store length of array in symbol table
                symbol_table.add_arr_length(id.get_name(), i)
              }
              case _ => {}
            }
          }
          case _ => {}
        }
      }
      case _ => {
        // No variable or method declarations in other StmtNodes
      }
    }
  }

  def eval_type(t : TypeNode) : String = {
    t match {
      case IntArrayType() => "int[]"
      case BooleanType() => "bool"
      case IntegerType() => "int"
      case IdentifierType(s) => s
      case _ => throw new Exception("type of Type node doesn't exist")
    }
  }

  def eval_id(id : IdNode) : String = {
    id.s
  }

  // Type Checking Helper Functions

  def check_ast(ast : AST) : Boolean = {
    val program = ast.program_node
    val main_valid = check_main(program.main)
    val class_decls_valid = check_class_decls_list(program.decl_list)
    main_valid && class_decls_valid
  }

  def check_main(main : MainNode) : Boolean = {
    val main_name = eval_id(main.id1)
    val symbol_table = main_symbol_table match {
      case Some(t) => t
      case None => throw new Exception("No Symbol table for main method")
    }
    check_stmt(main.stmt, symbol_table)
  }

  def check_class_decls_list(class_decls : ClassDeclList) : Boolean = {
    val checks_list = class_decls.decls.map(check_class_decl(_))
    checks_list.foldRight(true)((Cons, Cas) => Cons && Cas)
  }

  def check_class_decl(class_decl : ClassDecl) : Boolean = {
    class_decl match {
      case ClassDeclSimple(id, var_decls_list, method_decl_list) => {
        val symbol_table = class_symbol_tables.get(eval_id(id))
        symbol_table match {
          case Some(table) => {
            check_method_decls_list(method_decl_list, table)
          }
          case None => throw new Exception("No symbol table for class %s".format(eval_id(id)))
        }
      }
      case ClassDeclExtends(id1, id2, var_decls_list, method_decl_list) => {
        val symbol_table = class_symbol_tables.get(eval_id(id1))
        symbol_table match {
          case Some(table) => {
            check_method_decls_list(method_decl_list, table)
          }
          case None => throw new Exception("No symbol table for class %s".format(eval_id(id1)))
        }
      }
    }
  }

  def check_method_decls_list(method_decls : MethodDeclsList, symbol_table : ClassSymbolTable) : Boolean = {
    val method_checks = method_decls.decls.map(x => check_method_decl(x, symbol_table))
    method_checks.foldRight(true)((Cons, Cas) => Cons && Cas)
  }

  def check_method_decl(method_decl : MethodDecl, symbol_table : ClassSymbolTable) : Boolean = {
    val return_type = eval_type(method_decl.t)
    val method_name = eval_id(method_decl.id)
    val method_symbol_table = symbol_table.get_method_table(method_name)

    val stmts_check = check_stmt_list(method_decl.stmts, method_symbol_table)
    val return_type_check = check_exp(method_decl.exp, List[String](return_type), method_symbol_table)
    stmts_check && return_type_check
  }

  def check_stmt(stmt: StmtNode, symbol_table : SymbolTable) : Boolean = {
    stmt match {
      case BlockNode(stmts) => {
        check_stmt_list(stmts, symbol_table)
      }
      case IfNode(exp, stmt1, stmt2) => {
        val exp_valid = check_exp(exp, List[String]("bool"), symbol_table)
        val stmt1_valid = check_stmt(stmt1, symbol_table)
        val stmt2_valid = check_stmt(stmt2, symbol_table)
        exp_valid && stmt1_valid && stmt2_valid
      }
      case WhileNode(exp, stmt) => {
        val exp_valid = check_exp(exp, List[String]("bool"), symbol_table)
        val stmt_valid = check_stmt(stmt, symbol_table)
        exp_valid && stmt_valid
      }
      case PrintNode(exp) => {
        check_exp(exp, List[String]("int", "bool"), symbol_table)
      }
      case AssignNode(id, exp) => {
        val var_type = symbol_table.lookup_var_type(eval_id(id))
        var_type match {
          case Some(t) => check_exp(exp, List[String](t), symbol_table)
          case None => throw new Exception("cannot find variable %s in symbol table".format(eval_id(id)))
        }
      }
      case ArrayAssignNode(id, exp1, exp2) => {
        val check1 = check_exp(exp1, List[String]("int"), symbol_table)
        val check2 = check_exp(exp2, List[String]("int"), symbol_table)

        check1 && check2
      }
    }
  }

  def check_stmt_list(stmts : StmtList, symbol_table : SymbolTable) : Boolean = {
    val checks_list = stmts.decls.map(x => check_stmt(x, symbol_table))
    checks_list.foldRight(true)((Cons, Cas) => Cons && Cas)
  }

  def check_exp(exp : ExpNode, expected : List[String] = List[String](), symbol_table : SymbolTable) : Boolean = {
    exp match {
      case AndNode(exp1 : ExpNode, exp2 : ExpNode, l) => {
        val check1 = check_exp(exp1, List[String]("true", "false"), symbol_table)
        val check2 = check_exp(exp2, List[String]("true", "false"), symbol_table)

        check1 && check2
      }
      case LessThanNode(exp1 : ExpNode, exp2 : ExpNode, l) => {
        val check1 = check_exp(exp1, List[String]("int"), symbol_table)
        val check2 = check_exp(exp2, List[String]("int"), symbol_table)

        check1 && check2
      }
      case PlusNode(exp1 : ExpNode, exp2 : ExpNode, l) => {
        val check1 = check_exp(exp1, List[String]("int"), symbol_table)
        val check2 = check_exp(exp2, List[String]("int"), symbol_table)

        check1 && check2
      }
      case MinusNode(exp1 : ExpNode, exp2 : ExpNode, l) => {
        val check1 = check_exp(exp1, List[String]("int"), symbol_table)
        val check2 = check_exp(exp2, List[String]("int"), symbol_table)

        check1 && check2
      }
      case TimesNode(exp1 : ExpNode, exp2 : ExpNode, l) => {
        val check1 = check_exp(exp1, List[String]("int"), symbol_table)
        val check2 = check_exp(exp2, List[String]("int"), symbol_table)

        check1 && check2
      }
      case ArrayLookupNode(exp1 : ExpNode, exp2 : ExpNode, l) => {
        val check1  = exp1 match {
          case IdExpNode(s, line_num) => {
            val var_type = symbol_table.lookup_var_type(s)
            var_type match {
              case Some(t) => {
                if (t != "int[]") {
                  output_type_error(t, "int[]", line_num)
                  return false
                }
                true
              }
              case None => throw new Exception("cannot find variable %s in symbol table".format(s))
            }
          }
          case _ => {output_type_error(exp1.getClass().toString, "int", l); false} 
        }
        val check2 = exp2 match {
          case IdExpNode(s, line_num) => {
            val var_type = symbol_table.lookup_var_type(s)
            var_type match {
              case Some(t) => {
                if (t != "int") {
                  output_type_error(t, "int", line_num)
                  return false
                }
                true
              }
              case None => throw new Exception("cannot find variable %s in symbol table".format(s))
            }
          }
          case IntLiteralNode(_, _) => true
          case _ => false
        }
        check1 && check2
      }
      case ArrayLengthNode(exp, _) => {
        exp match {
          case ArrayNode(_, _) => true
          case IdExpNode(s, line_num) => {
            // Need to propagte this to check_id_exp_node for consistency reasonws!!!
            val var_type = symbol_table.lookup_var_type(s)
            var_type match {
              case Some(t) => {
                if (t != "int") {
                  output_type_error(t, "int", line_num)
                  return false
                }
                true
              }
              case None => throw new Exception("cannot find variable %s in symbol table".format(s))
            }
          }
          case _ => {output_type_error(exp.getClass().toString(), "int or int[]", exp.line_num); false}
        }
      }
      case CallNode(exp, id, exp_list, line_num) => {
        // First check whether exp corresponds to some class name
        val class_symbol_table = exp match {
          case IdExpNode(s, _) => class_symbol_tables.get(s)
          case ObjectNode(s, _) => class_symbol_tables.get(eval_id(s))
          case ThisNode(_) => symbol_table.parent_env
          case _ => None
        }
        class_symbol_table match {
          case Some(table) => {
            // get table for method
            val method_name = eval_id(id)
            val method_table = table.asInstanceOf[ClassSymbolTable].get_method_table(method_name)

            val return_type = method_table.return_type

            val params_check = check_param_list(exp_list, method_table)
            val return_check = {
              if (!expected.contains(method_table.return_type)){
                output_type_error(return_type, expected.mkString(" or "), line_num)
                false
              }
              else true
            }
            params_check && return_check
          }
          case None => throw new Exception("no class symbol table in CallNode, exp is %s".format(
                exp.getClass().toString()))
        }
      }
      case IntLiteralNode(i, line_num) => {
        if (!expected.contains("int")){
          output_type_error("int", expected(0), line_num)
          return false
        }
        true
      }
      case TrueNode(line_num) => {
        if (!expected.contains("bool")){
          output_type_error("bool", expected(0), line_num)
          return false
        }
        true
      }
      case FalseNode(line_num) => {
        if (!expected.contains("bool")){
          output_type_error("bool", expected(0), line_num)
          return false
        }
        true
      }
      case IdExpNode(s, line_num) => {
        val var_type = symbol_table.lookup_var_type(s)
            var_type match {
              case Some(t) => {
                if (t != "int") {
                  output_type_error(t, "int", line_num)
                  return false
                }
                true
              }
              case None => throw new Exception("cannot find variable %s in symbol table".format(s))
            }
      }
      case ThisNode(line_num) => {
        val class_name = symbol_table.asInstanceOf[ClassSymbolTable].get_class_name
        if (!expected.contains(class_name)){
          output_type_error(class_name, expected(0), line_num)
          return false
        }
        true
      }
      case ArrayNode(exp, _) => {
        check_exp(exp, List[String]("int"), symbol_table)
      }
      case ObjectNode(id, line_num) => {
        if (!expected.contains(eval_id(id))){
          output_type_error(eval_id(id), expected(0), line_num)
          return false
        }
        true
      }
      case NotNode(exp, _) => {
        check_exp(exp, List[String]("bool"), symbol_table)
      }
      case _ => throw new Exception("Unexpected ExpNode in check_exp")
    }
  }

  def check_param_list(params : ExpList, symbol_table : MethodSymbolTable) : Boolean = {
    assert(symbol_table.isInstanceOf[MethodSymbolTable])

    // Get list of parameter expressions from params.decls and expected parameter
    // types from symbol_table, zip both lists and check whether each pair of types
    // matches

    val checked_list = params.decls.zip(symbol_table.get_param_names()).map{ case (arg, param) => 
      check_exp(arg, List[String](
        symbol_table.lookup_var_type(param) match {
          case Some(t) => t;
          case _ => throw new Exception("no parameter type found")}
      ), symbol_table)}
    checked_list.foldRight(true)((cons, rest) => cons && rest)
  }

  def check_id(id : IdNode, expected: List[String], symbol_table : SymbolTable) : Boolean = {
    val var_name = eval_id(id)
    val var_type = symbol_table.lookup_var_type(var_name)
    var_type match {
      case Some(t) => {
        if (!expected.contains(t)) {
          output_type_error(t, expected(0), id.line_num)
          return false
        }
        true
      }
      case _ => throw new Exception("cannot find variable %s in symbol table".format(var_name))
    }
  }

  def add_block_symbol_table(dst_table : SymbolTable, block : BlockSymbolTable) = {
    if (dst_table.isInstanceOf[MethodSymbolTable]) {
        dst_table.asInstanceOf[MethodSymbolTable].add_block(block)
    }
    else if (dst_table.isInstanceOf[BlockSymbolTable]) {
        dst_table.asInstanceOf[BlockSymbolTable].add_block(block)
    }
    else {
        throw new Exception("Cannot add block in this environment")
    }
  }

}
