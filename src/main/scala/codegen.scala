package codegen

import parser.{AST}
import ir._
import node._
import fragment.{Fragment}
import frame.{Frame, Mips}
import temp.{Label, Temp}
import symbol.{MethodSymbolTable, SymbolTable, ClassSymbolTable, ClassSymbolTablesCollection, BlockSymbolTable}
import scala.collection.mutable.{ListBuffer, HashMap}

abstract class CodeGenerator(val ast : AST, class_symbol_tables_list : HashMap[String, ClassSymbolTable],
  val main_symbol_table : MethodSymbolTable) {

  val class_symbol_tables = new ClassSymbolTablesCollection(class_symbol_tables_list)
  val fragments : ListBuffer[Fragment]

  val index_bounds_exception_label : temp.Label 

  def create_seq(stmts_code : List[ir.Stmt]) : ir.Stmt = {
    val stmt_code = stmts_code(0)
    val new_stmts_code = stmts_code.drop(1)
    if (new_stmts_code.nonEmpty) {
      return ir.Seq(stmt_code, create_seq(new_stmts_code))
    }
    stmt_code
  }

}

class MipsCodeGenerator(ast : AST, class_symbol_tables_list : HashMap[String, ClassSymbolTable],
  main_symbol_table : MethodSymbolTable) extends CodeGenerator(ast, class_symbol_tables_list, main_symbol_table) {

  val index_bounds_exception_label : temp.Label = temp.Label("ArrayBoundsException")
  val fragments = ListBuffer[Fragment]()

  // General implementation idea:
  // We will create a code_gen method for each type of node.Node. This will
  // enable us to call a walk() function on the AST, where we recursively
  // call code_gen on each node. This will prevent unnecessary code duplication
  // as we had in the type checking code

  // Statements

  def get_code_and_fragments : (ir.Stmt, ListBuffer[Fragment]) = code_gen(ast.program_node)

  def code_gen(program : node.Program) : (ir.Stmt, ListBuffer[Fragment])= {

    // code_gen for main and class declarations

    // Need to first process the method declarations in order to update the symbol tables
    // with information about Temps
    code_gen(program.decl_list)
    val main_code = code_gen(program.main)

    (main_code, fragments)
  }

  def code_gen(main : MainNode) : ir.Stmt = {
    // Create a stack frame for main function

    val stack_frame = new frame.Mips(main_symbol_table)

    val (main_code, symbol_table, new_stack_frame) = code_gen(main.stmt, main_symbol_table, stack_frame)

    main_code
  }

  def code_gen(class_decls : ClassDeclList) : Unit = {

    // Note: The class declarations don't return any IR Code, we only have to traverse them in order
    // to get the code in the method declarations

    // -> NO!!! Don't think thats correct, we need to get the code for the method declarations!!!
    for (class_decl <- class_decls.decls) {
      val class_name = class_decl.id.get_name()
      val updatedSymbolTable = class_decl match {
        case x @ ClassDeclSimple(_,_,_) => code_gen(x)
        case x @ ClassDeclExtends(_,_,_,_) => code_gen(x)
      }
      class_symbol_tables.update_table(class_name, updatedSymbolTable.asInstanceOf[ClassSymbolTable])
    }
  }

  /*
  def code_gen(class_decl : node.ClassDecl) : ClassSymbolTable = {
    class_decl match {
      case x @ ClassDeclSimple(_,_,_) => code_gen(x.asInstanceOf[ClassDeclSimple])
      case x @ ClassDeclExtends(_,_,_,_) => code_gen(x.asInstanceOf[ClassDeclExtends])
    }
  }
   */

  def code_gen(class_decl : ClassDeclSimple) : SymbolTable = {

    val class_name = class_decl.id.get_name()
    val classSymbolTable = class_symbol_tables.get_class_table(class_name)

    // Generate code for the method declarations
    for (method_decl <- class_decl.method_decls_list.decls) {
      val method_name = method_decl.id.get_name()

      val (fragment, updatedSymbolTable) = code_gen(method_decl, classSymbolTable)
      fragments += fragment
      classSymbolTable.updateMethodTable(method_name, updatedSymbolTable)
    }

    classSymbolTable
  }

  def code_gen(class_decl : ClassDeclExtends) : SymbolTable = {

    val class_name = class_decl.id.get_name()
    val classSymbolTable = class_symbol_tables.get_class_table(class_name)

    // Generate code for the method declarations
    for (method_decl <- class_decl.method_decls_list.decls) {
      val method_name = method_decl.id.get_name()

      val (fragment, updatedSymbolTable) = code_gen(method_decl, classSymbolTable)
      fragments += fragment
      classSymbolTable.updateMethodTable(method_name, updatedSymbolTable)
    }

    classSymbolTable
  }

  def code_gen(method_decl : MethodDecl, symbol_table : ClassSymbolTable) :
      (Fragment, MethodSymbolTable) = {


    val methodName = method_decl.id.get_name()
    val methodSymbolTable : MethodSymbolTable = symbol_table.get_method_table(methodName)
    val newStackFrame = new Mips(methodSymbolTable)

    //val newStackFrame = new Mips(methodSymbolTable.asInstanceOf[MethodSymbolTable])

    newStackFrame.add_params(method_decl.param_list, methodSymbolTable)
    newStackFrame.add_local_vars(method_decl.vars, methodSymbolTable)

    val num_aux_temp = newStackFrame.get_temp("num_aux", methodSymbolTable)

    val (codeMethodStmts, newMethodSymbolTable, updated_stack_frame) = code_gen(method_decl.stmts,
                                                          methodSymbolTable, newStackFrame)

    // Side effect
    symbol_table.updateMethodTable(methodName, newMethodSymbolTable.asInstanceOf[MethodSymbolTable])

    // Get the return type
    val code_return_exp = code_gen_exp(method_decl.exp,
      newMethodSymbolTable, updated_stack_frame)

    symbol_table.updateMethodTable(methodName, newMethodSymbolTable.asInstanceOf[MethodSymbolTable])

    val codeMethodBody = ir.Eseq(codeMethodStmts, code_return_exp)

    val frag = new Fragment(updated_stack_frame, codeMethodBody)

    (frag, newMethodSymbolTable.asInstanceOf[MethodSymbolTable])
  }

  def code_gen(stmt_list : node.StmtList, symbol_table : SymbolTable, stack_frame : Frame) :
      (ir.Stmt, SymbolTable, Frame) = {
    
    // Need to return intermediate code of the form Seq(..., Seq(...), ...) if there are
    // multiple statements in stmt_list
    // We first generate code for each statement in stmt_list and then create the Seq() result

    val stmt_code_list = ListBuffer[ir.Stmt]()
    val stack_frames = ListBuffer[Frame](stack_frame)
    val symbol_tables = ListBuffer[SymbolTable](symbol_table)
    for (stmt <- stmt_list.decls) {
      val (code_stmt, new_symbol_table : SymbolTable, new_frame : frame.Frame) = code_gen(
        stmt, symbol_tables.last, stack_frames.last)
      stack_frames += new_frame
      symbol_tables += new_symbol_table
      stmt_code_list += code_stmt
    }
    val code_seqs = create_seq(stmt_code_list.toList)
    (code_seqs, symbol_table, stack_frame)
  }

  def code_gen(stmt : node.StmtNode, symbol_table : SymbolTable, stack_frame : Frame) :
      (ir.Stmt, SymbolTable, Frame) = {

    // Need to pattern match in order to avoid method overload error in code_gen(MainNode)
    // whose stmt field is of type node.StmtNode
    stmt match {
      case x @ node.BlockNode(_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.IfNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.WhileNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.PrintNode(_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.AssignNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.ArrayAssignNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
    }
  }

  def code_gen(block : node.BlockNode, symbol_table : SymbolTable, stack_frame : Frame) :
      (ir.Stmt, SymbolTable, Frame) = {

    def code_gen_block(symbolTable : SymbolTable) = {
      // Increase block number and get the correct BlockSymbolTable
      val current_block_number = symbolTable.get_block_number
      val (block_symbol_table, new_block_number) = symbolTable.get_block(current_block_number)
      // SIDE-EFFECT!!!
      symbol_table.set_block_number(new_block_number)

      // Add block to stack frame
      stack_frame.add_block(block_symbol_table.asInstanceOf[BlockSymbolTable])

      val stmts_list = ListBuffer[ir.Stmt]()
      val stack_frames = ListBuffer[Frame](stack_frame)
      val symbol_tables = ListBuffer[SymbolTable](block_symbol_table)
      for (stmt <- block.stmts.decls) {
        val (ir_stmt, new_block_symbol_table : SymbolTable, new_stack_frame : frame.Frame) = code_gen(
          stmt, symbol_tables.last, stack_frames.last)
        stack_frames += new_stack_frame
        symbol_tables += new_block_symbol_table

        stmts_list += ir_stmt
      }

      val block_table = block_symbol_table.asInstanceOf[BlockSymbolTable]

      // update block symbol table in parent symbol table
      symbolTable.update_block_table(current_block_number, block_table)

      val stmts_code = create_seq(stmts_list.toList)
      (stmts_code, symbolTable, stack_frame)
    }

    // NOTE: NEED TO START NEW BLOCK HERE AND FIND THE CORRECT BLOCK SYMBOL TABLE
    // IN symbol_table

    code_gen_block(symbol_table)
  }

  def code_gen(if_node : IfNode, symbol_table : SymbolTable, stack_frame : Frame)
        : (ir.Stmt, SymbolTable, Frame) = {

    // NOTE: NEED TO START NEW BLOCK HERE AND FIND THE CORRECT BLOCK SYMBOL TABLE
    // IN symbol_table
    def code_gen_if_block(stmt: StmtNode, symbolTable : SymbolTable, stack_frame : Frame) :
          (ir.Stmt, SymbolTable, Frame) = {

      // Increase block number and get the correct BlockSymbolTable
      val current_block_number = symbolTable.get_block_number
      val (block_symbol_table, new_block_number) = symbolTable.get_block(current_block_number)
      symbol_table.set_block_number(new_block_number)

      stack_frame.add_block(block_symbol_table.asInstanceOf[BlockSymbolTable])

      val (codeStmt, new_block_table : BlockSymbolTable, new_stack_frame : frame.Frame) = code_gen(
        stmt, block_symbol_table, stack_frame)

      symbolTable.update_block_table(current_block_number, new_block_table)
      (codeStmt, symbolTable, new_stack_frame)
    }

    val cond = code_gen_exp(if_node.exp, symbol_table, stack_frame)

    // NOTE: NEED TO START NEW BLOCK HERE AND FIND THE CORRECT BLOCK SYMBOL TABLE
    // IN symbol_table

    val (first, firstSymbolTable, first_stack_frame) = code_gen_if_block(if_node.stmt1,
      symbol_table, stack_frame)
    val (second, secondSymbolTable, secondStackFrame) = code_gen_if_block(if_node.stmt2, firstSymbolTable,
      first_stack_frame)

    val true_label = temp.Label()
    val false_label = temp.Label()
    val join_label = temp.Label()

    // code for statement1 starts after true_label, code for statement2 after false_label
    // depending on the expression of IfNode we jump to either true_label or false_label
    // We introduce a join label, which is placed at the end of the if-else clause, to
    // which both statements jump afterwards
    val code = ir.Seq((new translate.Expression(cond)).gen_Cx(true_label, false_label), ir.Seq(
      ir.Label(true_label), ir.Seq(first, ir.Seq(ir.Jump(join_label), ir.Seq(
        ir.Label(false_label), ir.Seq(second, ir.Seq(ir.Jump(join_label), ir.Label(join_label))))))))

    (code, secondSymbolTable, secondStackFrame)
  }

  def code_gen(while_node : WhileNode, symbol_table : SymbolTable,
       stack_frame : Frame) : (ir.Stmt, SymbolTable, Frame) = {

    // NOTE: NEED TO START NEW BLOCK HERE AND FIND THE CORRECT BLOCK SYMBOL TABLE
    // IN symbol_table

    def code_gen_while_block(stmt: StmtNode, symbolTable : SymbolTable, stack_frame : Frame) :
       (ir.Stmt, SymbolTable, Frame) = {

      // Increase block number and get the correct BlockSymbolTable
      val current_block_number = symbolTable.get_block_number
      val (block_symbol_table, new_block_number) = symbolTable.get_block(current_block_number)
      symbolTable.set_block_number(new_block_number)

      stack_frame.add_block(block_symbol_table.asInstanceOf[BlockSymbolTable])

      code_gen(stmt, block_symbol_table, stack_frame)
    }

    val exp_code = code_gen_exp(while_node.exp, symbol_table, stack_frame)

    val (stmt_code, new_symbol_table, new_stack_frame) = code_gen_while_block(
      while_node.stmt, symbol_table, stack_frame)

    val loop_label = temp.Label()
    val stmt_label = temp.Label()
    val end_label = temp.Label()

    val code = ir.Seq((new translate.Expression(exp_code)).gen_Cx(stmt_label, end_label), ir.Seq(
      ir.Label(stmt_label), ir.Seq(stmt_code, ir.Seq(ir.Jump(loop_label), ir.Label(end_label)))))

    (code, new_symbol_table, new_stack_frame)
  }

  def code_gen(assign : AssignNode, symbol_table : SymbolTable, stack_frame : Frame) :
      (ir.Stmt, SymbolTable, Frame) = {

    // Get Temp corresponding to variable, which is being assigned to
    val var_name = assign.id.get_name()
    val assign_temp = stack_frame.get_temp(var_name, symbol_table)

    val code_exp = code_gen_exp(assign.exp, symbol_table, stack_frame)

    (ir.Move(ir.Temp(assign_temp), code_exp), symbol_table, stack_frame)
  }

  def code_gen(assign : ArrayAssignNode, symbol_table : SymbolTable, stack_frame : Frame) :
      (ir.Stmt, SymbolTable, Frame) = {

    // Get Temp corresponding to array identifier, which is being assigned to
    val var_name = assign.id.get_name()
    val assign_temp = stack_frame.get_temp(var_name, symbol_table)

    // Get index of array
    val code_index = code_gen_exp(assign.exp1, symbol_table, stack_frame)

    // Get value that is assigned
    val value = code_gen_exp(assign.exp2, symbol_table, stack_frame)

    (ir.Move(ir.BinOp("+", new ir.Temp(assign_temp), code_index), value), symbol_table, stack_frame)
  }

  // EXPRESSIONS

  def code_gen_exp(exp : node.ExpNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    exp match {
      case x @ node.AndNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.LessThanNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.PlusNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.MinusNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.TimesNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.ArrayLookupNode(_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.ArrayLengthNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.CallNode(_,_,_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.IntLiteralNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.TrueNode(_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.FalseNode(_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.IdExpNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.ArrayNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.ObjectNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.NotNode(_,_) => code_gen(x, symbol_table, stack_frame)
      case x @ node.ThisNode(_) => code_gen(x, symbol_table, stack_frame)
    }
  }

  def code_gen(print_node : node.PrintNode, symbol_table : SymbolTable, stack_frame : Frame) :
      (ir.Stmt, SymbolTable, Frame) = {
    val print_exp = code_gen_exp(print_node.exp, symbol_table, stack_frame)
    (ir.PrintNode(print_exp), symbol_table, stack_frame)
  }

  def code_gen(and : AndNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {

    val first = code_gen_exp(and.exp1, symbol_table, stack_frame)
    val second = code_gen_exp(and.exp2, symbol_table, stack_frame)

    val first_true = temp.Label()
    val second_true = temp.Label() 
    val false_label = temp.Label()
    val end_label = temp.Label()

    val result = temp.Temp()

    val ir_code = ir.Eseq(ir.Seq((new translate.Expression(first)).gen_Cx(first_true, false_label),
      ir.Seq(ir.Label(first_true), ir.Seq((new translate.Expression(second)).gen_Cx(second_true, false_label),
        ir.Seq(ir.Label(second_true), ir.Seq(ir.Move(ir.Temp(result), ir.Const(1)), ir.Seq(
          ir.Jump(end_label), ir.Seq(ir.Label(false_label), ir.Seq(ir.Move(ir.Temp(result),
            ir.Const(0)), ir.Label(end_label))))))))),
      ir.Temp(result))

    ir_code
  }

  def code_gen(less : LessThanNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {

    val left = code_gen_exp(less.exp1, symbol_table, stack_frame)
    val right = code_gen_exp(less.exp2, symbol_table, stack_frame)

    ((new translate.RelCx("<", new translate.Expression(left), new translate.Expression(right))).gen_Ex())
  }

  def code_gen(plus : PlusNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {

    val left = code_gen_exp(plus.exp1, symbol_table, stack_frame)
    val right = code_gen_exp(plus.exp2, symbol_table, stack_frame)

    ir.BinOp("+", left, right)
  }

  def code_gen(minus : MinusNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {

    val left = code_gen_exp(minus.exp1, symbol_table, stack_frame)
    val right = code_gen_exp(minus.exp2, symbol_table, stack_frame)

    ir.BinOp("-", left, right)
  }

  def code_gen(times : TimesNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {

    val left = code_gen_exp(times.exp1, symbol_table, stack_frame)
    val right = code_gen_exp(times.exp2, symbol_table, stack_frame)

    ir.BinOp("*", left, right)
  }

  def code_gen(arr_length : ArrayLengthNode,  symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    // Array length is always contained in the first element of the array
    val code_arr_length = code_gen_exp(arr_length.exp, symbol_table, stack_frame)
    code_arr_length
  }
  
  // Note: For variable declarations in classes 
  def code_gen(var_decl : VarDecl, symbol_table : SymbolTable, stack_frame : Frame) :
      (ir.Stmt, SymbolTable, Frame) = {
    // All local variables will be translated to Temp instances. The register allocator will later
    // move local variables to the stack if necessary

    // NOTE: THE METHOD LOGIC WILL NEED TO BE MODIFIED IF WE ALLOW VARIABLE DECLARATIONS
    // INSIDE CLASS DECLARATIONS

    stack_frame.add_temp(var_decl.id.get_name(), symbol_table)


    (ir.NoOp(), symbol_table, stack_frame)
  }

  def code_gen(this_node : ThisNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    // No need to translate this for now, since "this" is only used in method calls
    // If we later add class variable lookup, then we need to translate "this" to the first
    // parameter of the method declaration
    ir.This()
  }

  def code_gen(arr : ArrayNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = { 
    // ArrayNode corresponds to array creation: -> new int[Exp]
    val code_arr_length = code_gen_exp(arr.exp, symbol_table, stack_frame)
    ir.AllocArray(code_arr_length)
  }

  def code_gen(not : NotNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = { 
    // Change Const(1)->Const(0) and Const(0)->Const(1)
    val exp_code = code_gen_exp(not.exp, symbol_table, stack_frame)
    val result = temp.Temp()

    val t = temp.Label()
    val f = temp.Label()
    val join = temp.Label() 

    val code = ir.Eseq(ir.Seq((new translate.Expression(exp_code)).gen_Cx(t,f), ir.Seq(ir.Label(t),
      ir.Seq(ir.Move(ir.Temp(result), Const(0)), ir.Seq(ir.Jump(join), ir.Seq(
        ir.Label(f), ir.Seq(ir.Move(ir.Temp(result), Const(1)), ir.Label(join))))))),
      ir.Temp(result))

    code
  }

  def code_gen(arr_lookup : ArrayLookupNode, symbol_table : SymbolTable, stack_frame : Frame)
        : ir.Exp = {

    // Need to get a temp for the array name, this should be done in the
    // code_gen statement of the first exp of arr_lookup
    val code_arr = code_gen_exp(arr_lookup.get_arr_exp(), symbol_table, stack_frame)
    val code_index = code_gen_exp(arr_lookup.get_index_exp(), symbol_table, stack_frame)

    val result = temp.Temp()
    val index_in_bounds = temp.Label()

    val valid_path = ir.Seq(ir.Label(index_in_bounds), ir.Move(ir.Temp(result),
      (new translate.ArrayLookupExp(code_arr, code_index)).gen_Ex()))

    // Check for out of bounds error
    // First check whether length of array (first element in array: Mem(arr)) is smaller than
    // the index. If it is then we generate code for the array lookup, if it isn't then
    // we jump to the label which generates code to print an array exception
    val code = ir.Eseq(ir.Seq(ir.CJump("<", ir.Mem(code_arr), code_index, index_in_bounds,
      index_bounds_exception_label), valid_path),
      ir.Temp(result))

    code
  }

  def code_gen(integer : IntLiteralNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    ir.Const(integer.i)
  }

  def code_gen(t: TrueNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    ir.Const(1)
  }

  def code_gen(t: FalseNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    ir.Const(0)
  }

  def code_gen(id_exp : IdExpNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    // lookup temp for variable in stack_frame
    ir.Temp(stack_frame.get_temp(id_exp.get_name, symbol_table))
  }

  def code_gen(id : IdNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    ir.Temp(stack_frame.get_temp(id.get_name(), symbol_table))
  }

  def code_gen(obj : ObjectNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {
    // We initialize all fields to Const(0), even arrays and other objects
    // These will later hold the address of the memory that was allocated
    // on the heap for arrays and objects

    // Note: At this point we have no way of accessing fields of objects anyway in the language
    // so I'm not sure what's the purpose of this

    // Alloc system call will initialize memory to 0, don't need to do this manually for now,
    // only later when I allow constructors
    /*
    def fields_init_code() : ir.Stmt = {

      def gen_move(i : Int) : ir.Stmt = {
        if (i == fields.length - 1) {
          val field = class_fields(i)
          val offset = symbol_table.get_offset(field)
          return ir.Move(ir.BinOp("+", base_address, offset), ir.Const(0))
        }
        val field = class_fields(i)
        val offset = symbol_table.get_offset(field)
        ir.Seq(ir.Move(ir.BinOp("+", base_address, offset), ir.Const(0)), gen_move(i + 1))
      }

      val class_fields = symbol_table.get_fields_in_order
      gen_move(0)
    }
     */
    // TO-DO: I have to add the offset of each field into the symbol table and the
    // size of each field
    val class_name = obj.get_name
    val symbol_table = class_symbol_tables.get_class_table(class_name)
    val class_size = symbol_table.get_class_size

    // Need to use system call to allocate memory for object
    // So the mips code that i will generate will be a syscall 9, which returns
    // an address in register $v0.
    // This will later be used to access fields of object by using offsets and will
    // be passed to method calls as an argument implicitly
    ir.AllocObject(class_size)
  }

  def code_gen(call : CallNode, symbol_table : SymbolTable, stack_frame : Frame) : ir.Exp = {

    // first get the class name of the object
    val class_name = call.exp match {
      case IdExpNode(s, _) => s
      case ObjectNode(id, _) => id.get_name()
      // For some reason scalac complained that symbol_table was of type Any
      case ThisNode(_) => symbol_table.asInstanceOf[SymbolTable].get_class_name
      case _ => throw new Exception("""unexpected type of object in code_gen of CallNode 
                                     cannot get class name of object""")
    }

    // Note: Not sure how to handle case where call.exp is IdNode. In that case I would need
    // to get the class corrsponding to that id => Possibly through symbol table

    val method_name = call.id.get_name()
    val method_symbol_table = class_symbol_tables.get_class_table(class_name).get_method_table(method_name)
    val func_label = method_symbol_table.code_label

    val obj = code_gen_exp(call.exp, symbol_table, stack_frame)
    val args = code_gen(call.exp_list.asInstanceOf[node.ExpList], symbol_table, stack_frame)

    // Add the object as first argument
    // Not sure how to get instance for object yet, but should be sufficient if I know the
    // correct label for the function
    val code_args = args.prepend(obj)

    ir.Call(ir.Name(func_label), code_args)
  }

  def code_gen(exp_list : node.ExpList, symbol_table : SymbolTable, stack_frame : Frame) : ir.ExpList = {
    // ATTRIBUTE OF EXP_LIST SHOULDNT BE CALLED DECLS!!!
    println("Inside code_gen for ExpList")
    val exps = exp_list.decls.map(x => code_gen_exp(x.asInstanceOf[node.ExpNode], symbol_table, stack_frame))
    ir.ExpList(exps)
  }
}
