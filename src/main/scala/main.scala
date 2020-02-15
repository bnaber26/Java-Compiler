import codegen.MipsCodeGenerator
import token.Tokenizer
import parser.Parser
import typecheck.TypeChecker
import canon.Canonicalizer
import scala.io.Source
import frame.Frame
import util.Util.{print_ir_tree, print_program}
import ir._

import scala.collection.mutable.ListBuffer


object Main extends App {
  val filename = "/Users/bn/Documents/Library/CS/Compilers/sample_program.java"
  val tokens = (new Tokenizer(filename)).tokenize_input()
  val ast = (new Parser(tokens)).parse()
  val type_checker = new TypeChecker(ast)
  val symbol_tables_collection = type_checker.get_symbol_tables
  val main_symbol_table = type_checker.get_main_symbol_table
  val types_correct = type_checker.type_check()
  if (types_correct) {
    val code_generator = new MipsCodeGenerator(ast, symbol_tables_collection, main_symbol_table)
    val (main_code, fragments) = code_generator.get_code_and_fragments
    println("before canonicalization:")
    print_program(main_code, fragments.toList)

    val canonicalizer = new Canonicalizer(main_code, fragments.toList)
    val (new_main, new_fragments) = canonicalizer.transform()
    println("\n after canonicalization:")
    print_program(new_main, new_fragments.toList)
    println("first tranformed fragment body")
    new_fragments(0).body.prettyPrint(0)

    println("Trying to linearize the ir code:")
    val linearized_main = canonicalizer.get_statement_list(main_code)
    println("linearized main:")
    linearized_main.prettyPrint(0)

    val linearized_fragment_bodies = ListBuffer[ir.StmtList]()
    for (frag <- new_fragments) {
      val linearized = canonicalizer.linearize(frag.body.asInstanceOf[ir.Eseq].stmt)
      println("linearized statement of fragment:")
      linearized.prettyPrint(0)
      linearized_fragment_bodies += canonicalizer.get_statement_list(frag.body)
    }
    for (linearized_body <- linearized_fragment_bodies) {
      linearized_body.prettyPrint(0)
    }

    println()
    println()
    println("Basic Blocks:")
    val first_fragment_blocks = canonicalizer.get_basic_blocks(linearized_fragment_bodies(0))
    println("Number of blocks returned: %d".format(first_fragment_blocks.length))
    for (block <- first_fragment_blocks) {
      block.prettyPrint()
    }

    /*
    println("Test linearization function")
    val test_stmt = ir.Seq(ir.Seq(ir.Seq(ir.Move(ir.Temp(temp.Temp()), ir.Const(1)),
        ir.Move(ir.Temp(temp.Temp()), ir.Const(0))), ir.Seq(ir.Seq(ir.Expression(ir.Const(0)),
        ir.Expression(ir.Const(0))), ir.Expression(ir.Const(0)))),
        ir.Expression(ir.Const(0)))
    test_stmt.prettyPrint(0)

    val linearized_stmt = canonicalizer.linearize(test_stmt)
    println("linearized statement")
    linearized_stmt.prettyPrint(0)
     */


  }
  else {
    println("Syntax errors, aborting compilation")
  }

}
