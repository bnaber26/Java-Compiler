package parser

import token._
import scala.collection.immutable.List
import node._

class Parser(val tokens : List[Token]) {
    var index = 0

    def parse() : AST = {
        val program_node = Prog()
        new AST(program_node)
    }

    def consume_next(expected : Token) = {
        val next_token = tokens(index)
        if (next_token == expected){
          index += 1
        }
        else {
        // Need to probably throw an error here
          throw new Exception("Cannot consume token %s on line %d".format(
            expected.getClass(), next_token.line_num))
        }
    }

  def advance() = {
    index += 1
  }

    def Prog() : Program = {
      /*
       Prog -> MainClass ClassDecl
       */
        val main_node = Main()
        val class_decl = CLASS_DECLS(List[ClassDecl]())
        Program(main_node, class_decl)
    }

    def Main() : MainNode = {
        /*
         MainClass -> class id { public static void main (String [] id)
                           { Statement } }
         */
        consume_next(CLASS(0))
        val id_node1 = ID_NODE()
        consume_next(LeftCurly(0))
        consume_next(PUBLIC(0))
        consume_next(STATIC(0))
        consume_next(VOID(0))
        consume_next(MAIN(0))
        consume_next(LeftParen(0))
        consume_next(STRING(0))
        consume_next(LeftBracket(0))
        consume_next(RightBracket(0))
        val id_node2 = ID_NODE()
        consume_next(RightParen(0))
        consume_next(LeftCurly(0))
        val stmt_node = STMT()
        consume_next(RightCurly(0))
        consume_next(RightCurly(0))
        MainNode(id_node1, id_node2, stmt_node)
    }

    def CLASS_DECL() : ClassDecl = {
      /*
       ClassDecl -> class id { VarDecl* MethodDecl* }
                  | class id extends id { VarDecl* MethodDecl* }
       */

      // Extract the first two common tokens
        consume_next(CLASS(0))
        val id_node1 = ID_NODE()

      // Distinguish two productions
      
        val rest_node = CLASS_DECL_PRIME()
      // first productions yields list of length 2
        if (rest_node.length == 2){
            val (var_decls : VarDeclList, meth_decls : MethodDeclsList) = (rest_node(0), rest_node(1))
            return ClassDeclSimple(id_node1, var_decls, meth_decls)
        }
        val (id_node2 : IdNode, var_decls : VarDeclList, meth_decls : MethodDeclsList) = (rest_node(0),
        rest_node(1), rest_node(2))
        ClassDeclExtends(id_node1, id_node2, var_decls, meth_decls)
    }

  /*
   Helper production for CLASS_DECL
   */
    def CLASS_DECL_PRIME() : List[Node] = {
      /*
       Either of the following productions:
         
       { VarDecl* MethodDecl* }
       extends id { VarDecl* MethodDecl* }

       */

      // Functions for distinguishing the two possible rules
        def first_rule() : List[Node] = {
            consume_next(LeftCurly(0))

          // VAR_DECLS will call itself recursively, add VarDecl instances
          // to the list we pass as argument
            val var_decls = VAR_DECLS(List[VarDecl]())

          // METHOD_DECLS also uses recursive calls, hence we pass the empty list
            val method_decls = METHOD_DECLS(List[MethodDecl]())

            consume_next(RightCurly(0))

            List(var_decls, method_decls)
        }

        def second_rule() : List[Node] = {
            consume_next(EXTENDS(0))
            val id_node = ID_NODE()
            consume_next(LeftCurly(0))
            val var_decls = VAR_DECLS(List[VarDecl]())
            val method_decls = METHOD_DECLS(List[MethodDecl]())
            consume_next(RightCurly(0))
            List(id_node, var_decls, method_decls)
        }

        val next_token = tokens(index)
        next_token match {
            case LeftCurly(_) => first_rule()
            case EXTENDS(_) => second_rule()
            case _ => throw new Exception("Error during CLASS_DECL_PRIME rule") 
        }
    }

    def CLASS_DECLS(previous_decls : List[ClassDecl]) : ClassDeclList = {
        val next_token = tokens(index)
        next_token match {
            // Follow(CLASS_DECLS) = {CLASS, EOF}
            case EOF(_) => ClassDeclList(previous_decls)
            case _ => {
                assert(next_token == CLASS(0))
                val class_decl = CLASS_DECL()
                CLASS_DECLS(previous_decls ++ List[ClassDecl](class_decl))
        }
        }
    }

    def VAR_DECL() : VarDecl = {
        val type_node = TYPE()
        val id_node = ID_NODE()
        consume_next(SemiColon(0))
        VarDecl(type_node, id_node)
    }

    def VAR_DECLS(previous_var_decls : List[VarDecl]) : VarDeclList = {
        val next_token = tokens(index)
        next_token match {
            case IntType(_) | token.BooleanType(_) => {
                val var_decl = VAR_DECL()
                return VAR_DECLS(previous_var_decls ++  List[VarDecl](var_decl))
            }
          case _ => {
            // Assert that next_token is in Follow(VarDecls)
            assert(next_token == PUBLIC(0) | next_token == RightCurly(0) |
              next_token == RETURN(0) | next_token == LeftCurly(0) |
              next_token == IF(0) | next_token == WHILE(0) | next_token == PRINT(0) |
              next_token == ID("",0))

            // No more Variable Declarations, return accumulated list of Variable Declarations
            VarDeclList(previous_var_decls)
          }
        }
    }

  def METHOD_DECL() : MethodDecl = {
    /*
     MethodDecl -> public Type id ( ParamList ) {
                       VarDecl* Statement* return Exp; }
     */
    consume_next(PUBLIC(0))
    val type_node = TYPE()
    val id_node = ID_NODE()
    consume_next(LeftParen(0))
    val params_list = PARAM_LIST()
    consume_next(RightParen(0))
    consume_next(LeftCurly(0))
    val var_decls = VAR_DECLS(List[VarDecl]())
    val stmts = STMTS( List[StmtNode]())
    consume_next(RETURN(0))
    val return_exp = EXP()
    consume_next(SemiColon(0))
    consume_next(RightCurly(0))
    MethodDecl(type_node, id_node, params_list, var_decls, stmts, return_exp)
  }

  def METHOD_DECLS(previous_decls : List[MethodDecl]) : MethodDeclsList = {
    val next_token = tokens(index)
    next_token match {
      case RightCurly(_) => return MethodDeclsList(previous_decls)
      case _ => {
        // More MethodDecls upcoming, assert next_token is in First(METHOD_DECL)
        assert(next_token == PUBLIC(0))

        val method_decl = METHOD_DECL()
        METHOD_DECLS(previous_decls ++  List[MethodDecl](method_decl))
      }
    }
  }

  def PARAM_LIST() : ParamList = {
    /*
     ParamList -> Type id ParamRest*
                | \eps
     */
    val next_token = tokens(index)
    next_token match {
      // RightParen is in Follow(PARAM_LIST) this is the empty production
      case RightParen(_) => ParamList(List[ParamNode]())
      case _ => {
        // Assert that next_token is a Type (First(ParamList) = {TYPE}
        assert(next_token == IntType(0) | next_token == token.BooleanType(0) |
        next_token == ID("",0))

        val type_node = TYPE()
        val id_node = ID_NODE()
        val rest = PARAM_REST(List[ParamNode]())

        ParamList( rest ++ List[ParamNode](ParamNode(type_node, id_node)))
      }
    }
  }

  def PARAM_REST(previous_params : List[ParamNode]) : List[ParamNode] = {
    /*
     ParamRest -> , Type id
     */

    val next_token = tokens(index)

    if (next_token == RightParen(0)){
      return previous_params
    }

    val type_node = TYPE()
    val id_node = ID_NODE()
    PARAM_REST(previous_params ++  List[ParamNode](ParamNode(type_node, id_node)))
  }

  def STMT() : StmtNode = {
    /*
     Statement -> { Statement* }
                | if (Exp) Statement else Statement
                | while (Exp) Statement else Statement
                | println(Exp);
                | id = Exp;
                | id[Exp] = Exp;
     */

    val next_token = tokens(index)
    next_token match {
      case LeftCurly(_) => {
        consume_next(LeftCurly(0))
        val stmts_list = STMTS( List[StmtNode]())
        consume_next(RightCurly(0))
        BlockNode(stmts_list)
      }
      case IF(_) => {
        consume_next(IF(0))
        consume_next(LeftParen(0))
        val exp = EXP()
        consume_next(RightParen(0))
        val stmt = STMT()
        consume_next(ELSE(0))
        val stmt2 = STMT()
        IfNode(exp, stmt, stmt2)
      }
      case WHILE(_) => {
        consume_next(WHILE(0))
        consume_next(LeftParen(0))
        val exp = EXP()
        consume_next(RightParen(0))
        val stmt = STMT()
        WhileNode(exp, stmt)
      }
      case PRINT(_) => {
        consume_next(PRINT(0))
        consume_next(LeftParen(0))
        val exp = EXP()
        consume_next(RightParen(0))
        consume_next(SemiColon(0))
        PrintNode(exp)
      }
      case ID(s, _) => {
        val id_node = ID_NODE()
        tokens(index) match {
          case EQUAL(_) => {
            consume_next(EQUAL(0))
            val exp = EXP()
            consume_next(SemiColon(0))
            return AssignNode(id_node, exp)
          }
          case LeftBracket(_) => {
            consume_next(LeftBracket(0))
            val exp1 = EXP()
            consume_next(RightBracket(0))
            consume_next(EQUAL(0))
            val exp2 = EXP()
            consume_next(SemiColon(0))
            return ArrayAssignNode(id_node, exp1, exp2)
          }
        }
      }
    }
  }

  def STMTS(previous_stmts : List[StmtNode]) : StmtList = {
    val next_token = tokens(index)
    next_token match {
      case RightCurly(_) | RETURN(_) => {
        return StmtList(previous_stmts)
      }
      case _ => {
        assert(next_token == IF(0) | next_token == WHILE(0) |  next_token == PRINT(0) |
           next_token == ID("", 0) | next_token == RETURN(0) | next_token == RightCurly(0))

        val stmt = STMT()
        return STMTS(previous_stmts ++  List[StmtNode](stmt))
      }
    }
  }

  def EXP() : ExpNode = {
    /*
     Exp -> Exp1 ExpPrime

     Grammar before elemination of left recursion:

     Exp -> Exp op Exp
          | Exp[Exp]
          | Exp.length
          | Exp.id(ExpList)
          | INT | true | false | id | this | new int[Exp] | new id() | !Exp | (Exp) 

     When parsing expressions corresponding to the first four statement we will
     call Exp1 and pass the result to ExpPrime as an inherited attribute. ExpPrime
     will then be called resursively (passing the intermediate results as inherited
     attributes again) until the recursion will bottom out, in which case we return
     the inherited attribute
     
     */

    val exp1 = EXP_1()
    // Pass the inherited attribute here
    EXP_PRIME(exp1)
  }

  def EXP_1() : ExpNode = {
    /*

     Exp1 -> INT | true | false | id | this | new int[Exp] | new id() | !Exp | (Exp)

     */

    val next_token = tokens(index)
    next_token match {
      case INT(v, l) => {advance(); return IntLiteralNode(v, l)}
      case TRUE(l) => {advance(); return TrueNode(l)}
      case FALSE(l) => {advance(); return FalseNode(l)}
      case ID(s, l) => {advance(); return IdExpNode(s, l)}
      case THIS(l) => {advance(); return ThisNode(l)}
      case NEW(l) => {
        consume_next(NEW(0))
        val second_next_token = tokens(index)
        second_next_token match {
          case IntType(_) => {
            consume_next(IntType(0))
            consume_next(LeftBracket(0))
            val exp = EXP()
            consume_next(RightBracket(0))
            return ArrayNode(exp, l)
          }
          case ID(_, l) => {
            val id_node = ID_NODE()
            consume_next(LeftParen(0))
            consume_next(RightParen(0))
            return ObjectNode(id_node, l)
          }
        }
      }
      case BANG(l) => {
        consume_next(BANG(0))
        val exp = EXP()
        return NotNode(exp, l)
      }
      case LeftParen(_) => {
        consume_next(LeftParen(0))
        val exp = EXP()
        consume_next(RightParen(0))
        return exp
      }
    }
  }

  def EXP_PRIME_FIRST(exp1 : ExpNode) : Option[ExpNode] = {
    /*
     Exp_Prime_First -> Exp op Exp
                      | Exp[Exp]
                      | Exp.length
                      | Exp.id(ExpList)
                      | \eps
     
     If this production results in an \eps derivation, we return None,
     otherwise we return Some(exp), where Exp is some type of ExpNode
     */

    val next_token = tokens(index)
    next_token match {
      case BinOp(op, l) => {
        consume_next(BinOp(op, 0))
        val exp2 = EXP()
        op match {
          case "+" => return Some(PlusNode(exp1, exp2, l))
          case "-" => return Some(MinusNode(exp1, exp2, l))
          case "*" => return Some(TimesNode(exp1, exp2, l))
          case "<" => return Some(LessThanNode(exp1, exp2, l))
          case "&&" => return Some(AndNode(exp1, exp2, l))
          case _ => throw new Exception("EXP_PRIME_FIRST not a valid BinOp")
        }
      }
      case LeftBracket(l) => {
        consume_next(LeftBracket(0))
        val exp2 = EXP()
        consume_next(RightBracket(0))
        Some(ArrayLookupNode(exp1, exp2, l))
      }
      case Dot(l) => {
        consume_next(Dot(0))
        val second_next_token = tokens(index)
        second_next_token match {
          case LENGTH(l) => {
            consume_next(LENGTH(0))
            Some(ArrayLengthNode(exp1, l))
          }
          case ID(s, l) => {
            val id_node = ID_NODE()
            consume_next(LeftParen(0))
            val exps_list = EXP_LIST()
            consume_next(RightParen(0))
            Some(CallNode(exp1, id_node, exps_list, l))
          }
        }
      }
      case _ => {
        // No more EXP_PRIME() productions following
        None
      }
    }
  }

  def EXP_PRIME(exp1 : ExpNode) : ExpNode = {
    /*

     ExpPrime -> ExpPrimeFirst ExpPrime
     
     */

    val first = EXP_PRIME_FIRST(exp1)
    first match {
      case Some(exp) => EXP_PRIME(exp)
      case None => exp1
    }
  }

  def EXP_LIST() : ExpList = {
    /*
     
     ExpList -> Exp ExpRest*

     */

    val next_token = tokens(index)
    next_token match {
      case RightParen(_) => ExpList( List[ExpNode]())
      case _ => {}
    }
    val exp = EXP()
    val exp_rest = EXP_REST( List[ExpNode]())
    ExpList(List[ExpNode](exp) ++ exp_rest)
  }

  def EXP_REST(previous_exps : List[ExpNode]) : List[ExpNode]= {
    /*
     
     ExpRest -> , Exp

     */

    val next_token = tokens(index)
    next_token match {
      case RightParen(_) => return previous_exps
      case _ => {
        assert(next_token == COMMA(0))

        consume_next(COMMA(0))
        val exp = EXP()
        return EXP_REST(previous_exps ++  List[ExpNode](exp))
      }
    }
  }

  def ID_NODE() : IdNode = {
    tokens(index) match {
      case ID(s, l) => {advance(); return IdNode(s, l)}
      case _ => throw new Exception("Error when parsing ID_NODE, token found was %s".format(
        tokens(index).getClass()))
    }
  }

  def TYPE() : TypeNode = {
    val next_token = tokens(index)
    next_token match {
      case IntType(_) => {
        consume_next(IntType(0))
        val second_next_token = tokens(index)
        second_next_token match {
          case LeftBracket(_) => {
            consume_next(LeftBracket(0))
            consume_next(RightBracket(0))
              IntArrayType()
          }
          case _ => {
            assert(second_next_token == ID("",0))

            IntegerType()
          }
        }
      }
      case token.BooleanType(_) => {
        consume_next(token.BooleanType(0))
        node.BooleanType()
      }
      case ID(s,_) => {
        val id_node = ID_NODE()
        IdentifierType(s)
      }
      case _ => throw new Exception("Error during TYPE()")
    }
  }

}

class AST(val program_node : Program)

