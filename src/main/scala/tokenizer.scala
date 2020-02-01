package token

import scala.collection.immutable.ListSet
import scala.io.Source
import scala.collection.mutable.ListBuffer

class Tokenizer(val filename : String) {
  var tokens : ListBuffer[Token] = new ListBuffer[Token]()
  var line_num : Int = 1
  val input = get_input(filename)

  def get_input(filename : String) : String = {
    val file_buffer = Source.fromFile(filename)
    file_buffer.mkString
  }

  def tokenize_input() : List[Token] = {
    var i : Int = 0
    while (i < input.length) {
        val current_char = input.charAt(i)
        if (current_char == '\n'){
            // println("current_char is newline character, next char is: %s".format(input.charAt(i + 1)))
            line_num += 1
            i += 1
        }
        else if (current_char.isWhitespace) {
            val new_index = skip_whitespace(input, i)
            i = new_index
        }
        else if (current_char.isDigit) { 
          val (int_token, new_index) = process_int(input, i)
          i = new_index
          tokens += int_token
        }
        else if (current_char == ';') {
          i += 1
          tokens += SemiColon(line_num)
        }
        else if (current_char == ':') {
          i += 1
          tokens += Colon(line_num)
        }
        else if (current_char == '(') {
          i += 1
          tokens += LeftParen(line_num)
        }
        else if (current_char == ')') {
          i += 1
          tokens += RightParen(line_num)
        }
        else if (current_char == '{') {
          i += 1
          tokens += LeftCurly(line_num)
        }
        else if (current_char == '}') {
          i += 1
          tokens += RightCurly(line_num)
        }
        else if (current_char == '[') {
          i += 1
          tokens += LeftBracket(line_num)
        }
        else if (current_char == ']') {
          i += 1
          tokens += RightBracket(line_num)
        }
        else if (current_char == '.') {
          i += 1
          tokens += Dot(line_num)
        }
        else if (current_char == '!') {
          i += 1
          tokens += BANG(line_num)
        }
        else if (current_char == '=') {
          i += 1
          tokens += EQUAL(line_num)
        }
        else if (current_char == ',') {
          i += 1
          tokens += COMMA(line_num)
        }
        /*
        else if (current_char == '>'){
          if (i + 1 < input.length) {
            val next_char = input.charAt(i+1)
            if (next_char == '=') {
              i += 2
              tokens += GEQ(line_num)
            }
            else {
              i += 1
              tokens += GT(line_num)
            }
          }
          else {
            i += 1
            tokens += GT(line_num)
          }
        }
        else if (current_char == '<') {
          if (i + 1 < input.length) {
            val next_char = input.charAt(i+1)
            if (next_char == '=') {
              i += 2
              tokens += LEQ(line_num)
            }
            else {
              i += 1
              tokens += LT(line_num)
            }
          }
          else {
            i += 1
            tokens += LT(line_num)
          }
        }
         */
        else if (current_char == '<'){
          i += 1
          tokens += BinOp("<", line_num)
        }
        else if (current_char == '+'){
          i += 1
          tokens += BinOp("+", line_num)
        }
        else if (current_char == '-'){
          i += 1
          tokens += BinOp("-", line_num)
        }
        else if (current_char == '*'){
          i += 1
          tokens += BinOp("*", line_num)
        }
        else if (current_char == '/'){
          i += 1
          tokens += BinOp("/", line_num)
        }
        else if (current_char == '&') {
          val next_char = input.charAt(i + 1)
          if (next_char == '&'){
            i += 2
            tokens += BinOp("&&", line_num)
          }
        }
        else {
          assert(current_char.isLetter)
          val (id, new_index) = process_identifier(input, i)
          i = new_index
          val token = lookup_identifier(id)
          tokens += token
        }
      }
      tokens += EOF(line_num)
      tokens.toList
    }

    def skip_whitespace(s : String, index : Int) : Int = {
      var i = 1
      while (index + i < input.length && s.charAt(index + i).isWhitespace) {
        i += 1
      }
      index + i
    }

  def process_int(s : String, start_index : Int) : (INT, Int) = {
    var current_index = start_index + 1
    while (current_index < input.length && s.charAt(current_index).isDigit){
      current_index += 1
    }
    (INT(s.substring(start_index, current_index).toInt, line_num), current_index)
  }

  def process_identifier(s : String, start_index : Int) : (String, Int) = {
    var i = 1
    var current_char = s.charAt(start_index + i)
    while (start_index + i < input.length && (current_char.isLetter | current_char.isDigit | current_char == '_')) {
      i += 1
      current_char = s.charAt(start_index + i)
    }
    val end_index = start_index + i 
    (s.substring(start_index, end_index), start_index + i)
  }

  def lookup_identifier(s : String) : Token = {
    if (s == "int") return IntType(line_num)
    else if (s == "boolean") return BooleanType(line_num)
    else if (s == "public") return PUBLIC(line_num)
    else if (s == "private") return PRIVATE(line_num)
    else if (s == "class") return CLASS(line_num)
    else if (s == "extends") return EXTENDS(line_num)
    else if (s == "void") return VOID(line_num)
    else if (s == "main") return MAIN(line_num)
    else if (s == "return") return RETURN(line_num)
    else if (s == "if") return IF(line_num)
    else if (s == "else") return ELSE(line_num)
    else if (s == "println") return PRINT(line_num)
    else if (s == "while") return WHILE(line_num)
    else if (s == "new") return NEW(line_num)
    else if (s == "this") return THIS(line_num)
    else if (s == "true") return TRUE(line_num)
    else if (s == "false") return FALSE(line_num)
    else if (s == "length") return LENGTH(line_num)
    else if (s == "String") return STRING(line_num)
    else if (s == "println") return PRINT(line_num)
    else if (s == "static") return STATIC(line_num)
    else return ID(s, line_num)
  }
}

abstract class Token(val line_num : Int){
  override def equals(that : Any) : Boolean = {
    this.getClass() == that.getClass()
  }
}

case class ID(str: String, override val line_num : Int) extends Token(line_num)
case class INT(value : Int,override val line_num : Int) extends Token(line_num)
case class BinOp(str : String, override val line_num : Int) extends Token(line_num)
case class IntType(override val line_num : Int) extends Token(line_num)
case class BooleanType(override val line_num : Int) extends Token(line_num)
case class PUBLIC(override val line_num : Int) extends Token(line_num)
case class PRIVATE(override val line_num : Int) extends Token(line_num)
case class VOID(override val line_num : Int) extends Token(line_num)
case class MAIN(override val line_num : Int) extends Token(line_num)
case class RETURN(override val line_num : Int) extends Token(line_num)
case class CLASS(override val line_num : Int) extends Token(line_num)
case class EXTENDS(override val line_num : Int) extends Token(line_num)
case class IF(override val line_num : Int) extends Token(line_num)
case class ELSE(override val line_num : Int) extends Token(line_num)
case class PRINT(override val line_num : Int) extends Token(line_num)
case class WHILE(override val line_num : Int) extends Token(line_num)
case class NEW(override val line_num : Int) extends Token(line_num)
case class THIS(override val line_num : Int) extends Token(line_num)
case class TRUE(override val line_num : Int) extends Token(line_num)
case class FALSE(override val line_num : Int) extends Token(line_num)
case class LENGTH(override val line_num : Int) extends Token(line_num)
case class STRING(override val line_num : Int) extends Token(line_num)
case class SemiColon(override val line_num : Int) extends Token(line_num)
case class Colon(override val line_num : Int) extends Token(line_num)
case class LeftParen(override val line_num : Int) extends Token(line_num)
case class RightParen(override val line_num : Int) extends Token(line_num)
case class LeftCurly(override val line_num : Int) extends Token(line_num)
case class RightCurly(override val line_num : Int) extends Token(line_num)
case class LeftBracket(override val line_num : Int) extends Token(line_num)
case class RightBracket(override val line_num : Int) extends Token(line_num)
case class Dot(override val line_num : Int) extends Token(line_num)
case class BANG(override val line_num : Int) extends Token(line_num)
case class EQUAL(override val line_num : Int) extends Token(line_num)
case class COMMA(override val line_num : Int) extends Token(line_num)
case class LT(override val line_num : Int) extends Token(line_num)
case class LEQ(override val line_num : Int) extends Token(line_num)
case class GT(override val line_num : Int) extends Token(line_num)
case class GEQ(override val line_num : Int) extends Token(line_num)
case class STATIC(override val line_num : Int) extends Token(line_num)
case class EOF(override val line_num : Int) extends Token(line_num)

