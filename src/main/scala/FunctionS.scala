import parser._
import Ast._

object FunctionS {

  def leftParen = pstring("(")

  def rigthParen = pstring(")")

  def leftCurly = pstring("{")

  def rigthCurly = pstring("}")

  def letter = {
    val letters = ('a' to 'z').toList ::: ('A' to 'Z').toList
    anyOf(letters)
  }

  def digit = anyOf(('0' to '9').toList)

  // define parser for one or more digits
  def digits = many1 (digit)

  def underline = pchar('_')

  def id ={
    val first_char = letter.setLabel("id_first")

    val rest_chars = many(choice(List(letter,digit,underline))).setLabel("id_rest")

    val ret = andThen(first_char, rest_chars)

    def transformer(z:(Char,List[Char])) = {
      (z._1 :: z._2).mkString
    }
    mapP(transformer)(ret)
  }

  def type_P = {
    val type_literal = List("int","bool","double")
    choice(type_literal.map(pstring(_))).setLabel("type")
  }

  def fun_def ={
    val def_list = List(type_P,whitespaces,id,whitespace,leftParen,whitespace,rigthParen,whitespace,leftCurly,whitespace,rigthCurly)
    val func_string = sequence(def_list)

    def transformer(z:List[String]) = {
//      new FunctionAST(z(0),z(1))
    }

  }
}
