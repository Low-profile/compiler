import parser._
import CharP._
import expression._
import Statement._

object FunctionS {


  def letter = {
    val letters = ('a' to 'z').toList ::: ('A' to 'Z').toList
    anyOf(letters)
  }


  def type_P = {
    val type_literal = List("int","bool","double")
    choice(type_literal.map(pstring(_))).setLabel("type")
  }

  def fun_def ={
    val func_P = whitespace <~ type_P ~> whitespaces ~ id ~> whitespace ~>
      leftParen ~> whitespace ~> rigthParen ~> whitespace ~>
      leftCurly~> whitespace ~
      many1(stmt) ~>
      rigthCurly~> whitespace

    def transformer(z:((String,String),StmtAST)) = {
      new FunctionAST(z._1._1,z._1._2,z._2)
    }
    mapP(transformer )(func_P)
  }
}
