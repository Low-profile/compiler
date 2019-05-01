import parser._
import CharP._
import Expression._
import Statement._

object FunctionS {

  //var_decl id [ '[' intcon ']' ]
  val var_decl = {
    val array = leftBracket ~> whitespace <~ intcon ~> whitespace ~> rightBracket ~> whitespace
    val ret = id ~ opt(array) ~> whitespace

    def transRet(z: (String, Option[Int])) = {
      new VardeclAST(z._1, z._2)
    }

    mapP(transRet)(ret)
  }

  //  dcl 	: 	type var_decl { ',' var_decl }
  val dcl = {
    lazy val arg = var_decl ~ many(comma <~ whitespace <~ var_decl)

    def transArgs(z: (VardeclAST, List[VardeclAST])) = {
      z._1 :: z._2
    }

    val args = mapP(transArgs)(arg)
    val ret = typeP ~> whitespace ~ args

    def transRet(z: (String, List[VardeclAST])) = {
      new DeclAST(z._1, z._2)
    }

    mapP(transRet)(ret)
  }

  val void = {
    val voidS = pstring("void") ~> whitespace

    def transVoid(z: String) = {
      new ParamsAST(Nil,true)
    }
    mapP(transVoid)(voidS)
  }

  //  parm_types 	: 	void
  //              |	type id [ '[' ']' ] { ',' type id [ '[' ']' ] }
  val parm_types = {
    val arg = typeP ~> whitespace ~ id ~ opt(leftBracket ~ rightBracket) ~> whitespace

    def transArg(z: ((String, String), Option[(String, String)])) = {
      z._2 match {
        case Some(_) => new ParamAST(z._1._1, z._1._2, true)
        case None => new ParamAST(z._1._1, z._1._2, false)
      }
    }

    val argP = mapP(transArg)(arg)

    val args = argP ~ many(comma <~ whitespace <~ argP)

    def transRet(z: (ParamAST, List[ParamAST])) = {
      new ParamsAST(z._1 :: z._2,false)
    }

    mapP(transRet)(args) |
      void
  }

  def letter = {
    val letters = ('a' to 'z').toList ::: ('A' to 'Z').toList
    anyOf(letters)
  }


  def typeP = {
    val type_literal = List("int", "bool", "double","char")
    choice(type_literal.map(pstring(_))).setLabel("type")
  }

  //type id '(' parm_types ')' '{' { type var_decl { ',' var_decl } ';' } { stmt } '}'
  def func = {
    val funcSignature = whitespace <~ typeP ~> whitespaces ~ id ~>
      leftParen ~> whitespace ~
      parm_types ~>
      rightParen ~> whitespace

    def transFuncSignature(z: ((String, String),ParamsAST)) = {
      new FuncSignatureAST(z._1._2, z._1._1, z._2)
    }

    val funcSignatureP = mapP(transFuncSignature)(funcSignature)

    val declP = dcl ~> semicolon ~> whitespace

    val funcP = funcSignatureP ~>
      leftCurly ~> whitespace ~
      many(declP) ~
      many(stmt) ~>
      rightCurly ~> whitespace

    def transformer(z: ((FuncSignatureAST, List[DeclAST]), List[StmtAST])) = {
      new FunctionAST(z._1._1, z._1._2, z._2)
    }

    mapP(transformer)(funcP)
  }

  //{ dcl ';'  |  func }
  val prog = {
    val dclP = dcl ~> semicolon ~> whitespaces
    val ret = dclP.asInstanceOf[Parser[AST]] | func.asInstanceOf[Parser[AST]]
    many(ret)
  }
}
