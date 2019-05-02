package Parser

import Parser.CharP._
import Parser.parser.{Parser, many, many1, pstring, _}

object Expression {

  //  expr' : 	binop expr expr'
  val binopExpr = {
    def binop = {
      val binop_ListChar = anyOf(List('+','-','*','/'))
      def transformer(z:Char) = {
        z.toString
      }
      mapP(transformer)(binop_ListChar)
    }

    val expr_List = binop ~> whitespace ~ expr ~ expr_ ~> whitespace
    def transformer(z:((String,ExprAST), Option[Expr_AST])) = {
      new BinopAST(z._1._1, z._1._2, z._2)
    }
    mapP(transformer)(expr_List)

  }

  //        | 	relop expr expr'
  val relopExpr = {
    def relop = {
      val opratorPList = List("==","!=","<=","<",">=",">").map(pstring(_))
      choice(opratorPList)
    }
    val expr_List = relop ~> whitespace ~ expr ~ expr_ ~> whitespace
    def transformer(z:((String,ExprAST), Option[Expr_AST])) = {
      new RelopAST(z._1._1, z._1._2, z._2)
    }
    mapP(transformer)(expr_List)

  }

  //        | 	logical_op expr expr'
  val logicalopExpr = {
    def logicalop = {
      val opratorPList = List("&&","||").map(pstring(_))
      choice(opratorPList)
    }
    val expr_List = logicalop ~> whitespace ~ expr ~ expr_ ~> whitespace
    def transformer(z:((String,ExprAST), Option[Expr_AST])) = {
      new LogicalopAST(z._1._1, z._1._2, z._2)
    }
    mapP(transformer)(expr_List)

  }


  val expr_ :Parser[Option[Expr_AST]] = {
    val ret = binopExpr.asInstanceOf[Parser[Expr_AST]] |
      relopExpr.asInstanceOf[Parser[Expr_AST]] |
      logicalopExpr.asInstanceOf[Parser[Expr_AST]]
    opt(ret)
  }

  //        |	  charcon expr'
  //        |	  stringcon expr'
  val intcon = {
    // define parser for one or more digits
    def digits = {
      val digits_ListChar = many1 (digit)
      def transformer(z:List[Char]) = {
        z.mkString
      }
      mapP(transformer)(digits_ListChar)

    }
    val expr = digits ~> whitespace

    def transformer(z:String) = {
      z.toInt
    }
    //      [ExprAST,(String,Option[Expr_AST])]
    mapP(transformer)(expr).setLabel("Int")
  }


  //        | 	intcon expr'
  val intcon_ = {
    val intconS = intcon ~ expr_ ~>whitespace

    def transformer(z:(Int,Option[Expr_AST])) = {
      new NumberExprAST(z._1, z._2)
    }
//      [ExprAST,(String,Option[Expr_AST])]
    mapP(transformer)(intconS).setLabel("Int")
  }

  //  expr 	: 	'–' expr expr'
  val minusExpr = {
    def minusSign = pstring("-")

    val ret = minusSign ~> whitespace <~ expr ~> whitespace ~ expr_

    def transformer(z:(ExprAST,Option[Expr_AST])) = {
      new MinusExprAST(z._1, z._2)
    }

    mapP(transformer)(ret)
  }

  //        | 	'!' expr expr'
  val notExpr = {
    val notSign = pstring("!")
    val ret = notSign ~> whitespace <~ expr ~> whitespace ~ expr_

    def transformer(z:(ExprAST,Option[Expr_AST])) = {
      new NotExprAST(z._1, z._2)
    }
    mapP(transformer)(ret)
  }

  //        | 	'(' expr ')' expr'
  val parenExpr = {
    val ret = leftParen <~ whitespace <~ expr ~> rightParen ~> whitespace ~ expr_

    def transformer(z:(ExprAST,Option[Expr_AST])) = {
      new ParenExprAST(z._1, z._2)
    }
    mapP(transformer)(ret)
  }

  //        | 	id [ '(' [expr { ',' expr } ] ')' | '[' expr ']' ] expr'
  val variableExpr = {
    lazy val arg = expr ~many(comma <~ whitespace <~expr)
    val args = leftParen ~> whitespace <~ opt(arg) ~> whitespace ~> rightParen  ~> whitespace
    val array = leftBracket ~> whitespace <~ expr ~> whitespace ~> rightBracket ~> whitespace

    def transArgs(z:Option[(ExprAST,List[ExprAST])]) ={
      z match {
        case Some((head,rest)) => new ArgsAST(Some(head::rest))
        case None => new ArgsAST(None)
      }
    }

    val argsP = mapP(transArgs)(args)

    val ret = id ~ opt(argsP.asInstanceOf[Parser[ExprAST]] | array) ~ expr_


    def transformer(z:((String,Option[ExprAST]),Option[Expr_AST])) = {
      new VariableExprAST(z._1._1,z._1._2, z._2)
    }
    mapP(transformer)(ret)
  }


  val expr : Parser[ExprAST] = {
    intcon_.asInstanceOf[Parser[ExprAST]] |
      minusExpr.asInstanceOf[Parser[ExprAST]] |
      notExpr.asInstanceOf[Parser[ExprAST]] |
      parenExpr.asInstanceOf[Parser[ExprAST]] |
      variableExpr.asInstanceOf[Parser[ExprAST]]
  }
}
