import parser._

object expression {
  // define parser for one or more digits
  def digits = {
    val digits_ListChar = many1 (digit)
    def transformer(z:List[Char]) = {
      z.mkString
    }
    mapP(transformer)(digits_ListChar)

  }

  def binop = {
    val binop_ListChar = anyOf(List('+','-','*','/'))
    def transformer(z:Char) = {
      z.toString
    }
    mapP(transformer)(binop_ListChar)
  }

  def minusSign = pstring("-")

  def epsilon = returnP("")


  //  expr' : 	binop expr expr'
//        | 	relop expr expr'
//        | 	logical_op expr expr'
  //      |   epsilon


  val expr_ :Parser[Expr_AST] = {
    val expr_List = binop ~> whitespace ~ expr ~ expr_ ~> whitespace
    def transformer(z:((String,ExprAST), Expr_AST)) = {
      new BinopAST(z._1._1, z._1._2, z._2)
    }
    val binopExpr  = mapP(transformer)(expr_List)


    def transEpsilon(z:String) = {
      new EpsilonAST()
    }

    binopExpr.asInstanceOf[Parser[Expr_AST]] | mapP[Expr_AST,String](transEpsilon)(epsilon)
  }

  //  expr 	: 	'–' expr expr'
  //        | 	'!' expr expr'
  //        | 	id [ '(' [expr { ',' expr } ] ')' | '[' expr ']' ] expr'
  //        | 	'(' expr ')' expr'
  //        | 	intcon expr'
  //        |	  charcon expr'
  //        |	  stringcon expr'
  val expr : Parser[ExprAST] = {
    val intcon = digits ~>whitespace ~ expr_ ~>whitespace

    def transformer(z:(String,Expr_AST)) = {
      new NumberExprAST(z._1.toInt, z._2)
    }

    mapP[ExprAST,(String,Expr_AST)](transformer)(intcon)
  }
}
