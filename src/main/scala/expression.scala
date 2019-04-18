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



//  expr' | 	binop expr expr'
//        | 	relop expr expr'
//        | 	logical_op expr expr'
  //      |   epsilon

  def epsilon = returnP("")

  val expr_ :Parser[String] = {
    val expr_List = binop ~ expr ~ expr_
    def transformer(z:((String,String), String)) = {
      z._1._1 + z._1._2 + z._2
    }
    val binopExpr  = mapP(transformer)(expr_List)
    epsilon | binopExpr
  }

  //  expr 	: 	'–' expr expr'
  //        | 	'!' expr expr'
  //        | 	id [ '(' [expr { ',' expr } ] ')' | '[' expr ']' ] expr'
  //        | 	'(' expr ')' expr'
  //        | 	intcon expr'
  //        |	  charcon expr'
  //        |	  stringcon expr'
  val expr = {
    val rest = expr_
    val intcon = digits ~ rest

    def transformer(z:(String,String)) = {
      z._1 + z._2
    }

    mapP(transformer)(intcon)
  }
}
