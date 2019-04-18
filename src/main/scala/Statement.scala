import parser._
import CharP._
import expression.expr

object Statement {
//  stmt 	: 	if '(' expr ')' stmt [ else stmt ]
      //  | 	while '(' expr ')' stmt
    //    | 	for '(' [ assg ] ';' [ expr ] ';' [ assg ] ')' stmt
      //  | 	return [ expr ] ';'
      //  | 	assg ';'
      //  | 	id '(' [expr { ',' expr } ] ')' ';'
      //  | 	'{' { stmt } '}'
      //  | 	';'

  val semicolon = pstring(";")

  val returnP = {
    val returnPS = pstring ("return")
    val ret = returnPS <~ whitespace <~ opt(expr) ~> semicolon
    def transRet(z:Option[ExprAST]) = {
      new RetAST(z)
    }
    mapP(transRet)(ret)
  }

  val assg:Parser[AssignAST] = {

    val array = leftBracket ~> whitespace <~ expr ~> whitespace ~> rigthBracket ~> whitespace

    val assgP = id ~> whitespace ~ opt(array) ~> equalP ~> whitespace ~ expr ~> semicolon ~> whitespace

    def transassgP(z:((String,Option[ExprAST]),ExprAST)) = {
      new AssignAST(z._1._1, z._1._2, z._2)
    }

    mapP(transassgP)(assgP)
  }

  val stmt : Parser[StmtAST] = {
    val ifP = pstring("if")
    val cond = ifP <~ whitespace <~ leftParen <~ whitespace <~ expr ~>  whitespace ~> rigthParen ~>  whitespace
    lazy val condStmt = stmt ~> whitespace
    val elseP = pstring("else")

    val elseStmt = elseP <~ whitespace <~ stmt ~> whitespace

    val conditionP = cond ~ condStmt ~ elseStmt


    def transConditionP(z:((ExprAST,StmtAST),StmtAST)) = {
      new ConditionStmtAST(z._1._1, z._1._2, z._2)
    }

    val conditionStmt =  mapP[StmtAST,((ExprAST,StmtAST),StmtAST)](transConditionP)(conditionP)


    def transSemicolonP(z:String) = {
      new SemicolonAST("semicolon")
    }

    val ret =  mapP[StmtAST,String](transSemicolonP)(semicolon) |
      conditionStmt.setLabel("Condtion") |
      assg.asInstanceOf[Parser[StmtAST]] |
      returnP.asInstanceOf[Parser[StmtAST]]

//    def transRet(z:String) = {
//    }

    ret
  }

}
