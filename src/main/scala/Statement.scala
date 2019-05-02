import parser._
import CharP._
import Expression.expr

object Statement {

  //  | 	'{' { stmt } '}'
  val blockStmt = {
    val ret = leftCurly <~ whitespace <~ many1(stmt) ~> whitespace ~> rightCurly ~> whitespace

    def transRet(z:List[StmtAST]) = {
      new BlockStmtAST(z)
    }
    mapP(transRet)(ret)

  }

  //  | 	';'
  val semicolon = pstring(";")

  //  | 	return [ expr ] ';'
  val returnP = {
    val returnPS = pstring ("return")
    val ret = returnPS <~ whitespace <~ opt(expr) ~> semicolon ~> whitespace
    def transRet(z:Option[ExprAST]) = {
      new RetAST(z)
    }
    mapP(transRet)(ret)
  }

  //  | 	id '(' [expr { ',' expr } ] ')' ';'
  val callP = {
    lazy val args = expr ~many(comma <~ whitespace <~expr)
    val ret = id ~> leftParen ~> whitespace ~ opt(args) ~> whitespace ~> rightParen ~> whitespace  ~>semicolon ~> whitespace

    def transRet(z:(String,Option[(ExprAST,List[ExprAST])])) ={
      z._2 match {
        case Some((head,rest)) => new CallAST(z._1,Some(head::rest))
        case None => new CallAST(z._1, None)
      }
    }

    mapP(transRet)(ret)
  }

  //  | 	id [ '[' expr ']' ] = expr ';'
  val assg:Parser[AssignAST] = {

    val array = leftBracket ~> whitespace <~ expr ~> whitespace ~> rightBracket ~> whitespace

    val assgP = id ~ opt(array) ~> equalP ~> whitespace ~ expr ~>  whitespace

    def transassgP(z:((String,Option[ExprAST]),ExprAST)) = {
      new AssignAST(z._1._1, z._1._2, z._2)
    }

    mapP(transassgP)(assgP)
  }

  //  stmt 	: 	if '(' expr ')' stmt [ else stmt ]
  val conditionStmt = {
    val ifP = pstring("if")
    val cond = ifP <~ whitespace <~ leftParen <~ whitespace <~ expr ~>  whitespace ~> rightParen ~>  whitespace
    lazy val condStmt = stmt ~> whitespace
    val elseP = pstring("else")
    val elseStmt = elseP <~ whitespace <~ stmt ~> whitespace
    val conditionP = cond ~ condStmt ~ elseStmt

    def transConditionP(z:((ExprAST,StmtAST),StmtAST)) = {
      new ConditionStmtAST(z._1._1, z._1._2, z._2)
    }

    mapP[StmtAST,((ExprAST,StmtAST),StmtAST)](transConditionP)(conditionP)
  }

  //  | 	while '(' expr ')' stmt
  val whileStmt = {
    val whileP = pstring("while")
    val cond = whileP <~ whitespace <~ leftParen <~ whitespace <~ expr ~>  whitespace ~> rightParen ~>  whitespace
    lazy val stmtP = stmt ~> whitespace

    val ret =  cond ~ stmtP

    def transRet(z:(ExprAST,StmtAST)) = {
      new WhileAST(z._1, z._2)
    }

    mapP(transRet)(ret)
  }

//    | 	for '(' [ assg ] ';' [ expr ] ';' [ assg ] ')' stmt
  val forStmt = {
    val forP = pstring("for")
    val init = opt(assg)
    val cond = opt(expr)
    val step = opt(assg)
    val condP = forP <~ whitespace <~ leftParen <~ whitespace <~
      init  ~>  semicolon ~>  whitespace ~
        cond  ~>  semicolon ~>  whitespace ~
        step  ~> rightParen ~>  whitespace

    lazy val stmtP = stmt ~> whitespace

    val ret =  condP ~ stmtP

    def transRet(z:((((Option[AssignAST],Option[ExprAST]),Option[AssignAST]),StmtAST))) = {
      new ForAST(z._1._1._1,z._1._1._2,z._1._2, z._2)
    }

    mapP(transRet)(ret)
  }


  val stmt : Parser[StmtAST] = {

    def transSemicolonP(z:String) = {
      new SemicolonAST("semicolon")
    }

    val ret =  mapP[StmtAST,String](transSemicolonP)(semicolon) |
      whileStmt.asInstanceOf[Parser[StmtAST]] |
      conditionStmt.setLabel("Condtion") |
      (assg  ~> semicolon ~> whitespace).asInstanceOf[Parser[StmtAST]] |
      returnP.asInstanceOf[Parser[StmtAST]] |
      callP.asInstanceOf[Parser[StmtAST]] |
      forStmt.asInstanceOf[Parser[StmtAST]] |
      blockStmt.asInstanceOf[Parser[StmtAST]]

//    def transRet(z:String) = {
//    }

    ret
  }

}
