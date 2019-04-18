import parser._
import  CharP._
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

  val stmt : Parser[StmtAST] = {
    val ifP = pstring("if")
    val cond = ifP <~ whitespace <~ leftParen <~ whitespace <~ expr ~>  whitespace ~> rigthParen ~>  whitespace
    lazy val condStmt = stmt ~> whitespace
    val elseP = pstring("else")

    val elseStmt = elseP <~ whitespace <~ stmt ~> whitespace

    val conditionP = cond ~ condStmt ~ elseStmt

    val semicolon = pstring(";")

    def transConditionP(z:((String,String),String)) = {
      z._1._1 + z._1._2+ z._2
    }

    val conditionStmt =  mapP(transConditionP)(conditionP)


    val ret =  semicolon | conditionStmt

    def transRet(z:String) = {
    }

    ret
  }

}
