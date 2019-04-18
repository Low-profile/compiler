case class AST(nodeType:String)


class ExprAST(nodeType:String = "Expr") extends AST(nodeType)

class Expr_AST(nodeType:String = "Expr") extends AST(nodeType)


class BinopAST(op: String, right: ExprAST, rest: Expr_AST,nodeType:String = "Binop") extends Expr_AST(nodeType){
  override def toString: String = (nodeType + "\n"
    + "op: " + op + "\n"
    + "right: " + right + "\n"
    + "rest: " + rest.toString)
}

class EpsilonAST(nodeType:String = "epsilon") extends Expr_AST(nodeType){
//  override def toString: String = (nodeType + "\n")

}

class NumberExprAST(value :Int, rest: Expr_AST, nodeType:String = "Num") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "value: " + value + "\n"
    + "rest: " + rest.toString)
}


/// FunctionAST - This class represents a function definition itself.
class StmtAST(nodeType:String = "Stmt") extends AST(nodeType)

class SemicolonAST(nodeType:String = ";") extends StmtAST(nodeType){
  override def toString: String = (nodeType + "\n")
}

class RetAST(ret:Option[ExprAST], nodeType:String = "return") extends StmtAST(nodeType){
  override def toString: String = ( nodeType + "\n"
    + "ret: " + ret + "\n")
}


class AssignAST(left:String, array:Option[ExprAST],right:ExprAST, nodeType:String = "assignment") extends StmtAST(nodeType){
  override def toString: String = ( nodeType + "\n"
    + "left: " + left + "\n"
    + "array: " + array + "\n"
    + "right: {\n" + right + "}")
}


class FunctionAST(name :String, retType:String, stmts:StmtAST) extends AST("Function")
{

  override def toString: String = ( nodeType + "\n"
    + "id: " + name + "\n"
    + "retType: " + retType + "\n"
    + "stmts: {\n" + stmts + "}")

}

class ConditionStmtAST(cond: ExprAST, thenStmt: StmtAST, elesStmt: StmtAST,tok:String = "Cond") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "cond:{\n" + cond + "}\n"
    + "thenStmt: " + thenStmt
    + "elesStmt: " + elesStmt)
}