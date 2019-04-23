case class AST(nodeType:String)

class VardeclAST(id:String, array :Option[ExprAST], nodeType:String = "vardecl") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "id: " + id + "\n"
    + "argsOrArray: " + array + "\n")
}

class VoidParamAST(nodeType:String = "voidparam") extends AST(nodeType) {
  override def toString: String = nodeType + "\n"
}

class ParamAST(typeS:String, id :String, arrayOrNot:Boolean, nodeType:String = "param") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "typeS: " + typeS + "\n"
    + "id: " + id + "\n"
    + "arrayOrNot: " + arrayOrNot + "\n")
}

class ParamsAST( args:List[ParamAST], nodeType:String = "params") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")
}

class FuncSignatureAST(name :String, retType:String,args:AST, nodeType:String = "funcSign") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")
}

class DeclAST(typeS:String, Vars :List[VardeclAST], nodeType:String = "decl") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "typeS: " + typeS + "\n"
    + "Vars: " + Vars + "\n")
}

class ExprAST(nodeType:String = "Expr") extends AST(nodeType)

class Expr_AST(nodeType:String = "Expr") extends AST(nodeType)

class BinopAST(op: String, right: ExprAST, rest: Option[Expr_AST],nodeType:String = "Binop") extends Expr_AST(nodeType){
  override def toString: String = (nodeType + "\n"
    + "op: " + op + "\n"
    + "right: " + right + "\n"
    + "rest: " + rest.toString)
}

class RelopAST(op: String, right: ExprAST, rest: Option[Expr_AST],nodeType:String = "Relop") extends Expr_AST(nodeType){
  override def toString: String = (nodeType + "\n"
    + "op: " + op + "\n"
    + "right: " + right + "\n"
    + "rest: " + rest.toString)
}

class LogicalopAST(op: String, right: ExprAST, rest: Option[Expr_AST],nodeType:String = "logicalop") extends Expr_AST(nodeType){
  override def toString: String = (nodeType + "\n"
    + "op: " + op + "\n"
    + "right: " + right + "\n"
    + "rest: " + rest.toString)
}

class EpsilonAST(nodeType:String = "epsilon") extends Expr_AST(nodeType){
//  override def toString: String = (nodeType + "\n")

}

class NumberExprAST(value :Int, rest: Option[Expr_AST], nodeType:String = "Num") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "value: " + value + "\n"
    + "rest: " + rest.toString)
}

class MinusExprAST(value :ExprAST, rest: Option[Expr_AST], nodeType:String = "minus") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "value: " + value + "\n"
    + "rest: " + rest.toString)
}

class NotExprAST(value :ExprAST, rest: Option[Expr_AST], nodeType:String = "not") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "value: " + value + "\n"
    + "rest: " + rest.toString)
}

class ParenExprAST(priorExpr :ExprAST, rest: Option[Expr_AST], nodeType:String = "paren") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "priorExpr: " + priorExpr + "\n"
    + "rest: " + rest.toString)
}

class VariableExprAST(id:String, argsOrArray :Option[ExprAST], rest: Option[Expr_AST], nodeType:String = "var") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "id: " + id + "\n"
    + "argsOrArray: " + argsOrArray + "\n"
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

class FunctionAST(funcSign :FuncSignatureAST, decls : List[DeclAST], stmts:List[StmtAST]) extends AST("Function") {

  override def toString: String = ( nodeType + "\n"
    + "funcSign: " + funcSign + "\n"
    + "decls: " + decls + "\n"
    + "stmts: {\n" + stmts + "}")

}

class ConditionStmtAST(cond: ExprAST, thenStmt: StmtAST, elesStmt: StmtAST,tok:String = "Cond") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "cond:{\n" + cond + "}\n"
    + "thenStmt: " + thenStmt
    + "elesStmt: " + elesStmt)
}

class WhileAST(cond: ExprAST, stmts: StmtAST,tok:String = "While") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "cond:{\n" + cond + "}\n"
    + "stmts: " + stmts)
}

class ForAST(init:Option[AssignAST], cond: Option[ExprAST],step:Option[AssignAST], stmts: StmtAST,tok:String = "for") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "init:{\n" + init + "}\n"
    + "cond:{\n" + cond + "}\n"
    + "step:{\n" + step + "}\n"
    + "stmts: " + stmts)
}

class CallAST(id: String, args: Option[List[ExprAST]],tok:String = "Call") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "id:{\n" + id + "}\n"
    + "args: " + args + "\n")
}

class ArgsAST(args: Option[List[ExprAST]],tok:String = "args") extends ExprAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")
}