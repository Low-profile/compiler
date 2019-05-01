import llvmir.Types._
import llvmir.{Function, Program}

import llvmir._
import llvmir.Types._
import llvmir.ILInstructions._
import llvmir.AbstractILInstructions.OOP._
import llvmir.OperationChains._


case class AST(nodeType:String) {
  def codegen(prog: Program, NamedValues: Map[String, Any]) = {
  }
}
class VardeclAST(id:String, array :Option[Int], nodeType:String = "vardecl") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "id: " + id + "\n"
    + "argsOrArray: " + array + "\n")

  def getVars ={
    (id,array)
  }
}

class VoidParamAST(nodeType:String = "voidparam") extends AST(nodeType) {
  override def toString: String = nodeType + "\n"
}

class ParamAST(typeS:String, id :String, arrayOrNot:Boolean, nodeType:String = "param") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "typeS: " + typeS + "\n"
    + "id: " + id + "\n"
    + "arrayOrNot: " + arrayOrNot + "\n")
  def getParam = (typeS,id)
}

class ParamsAST( args:List[ParamAST], isNull:Boolean, nodeType:String = "params") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")
  def getParams() = if (isNull) Nil else args.map(_.getParam)
}

class FuncSignatureAST(name :String, retType:String,args:ParamsAST, nodeType:String = "funcSign") extends AST(nodeType) {
  def getRettype = retType match {
    case "int" => TInt
  }
  def getName = name
  def getParams = args.getParams()
  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")
}

class DeclAST(typeS:String, Vars :List[VardeclAST], nodeType:String = "decl") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "typeS: " + typeS + "\n"
    + "Vars: " + Vars + "\n")

  def codegen(func: Function, NamedValues: Map[String, Any]): Unit = {
    for (var_ <- Vars){
      val (a,b)= var_.getVars
      b match {
        case Some(value)=> {
          a match {
            case "int" => func.append(Alloca(TArray(value,TInt)))
            case "bool" => func.append(Alloca(TArray(value,TBool)))
          }
        }
        case None=>{
          a match {
            case "int" => func.append(Alloca(TInt))
            case "bool" => func.append(Alloca(TBool))
          }
        }
      }
    }
  }
}

class ExprAST(nodeType:String = "Expr") extends AST(nodeType)

class Expr_AST(nodeType:String = "Expr_") extends AST(nodeType)

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

  override def codegen(prog:Program, NamedValues:Map[String,Any]) ={
    val args = funcSign.getParams.map(
      s => s._1 match {
        case "int" => (TInt,"int")
        case "char" => (TChar,"char")
      }
    )
    val func = prog.addStatic(funcSign.getName,args,funcSign.getRettype)
    for (decl <- decls){
      decl.codegen(func,NamedValues)
    }
    for (stmt <- stmts){
      stmt.codegen(prog,NamedValues)
    }
  }

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