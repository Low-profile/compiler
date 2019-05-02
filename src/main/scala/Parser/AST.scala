package Parser

import llvmir.ILInstructions._
import llvmir.Types._
import llvmir.{Function, Program}


case class AST(nodeType: String) {
  def codegen(prog: Program, NamedValues: Map[String, Identifier]):Map[String, Identifier] = {
    NamedValues
  }
}

class VardeclAST(id: String, array: Option[Int], nodeType: String = "vardecl") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "id: " + id + "\n"
    + "argsOrArray: " + array + "\n")

  def getVars = {
    (id, array)
  }
}

class VoidParamAST(nodeType: String = "voidparam") extends AST(nodeType) {
  override def toString: String = nodeType + "\n"
}

class ParamAST(typeS: String, id: String, arrayOrNot: Boolean, nodeType: String = "param") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "typeS: " + typeS + "\n"
    + "id: " + id + "\n"
    + "arrayOrNot: " + arrayOrNot + "\n")

  def getParam = (typeS, id)
}

class ParamsAST(args: List[ParamAST], isNull: Boolean, nodeType: String = "params") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")

  def getParams() = if (isNull) Nil else args.map(_.getParam)
}

class FuncSignatureAST(name: String, retType: String, args: ParamsAST, nodeType: String = "funcSign") extends AST(nodeType) {
  def getRettype = retType match {
    case "int" => TInt
  }

  def getName = name

  def getParams = args.getParams()

  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")
}

class DeclAST(typeS: String, Vars: List[VardeclAST], nodeType: String = "decl") extends AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "typeS: " + typeS + "\n"
    + "Vars: " + Vars + "\n")

  def codegen(func: Function, NamedValues: Map[String, Identifier]): Map[String, Identifier] = {

    def declsGen(decls: List[VardeclAST], NamedValues: Map[String, Identifier]): Map[String, Identifier] = {
      decls match {
        case head :: rest => {
          val (a, b) = head.getVars
          typeS match {
            case "int" => {
              b match {
                case Some(value) => {
                  val id = func.append(Alloca(TArray(value, TInt)))
                  func.append(Store(Const(0),id))
                  val updatedNV = NamedValues.updated(a, id)
                  declsGen(rest, updatedNV)
                }
                case None => {
                  val id = func.append(Alloca(TInt),a)
                  func.append(Store(Const(0),id))
                  val updatedNV = NamedValues.updated(a, id)
                  declsGen(rest, updatedNV)

                }
              }
            }
          }
        }
        case Nil => {
          NamedValues
        }
      }
    }

    declsGen(Vars, NamedValues)
    //    for (var_ <- Vars) {
    //      val (a, b) = var_.getVars
    //
    //      typeS match {
    //        case "int" => {
    //          b match {
    //            case Some(value) => {
    //              val id = func.append(Alloca(TArray(value, TInt)))
    //              NamedValues.updated(id.name, id)
    //            }
    //            case None => {
    //              val id = func.append(Alloca(TInt))
    //              NamedValues = NamedValues.updated(id.name, id)
    //            }
    //          }
    //        }
    //        case "bool" => {
    //          b match {
    //            case Some(value) => {
    //              val id = func.append(Alloca(TArray(value, TInt)))
    //              NamedValues.updated(id.name, id)
    //            }
    //          }
    //        }
    //
    //      }
    //    }
    //    NamedValues
  }
}

abstract class ExprAST(nodeType: String = "Expr") extends AST(nodeType) {
  def codegen(prog: Function, NamedValues: Map[String, Identifier]): Identifier= {
    Const(0)
  }
}


class Expr_AST(nodeType: String = "Expr_") extends AST(nodeType)

class BinopAST(op: String, right: ExprAST, rest: Option[Expr_AST], nodeType: String = "Binop") extends Expr_AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "op: " + op + "\n"
    + "right: " + right + "\n"
    + "rest: " + rest.toString)

  def getOp = {
    op
  }

  def getRight(func: Function, NamedValues: Map[String, Identifier]) = {
    right.codegen(func, NamedValues)
  }

  def codegen(left: Identifier, func: Function, NamedValues: Map[String, Identifier]): Identifier = {
    val cur = op match {
      case "+" => func.append(Add(left, right.codegen(func, NamedValues)))
    }

    rest match {
      case Some(expr) => {
        expr match {
          //          case rel :RelopAST => bin.
          case bin: BinopAST => {
            bin.codegen(cur,func,NamedValues)
          }
        }
        //          case log :LogicalopAST => log

      }
      case None => cur
    }
  }
}

class RelopAST(op: String, right: ExprAST, rest: Option[Expr_AST], nodeType: String = "Relop") extends Expr_AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "op: " + op + "\n"
    + "right: " + right + "\n"
    + "rest: " + rest.toString)

  def codegen(left: Identifier, func: Function, NamedValues: Map[String, Identifier]): Identifier = {
    val cur = op match {
      case "<" => func.append(Icmp(Comparisons.slt, left, right.codegen(func, NamedValues)))
    }

    rest match {
      case Some(expr) => {
        expr match {
          case rel :RelopAST => {
            rel.codegen(cur,func,NamedValues)
          }
          case bin: BinopAST => {
            bin.codegen(cur,func,NamedValues)
          }
        }
        //          case log :LogicalopAST => log

      }
      case None => cur
    }
  }
}

class LogicalopAST(op: String, right: ExprAST, rest: Option[Expr_AST], nodeType: String = "logicalop") extends Expr_AST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "op: " + op + "\n"
    + "right: " + right + "\n"
    + "rest: " + rest.toString)
}

class EpsilonAST(nodeType: String = "epsilon") extends Expr_AST(nodeType) {
  //  override def toString: String = (nodeType + "\n")

}

class NumberExprAST(value: Int, rest: Option[Expr_AST], nodeType: String = "Num") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "value: " + value + "\n"
    + "rest: " + rest.toString)

  override def codegen(func: Function, NamedValues: Map[String, Identifier]) = {
    rest match {
      case Some(expr) => {
        expr match {
          //          case rel :RelopAST => bin.
          case bin: BinopAST => {
            bin.codegen(Const(value),func, NamedValues)
          }
          //          case log :LogicalopAST => log
        }
      }
      case None => Const(value)
    }
  }
}

class MinusExprAST(value: ExprAST, rest: Option[Expr_AST], nodeType: String = "minus") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "value: " + value + "\n"
    + "rest: " + rest.toString)

  override def codegen(func: Function, NamedValues: Map[String, Identifier]) = {
    val cur = value.codegen(func,NamedValues)
    val ret = func.append(Sub(Const(0),cur))
    rest match {
      case Some(expr) => {
        expr match {
          //          case rel :RelopAST => bin.
          case bin: BinopAST => {
            bin.codegen(ret,func, NamedValues)
          }
          //          case log :LogicalopAST => log
        }
      }
      case None =>  ret

    }
  }

}

class NotExprAST(value: ExprAST, rest: Option[Expr_AST], nodeType: String = "not") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "value: " + value + "\n"
    + "rest: " + rest.toString)
}

class ParenExprAST(priorExpr: ExprAST, rest: Option[Expr_AST], nodeType: String = "paren") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "priorExpr: " + priorExpr + "\n"
    + "rest: " + rest.toString)
}

class VariableExprAST(id: String, argsOrArray: Option[ExprAST], rest: Option[Expr_AST], nodeType: String = "var") extends ExprAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "id: " + id + "\n"
    + "argsOrArray: " + argsOrArray + "\n"
    + "rest: " + rest.toString)

  override def codegen(func: Function, NamedValues: Map[String, Identifier]): Identifier = {
    argsOrArray match {
      case None => {
        val cur = NamedValues(id)
        val dest = func.append(Load(cur))
        rest match {
          case Some(expr) => {
            expr match {
              case rel :RelopAST => {
                rel.codegen(dest,func,NamedValues)
              }
              case bin: BinopAST => {
                bin.codegen(dest,func,NamedValues)
              }
            }
            //          case log :LogicalopAST => log

          }
          case None => dest
        }
      }
      case Some(expr) => {
        expr match {
          case args:ArgsAST => {
            val callFunc = NamedValues(id)
            val args_ = args.getArgs(func,NamedValues)
            val dest = func.append(Call(callFunc,args_))
            rest match {
              case Some(expr) => {
                expr match {
                  case rel :RelopAST => {
                    rel.codegen(dest,func,NamedValues)
                  }
                  case bin: BinopAST => {
                    bin.codegen(dest,func,NamedValues)
                  }
                }
                //          case log :LogicalopAST => log

              }
              case None => dest
            }

          }
          case _:ExprAST =>{
            val cur = NamedValues(id)
            val dest = func.append(Load(cur))
            rest match {
              case Some(expr) => {
                expr match {
                  case rel :RelopAST => {
                    rel.codegen(dest,func,NamedValues)
                  }
                  case bin: BinopAST => {
                    bin.codegen(dest,func,NamedValues)
                  }
                }
                //          case log :LogicalopAST => log

              }
              case None => dest
            }
          }
        }
      }

    }
  }
}

abstract class StmtAST(nodeType: String = "Stmt") extends AST(nodeType) {
  def codegen(func: Function, NamedValues: Map[String, Identifier])
}

class SemicolonAST(nodeType: String = ";") extends StmtAST(nodeType) {
  override def toString: String = (nodeType + "\n")

  def codegen(func: Function, NamedValues: Map[String, Identifier])= {
  }
}

class RetAST(ret: Option[ExprAST], nodeType: String = "return") extends StmtAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "ret: " + ret + "\n")

  def codegen(func: Function, NamedValues: Map[String, Identifier]) = {
    ret match {
      case None => func.append(RetVoid)
      case Some(ret) => {
        val retVal = ret.codegen(func,NamedValues);
        func.append(Ret(retVal))
      }
    }
  }
}

class AssignAST(left: String, array: Option[ExprAST], right: ExprAST, nodeType: String = "assignment") extends StmtAST(nodeType) {
  override def toString: String = (nodeType + "\n"
    + "left: " + left + "\n"
    + "array: " + array + "\n"
    + "right: {\n" + right + "}")

  override def codegen(func: Function, NamedValues: Map[String, Identifier])= {
    val loaded = func.append(Load(NamedValues(left)))
    val rightID = right.codegen(func,NamedValues)
    func.append(Store(rightID,loaded))
  }
}

class FunctionAST(funcSign: FuncSignatureAST, decls: List[DeclAST], stmts: List[StmtAST]) extends AST("Function") {

  override def toString: String = (nodeType + "\n"
    + "funcSign: " + funcSign + "\n"
    + "decls: " + decls + "\n"
    + "stmts: {\n" + stmts + "}")

  override def codegen(prog: Program, NamedValues: Map[String, Identifier]): Map[String, Identifier] = {



    val args = funcSign.getParams.map(
      s => s._1 match {
        case "int" => {
          (TInt, s._2)
        }
        case "char" => {
          (TChar, s._2)
        }
      }
    )
    val func = prog.addStatic(funcSign.getName, args, funcSign.getRettype)

    def declsGen(decls: List[DeclAST], NamedValues: Map[String, Identifier]): Map[String, Identifier] = {
      decls match {
        case head :: rest => {
          val updatedNV = head.codegen(func, NamedValues)
          declsGen(rest, updatedNV)
        }
        case Nil => {
          NamedValues
        }
      }
    }

    def paramsGen(args: List[(String, String)], NamedValues: Map[String, Identifier]): Map[String, Identifier] = {
      args match {
        case head :: rest => {
          head._1 match{
            case "int" => {
              val id = func.append(Alloca(TInt),head._2)
              func.append(Store(Const(0),id))
              val updatedNV = NamedValues.updated(head._2, id)
              paramsGen(rest, updatedNV)
            }
          }

//          val namedValues = NamedValues.updated(head._2, Null)
//          paramsGen(rest, namedValues)
        }
        case Nil => {
          NamedValues
        }
      }
    }

    val namedValues = paramsGen(funcSign.getParams, NamedValues)
    val updatedNV = declsGen(decls, namedValues)
//    stmtsGen(stmts, updatedNV)
    for(stmt <- stmts){
      stmt.codegen(func,updatedNV)
    }
    val funcID = Global(TPointer(func.functionType),func.name)
    NamedValues.updated(funcSign.getName,funcID)
    //    val s = Icmp(Comparisons.ne, Const(1), Const(3)) +>
    //      ( BrCond(_, "left", "right") ) ::
    //      Label("left") ::
    //      Ret(Const(1)) ::
    //      Label("right") ::
    //      Ret(Const(0))
    //
    //    func.append(s)

    //    for (stmt <- stmts) {
    //      stmt.codegen(func, NamedValues)
    //    }
  }

}

class ConditionStmtAST(cond: ExprAST, thenStmt: StmtAST, elesStmt: StmtAST, tok: String = "Cond") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "cond:{\n" + cond + "}\n"
    + "thenStmt: " + thenStmt
    + "elesStmt: " + elesStmt)

  def codegen(func: Function, NamedValues: Map[String, Identifier])= {
    val CondIR = cond.codegen(func,NamedValues)
    val cmp = func.append(Icmp(Comparisons.ne, CondIR, Const(0)))
    val trueLabel = func.getFreshName("true")
    val falseLabel = func.getFreshName("false")
    val endLabel = func.getFreshName("end")

    func.append(BrCond(cmp,trueLabel ,falseLabel))

    func.append(Label(trueLabel))
    thenStmt.codegen(func,NamedValues)
    func.append(Br(endLabel))


    func.append(Label(falseLabel))
    elesStmt.codegen(func,NamedValues )
    func.append(Br(endLabel))

    func.append(Label(endLabel))
  }
}

class WhileAST(cond: ExprAST, stmts: StmtAST, tok: String = "While") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "cond:{\n" + cond + "}\n"
    + "stmts: " + stmts)

  def codegen(func: Function, NamedValues: Map[String, Identifier]) = {
    val condLabel = func.getFreshName("cond")
    func.append(Br(condLabel))
    func.append(Label(condLabel))

    val CondIR = cond.codegen(func,NamedValues)
    val cmp = func.append(Icmp(Comparisons.ne, CondIR, Const(0)))
    val trueLabel = func.getFreshName("true")
    val endLabel = func.getFreshName("end")

    func.append(BrCond(cmp,trueLabel ,endLabel))

    func.append(Label(trueLabel))
    stmts.codegen(func,NamedValues)
    func.append(Br(condLabel))

    func.append(Label(endLabel))
  }
}

class ForAST(init: Option[AssignAST], cond: Option[ExprAST], step: Option[AssignAST], stmts: StmtAST, tok: String = "for") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "init:{\n" + init + "}\n"
    + "cond:{\n" + cond + "}\n"
    + "step:{\n" + step + "}\n"
    + "stmts: " + stmts)

  def codegen(func: Function, NamedValues: Map[String, Identifier]) = {
    init match {
      case Some(assign) => assign.codegen(func,NamedValues)
      case None =>
    }

    val trueLabel = func.getFreshName("true")

    cond match {
      case Some(expr) =>{
        val condLabel = func.getFreshName("cond")
        func.append(Br(condLabel))
        func.append(Label(condLabel))
        val CondIR = expr.codegen(func,NamedValues)
        val cmp = func.append(Icmp(Comparisons.ne, CondIR, Const(0)))
        val endLabel = func.getFreshName("end")
        func.append(BrCond(cmp,trueLabel ,endLabel))

        func.append(Label(trueLabel))
        stmts.codegen(func,NamedValues)
        step match {
          case Some(expr_) => expr_.codegen(func,NamedValues)
          case None =>
        }
        func.append(Br(condLabel))

        func.append(Label(endLabel))
      }
      case None => {
        func.append(Br(trueLabel))
        func.append(Label(trueLabel))
        stmts.codegen(func,NamedValues)
        step match {
          case Some(expr_) => expr_.codegen(func,NamedValues)
          case None =>
        }
        func.append(Br(trueLabel))
      }
    }


  }
}

class CallAST(id: String, args: Option[List[ExprAST]], tok: String = "Call") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "id:{\n" + id + "}\n"
    + "args: " + args + "\n")

  def codegen(func: Function, NamedValues: Map[String, Identifier])= {
    args match {
      case None => func.append(CallVoid(NamedValues(id),Nil))
      case Some(exprs) => {
        val args = exprs.map(_.codegen(func,NamedValues))
        func.append(Call(NamedValues(id),args))
      }
    }
  }
}

class ArgsAST(args: Option[List[ExprAST]], tok: String = "args") extends ExprAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "args: " + args + "\n")

  def getArgs(func: Function, NamedValues: Map[String, Identifier]): List[Identifier] = {
    args match {
      case None => {
        Nil
      }
      case Some(list_Expr) => {
        list_Expr.map(_.codegen(func,NamedValues))
      }
    }
  }
}

class BlockStmtAST(stmts: List[StmtAST], tok: String = "blockStmts") extends StmtAST(tok) {
  override def toString: String = (nodeType + "\n"
    + "blockstmts: " + stmts + "\n")

  def codegen(func: Function, NamedValues: Map[String, Identifier])= {
    for(stmt <- stmts){
      stmt.codegen(func,NamedValues)
    }
  }
}