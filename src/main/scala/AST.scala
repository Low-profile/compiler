case class ExprAST(nodeType:String)
{

}

class StmtAST
{

  // public:


  // virtual Value *codegen() = 0;

  public:
    virtual Value * codegen(std::map<std::string, AllocaInst *>& NamedValues) = 0;
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST(name :String, retType:String, stmts:StmtAST) extends Ast("Function")
{

  override def toString: String = ("function\n"
    + "id: " + name + "\n"
    + "retType: " + retType + " ")

  //  std::vector<std::tuple<std::string,std::string>> Args;
//  std::unique_ptr<StmtblockAST> Body;
//
//  public:
//    FunctionAST(const std::string &type, const std::string &Name, std::vector<std::tuple<std::string,std::string>> Args, std::unique_ptr<StmtblockAST> Body, int width = 64)
//  : type(type), Name(Name), Args(std::move(Args)), Body(std::move(Body)), width(width)
//{}
//
//  Function *codegen();
//
//  const std::string &getName() const { return Name; }

}


class StmtAST(tok:String, children: List[Ast]) extends Ast(tok,children)

class ConditionStmtAST(tok:String, children: List[Ast]) extends StmtAST(tok,children)