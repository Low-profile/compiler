case class Ast(tok:String,children: List[Ast])
{
  def appendChild(child:Ast)
  {
    this.copy(children = children :+ child)
  }
}

/// FunctionAST - This class represents a function definition itself.
class FunctionAST(name :String, retType:String, tok:String, children: List[Ast]) extends Ast(tok,children)
{
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