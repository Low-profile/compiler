package llvmir

import Types._

class Program extends ConstantPool {
  private[llvmir] val sp: StringPool = new StringPool()
  def string(const: String) = sp.string(const)

  private[llvmir] var classes: Map[String, Class] = Map()
  private[llvmir] var statics: List[Function] = Nil

  def addClass(className: String, parentName: Option[String]): Class = {
    val cls = new Class(className, parentName, this)
    classes += (className -> cls)

    cls
  }

  def addStatic( name: String,
                 args: List[(Type, String)],
                 retTpe: Type
               ): Function = {
    val fnc = new Function(name, args, retTpe, sp)
    statics = statics ::: (fnc :: Nil)

    fnc
  }

  def addMain(): Function = {
    val fnc = new Function("main", Nil, TInt, sp)
    statics = fnc :: statics

    fnc
  }

  def writeToFile(fileName: String) = {
    import java.io._

    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))

    try {
      ILPrinter(this).foreach({line => bw.write(line); bw.write("\n")})
    } finally {
      bw.close()
    }
  }
}

