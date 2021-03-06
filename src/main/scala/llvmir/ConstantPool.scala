package llvmir

import ILInstructions._
import Types._
import Prefixes._
import scala.collection.mutable.HashMap

trait ConstantPool {
  def string(const: String): Identifier
}

private[llvmir] class StringPool extends ConstantPool {
  private val strings: HashMap[String, Identifier] = HashMap()
  private val prefix = Prefixes.string
  private var nextIndex: Int = 0

  def string(const: String): Identifier = strings.getOrElse(const, {
    val tpe = TPointer(TStruct(None, List(TInt, TArray(StringPool.normalize(const).length + 1, TChar))))
    val vlue = Global(tpe, "%s%d".format(prefix,nextIndex))
    nextIndex += 1
    strings += (const -> vlue)

    vlue
  })

  def allStrings = strings.toMap
}

object StringPool {
  def normalize(string: String): String = {
    import java.text.Normalizer

    val normalized = Normalizer.normalize(string, Normalizer.Form.NFD)
    val ascii = normalized.replaceAll("[^\\p{ASCII}]", "")
    ascii
  }
}
