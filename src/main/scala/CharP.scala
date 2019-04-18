import parser.{pchar, pstring}

object CharP {

  def leftParen = pstring("(")

  def rigthParen = pstring(")")

  def leftCurly = pstring("{")

  def rigthCurly = pstring("}")

  def leftBracket = pstring("[")

  def rigthBracket = pstring("]")

  def underline = pchar('_')


}
