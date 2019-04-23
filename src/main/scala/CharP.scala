import FunctionS.letter
import parser._

object CharP {

  def leftParen = pstring("(")

  def rightParen = pstring(")")

  def leftCurly = pstring("{")

  def rightCurly = pstring("}")

  def leftBracket = pstring("[")

  def rightBracket = pstring("]")

  def equalP = pstring("=")

  def underline = pchar('_')

  def whitespace = {
    val whitespace_chars = many(whitespaceChar)

    def transformer(z: List[Char]) = z.mkString

    mapP(transformer)(whitespace_chars).setLabel("space")
  }

  def whitespaces = {
    val whitespace_chars = many1(whitespaceChar)

    def transformer(z: List[Char]) = z.mkString

    mapP(transformer)(whitespace_chars).setLabel("spaces")
  }

  def id ={
    val first_char = letter.setLabel("id_first")

    val rest_chars = many(choice(List(letter,digit,underline))).setLabel("id_rest")

    val ret = andThen(first_char, rest_chars)

    def transformer(z:(Char,List[Char])) = {
      (z._1 :: z._2).mkString
    }
    mapP(transformer)(ret)  ~> whitespace
  }

  def comma = pstring(",")
}
