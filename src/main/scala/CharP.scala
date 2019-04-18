import FunctionS.letter
import parser.{andThen, choice, digit, many, mapP, pchar, pstring}

object CharP {

  def leftParen = pstring("(")

  def rigthParen = pstring(")")

  def leftCurly = pstring("{")

  def rigthCurly = pstring("}")

  def leftBracket = pstring("[")

  def rigthBracket = pstring("]")

  def equalP = pstring("=")

  def underline = pchar('_')

  def id ={
    val first_char = letter.setLabel("id_first")

    val rest_chars = many(choice(List(letter,digit,underline))).setLabel("id_rest")

    val ret = andThen(first_char, rest_chars)

    def transformer(z:(Char,List[Char])) = {
      (z._1 :: z._2).mkString
    }
    mapP(transformer)(ret)
  }

}
