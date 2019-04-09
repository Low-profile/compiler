import scala.collection.mutable
import scala.language.implicitConversions


//trait Parsers {
//  trait Parser extends (Stream[Char] => Boolean) {
//    def isNullable: Boolean
//
//    def derive(c: Char): Parser
//
//    def innerToString(visited: Set[Parser]): (String, Set[Parser])
//
//    def apply(str: Stream[Char]) : Boolean = str match {
//      case hd #:: tail => derive(hd)(tail)
//
//      case Stream() => isNullable
//    }
//
//    // ~ concat
//    def ~(that: => Parser): Parser = new ConcatParser(this, that)
//
//    override def toString : String = innerToString(Set())._1
//
//  }
//
//  trait MemoizedDerivation extends Parser {
//
//    protected def innerDerive(c: Char): Parser
//
//    private val derivations = mutable.Map[Char, Parser]()
//
//    final def derive(c: Char):Parser = derivations get c getOrElse {
//      val back = innerDerive(c)
//      derivations(c) = back
//
//      back
//    }
//
//  }
//
//  val epsilon: Parser = new Parser {
//    val isNullable = true
//
//    def derive(c: Char):Parser = empty
//
//    def innerToString(visited: Set[Parser]):(String,Set[Parser]) = ("<>", visited)
//  }
//
//  private val empty = new Parser {
//    val isNullable = false
//
//    def derive(c: Char):Nothing = sys.error("Cannot derive the empty parser")
//
//    override def ~(that: => Parser):Parser = this
//
//    def innerToString(visited: Set[Parser]):(String,Set[Parser]) = ("{}", visited)
//  }
//
//  case class LiteralParser(c: Char) extends Parser {
//    val isNullable = false
//
//    def derive(c: Char):Parser =
//      if (this.c == c) epsilon else empty
//
//    def innerToString(visited: Set[Parser]) : (String,Set[Parser]) = (c.toString, visited)
//  }
//
//  class UnionParser(_left: => Parser, _right: => Parser) extends MemoizedDerivation { //Parser with
//    lazy val left = _left
//    lazy val right = _right
//
//    private var _isNullable: Option[Boolean] = None
//
//    def isNullable:Boolean = _isNullable getOrElse {
//      _isNullable = Some(false)
//      var back = left.isNullable || right.isNullable
//      _isNullable = Some(back)
//
//      // try to achieve a fixpoint
//      while ((left.isNullable || right.isNullable) != back) {
//        back = left.isNullable || right.isNullable
//        _isNullable = Some(back)
//      }
//
//      back
//    }
//
//    def innerDerive(c: Char) = {
//      if (left == empty && right == empty)
//        empty
//      else if (left == empty)
//        right.derive(c)
//      else if (right == empty)
//        left.derive(c)
//      else
//        left.derive(c) | right.derive(c)
//    }
//
//    def innerToString(visited: Set[Parser]) = {
//      if (visited contains this)
//        (hashCode.toString, visited)
//      else {
//        val (leftStr, leftVisit) = left.innerToString(visited + this)
//        val (rightStr, rightVisit) = right.innerToString(leftVisit)
//        ("(| " + leftStr + " " + rightStr + ")", rightVisit)
//      }
//    }
//  }
//
//  class RichParser(left: => Parser) {
//    def |(right: => Parser): Parser = new UnionParser(left, right)
//  }
//
//
//  implicit def literal(c: Char): Parser = LiteralParser(c)
//
//  implicit def parser2rich(left: => Parser): RichParser = new RichParser(left)
//
//  implicit def unionSyntax(c: Char): RichParser = parser2rich(literal(c))
//
//
//  class ConcatParser(_left: => Parser, _right: => Parser) extends Parser {
//    lazy val left = _left
//    lazy val right = _right
//
//    def isNullable = left.isNullable && right.isNullable
//
//    def derive(c: Char) = {
//      if (left.isNullable)
//        left.derive(c) ~ right | right.derive(c)
//      else
//        left.derive(c) ~ right
//    }
//
//    def innerToString(visited: Set[Parser]) = {
//      val (leftStr, leftVisited) = left.innerToString(visited)
//      val (rightStr, rightVisited) = right.innerToString(visited)
//      ("(~ " + leftStr + " " + rightStr + ")", rightVisited)
//    }
//  }
//
//}
//
////class example extends Parsers {
////}
//
//
//object test extends Parsers {
//  def main(args: Array[String]): Unit = {
//
//    val r =  LiteralParser('a')
//    val s = s | r
//    val out = (r | r)("a".toStream)
//    println(out)
//    print(r)
//  }
//}


object test {

  //run :: the actual parser
  //Parser :: wrapper
  //A
  //  class Parser[A](run: String => List[(A, String)]) {
  //    def apply(s: String) = run(s)
  //
  ////    def flatMap[B](f: A => Parser[B]): Parser[B] = {
  ////      val runB = { s: String => run(s).flatMap { case (a, rest) => f(a)(rest) } }
  ////      new Parser(runB)
  ////    }
  ////
  ////    def map[B](f: A => B): Parser[B] = {
  ////      val runB = { s: String =>
  ////        run(s).map { case (a, rest) => (f(a), rest) }
  ////      }
  ////      new Parser(runB)
  ////    }
  //  }
  //
  //  //
  //  def success[A](a: A): Parser[A] = {
  //    val run = { s: String => List((a, s)) }
  //    new Parser(run)
  //  }
  //
  //  def failure[A](): Parser[A] = {
  //    val run = { s: String => Nil }
  //    new Parser[A](run)
  //  }
  //
  //  def item(): Parser[Char] = {
  //    val run = { s: String =>
  //      if (s.isEmpty) Nil else List((s.head, s.tail))
  //    }
  //    new Parser(run)
  //  }

  //  let pchar (charToMatch,str) =
  //    if String.IsNullOrEmpty(str) then
  //      let msg = "No more input"
  //  (msg,"")
  //  else
  //  let first = str.[0]
  //  if first = charToMatch then
  //    let remaining = str.[1..]
  //  let msg = sprintf "Found %c" charToMatch
  //  (msg,remaining)
  //  else
  //  let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
  //    (msg,str)



  case class Success[T](result: T, rest: String)

  case class Failure(msg: String)

  case class Parser[T](innerFn: String => Either[Failure,Success[T]]) {
    //andThen
    def ~[S](that: Parser[S]) = {
      def innerFn(input: String) = {
        val result1 = run(this)(input)
        result1 match {
          case Left(f) => Left(f)

          case Right(Success(s_1,rest)) => {
            val result2 = run(that)(rest)

            result2 match {
              case Left(f) => Left(f)

              case Right(Success(s_2,rest_2)) => {
                // combine both values as a pair
                val newValue = (s_1, s_2)
                // return remaining input after parser2
                Right(Success(newValue, rest_2))
              }
            }
          }
        }

      }

      new Parser(innerFn)
    }

    //orElse
    def |(that: Parser[T]) = {

      def innerFn(input: String) = {
        val result1 = run(this)(input)
        result1 match {
          case Left(f)=> {
            val result2 = run(that)(input)
            result2
          }
          case Right(s) => {
            Right(s)
          }
        }

      }

      new Parser(innerFn)
    }

  }

  def run[T](parser: Parser[T])(input: String) = {
    // unwrap parser to get inner function
    val innerFn = parser.innerFn
    // call inner function with input
    innerFn(input)
  }

  def orElse[T](l: Parser[T], r: Parser[T]) = {
    l | r
  }

  def andThen[T](l: Parser[T], r: Parser[T]) = {
    l ~ r
  }

  def choice[T](listOfParsers: List[Parser[T]]) =
    listOfParsers.reduce(orElse[T])

  def pchar(charToMatch: Char) = {

    def innerFn (s: String) = {
      if (s.isEmpty)
        Left(Failure("No more input"))
      else {
        val first = s.head
        if (first == charToMatch) {
          val remaining = s.tail
          Right(Success(charToMatch, remaining))
        }
        else
          Left(Failure("Expecting " + charToMatch + ". Got " + first))
      }
    }

    new Parser(innerFn)

  }

  def anyOf(listOfChars: List[Char]) =
    choice(listOfChars.map(pchar))

  def mapP[B, T](f: T => B)(parser: Parser[T]) = {
    val innerFn = (input: String) => {
      val result = run(parser)(input)
      result match {
        case Left(f) => Left(f)
        case Right(Success(s, rest)) => {
          val newValue = f(s)
          Right(Success(newValue, rest))
        }
      }

    }

    new Parser(innerFn)

  }



  def returnP[T] (x:T) ={
    def innerFn(input:String) = {
      Right(Success(x,input))
    }
    new Parser(innerFn)
  }

  def applyP[A,B] (x:Parser[A]) (y:Parser[B]) ={
    val transformer = (s: (A, B)) =>
      s match {
        case (a, b) => a(b)
      }
    val tmp = x ~ y
    mapP(transformer)(tmp)
  }

//  let applyP fP xP =
//    // create a Parser containing a pair (f,x)
//    (fP .>>. xP)
//  // map the pair by applying f to x
//  |> mapP (fun (f,x) -> f x)


  def main(args: Array[String]): Unit = {


    val parseLowercase = anyOf(('a' to 'z').toList)
    val parseDigit = anyOf(('0' to '9').toList)


    def parseThreeDigits = {
      // create a parser that returns a tuple
      def tupleParser =
        parseDigit ~ parseDigit ~ parseDigit

      // create a function that turns the tuple into a string
      val transformTuple = (s: ((Char, Char), Char)) =>
        s match {
          case ((s1, s2), s3) => "" + s1 + s2 + s3
        }

      // use "map" to combine them
      mapP(transformTuple)(tupleParser)

    }

    def parseThreeDigitsAsInt = {

      // create a function that turns the tuple into a string
      val transformStr = (s: String) =>
        s.toInt

      // use "map" to combine them
      mapP(transformStr)(parseThreeDigits)

    }


    println(run(parseThreeDigitsAsInt)("123A"))

  }

}