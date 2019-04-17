import scala.language.implicitConversions

import scala.io.Source


object test {

  case class Success[T](result: T, rest: InputState) {
    def printR = print(result)
  }
  case class Failure(label:String,msg: String, pos:ParserPosition){
    def printR= {
      val errorLine = pos.currentLine
      val colPos = pos.col
      val linePos = pos.line
      val failureCaret =  " "*colPos + "^"+ msg
      println("Line:", linePos, "Col:", colPos,"Error parsing", label, "\n", errorLine,"\n", failureCaret)
    }
  }

  case class Parser[T](innerFn: InputState => Either[Failure,Success[T]],label:String ) {
    //andThen
    def ~[S](that: Parser[S]) = {
      def innerFn(input: InputState) = {
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

      new Parser(innerFn,label)
    }

    //orElse
    def |(that: Parser[T]) = {

      def innerFn(input: InputState) = {
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

      new Parser(innerFn,label)
    }

    def setLabel(newLabel:String) = {
      def newInnerFn (input:InputState)= {
        val result =  innerFn(input)
        result match {
          case Right(s) => Right(s)
          case Left(Failure(oldLabel,err,pos)) => Left(Failure (newLabel,err,pos))
        }
      }
      Parser(newInnerFn,newLabel)
    }

  }

  case class Position(line:Int,col:Int){
    def incrCol = Position(line,col+1)
    def incrLine = Position(line+1,col)
  }

  case class InputState(lines:List[String],position:Position){
    def currentLine ={
      val linePos = position.line
      if (linePos < lines.length)
        lines(linePos)
      else
        "end of file"
    }

    def nextChar = {
      val linePos = position.line
      val colPos = position.col
      if (linePos >= lines.length)
        (this, None)
      else{
        val curLine = currentLine
        if (colPos < curLine.length){
          val char = currentLine(colPos)
          val newPos = position.incrCol
          val newState = this.copy(position=newPos)
          (newState, Some(char))
        }
        else{
          val char = '\n'
          val newPos = position.incrLine
          val newState = this.copy(position=newPos)
          (newState, Some(char))
        }
      }

    }

    def toParserPosition={
      ParserPosition(currentLine,position.line,position.col)
    }
  }

  case class ParserPosition(currentLine : String,line:Int,col:Int)


  def readAllChars (input:InputState):List[Char]={
    val (remainingInput,charOpt) = input.nextChar
    charOpt match{
      case None => List()
      case Some(ch) => {
        ch :: readAllChars(remainingInput)
      }
    }
  }

  def run[T] (parser:Parser[T]) (input:InputState) =
    // call inner function with input
    parser.innerFn(input)


//  def run[T](parser: Parser[T])(input: String) = {
//    // unwrap parser to get inner function
//    val innerFn = parser.innerFn
//    // call inner function with input
//    innerFn(input)
//  }

  def orElse[T](l: Parser[T], r: Parser[T]) = {
    l | r
  }

//  def andThen[T,S](l: Parser[T], r: Parser[S]) = {
//    l ~ r
//  }

  def andThen[T,S](l: Parser[T], r: Parser[S]) = {
    def parse1(p1:T) ={
      def parse2(p2:S) = {
        returnP((p1,p2))
      }
      bindP(parse2)(r)
    }
    bindP(parse1)(l)
  }

  def choice[T](listOfParsers: List[Parser[T]]) =
    listOfParsers.reduce(orElse[T])

  //  def mapP[B, T](f: T => B)(parser: Parser[T]) = {
//    val innerFn = (input: String) => {
//      val result = run(parser)(input)
//      result match {
//        case Left(f) => Left(f)
//        case Right(Success(s, rest)) => {
//          val newValue = f(s)
//          Right(Success(newValue, rest))
//        }
//      }
//
//    }
//
//    new Parser(innerFn)
//
//  }

  def mapP [B, T](f: T => B) ={
    //scala andThen
    val composed = f andThen returnP
    bindP (composed) _
  }

  def returnP[T] (x:T) ={
    val label = "unknown"
    def innerFn(input:InputState) = {
      Right(Success(x,input))
    }
    new Parser(innerFn,label)
  }

//  def applyP[A,B] (x:Parser[A=>B]) (y:Parser[A]) ={
//    def transformer(s: (A=>B, A)):B =
//      s match {
//        case (a, b) => a(b)
//      }
//    val tmp = x ~ y
//    mapP(transformer)(tmp)
//  }

  def applyP[A,B] (x:Parser[A=>B]) (y:Parser[A]) = {
    def parse1(x:A=>B) ={
      def parse2(y:A) = {
        returnP(x(y))
      }
      bindP(parse2)(y)
    }
    bindP(parse1)(x)
  }


  def lift2[A,B,C] (f:A=>B=>C)(x:Parser[A]) (y:Parser[B]) ={
    val lift = returnP(f)
    val trans = applyP(lift)(x)
    applyP(trans)(y)
  }


  def parseStartsWith (str:String) (prefix:String) =
    str.startsWith(prefix)


  def startsWithP = lift2(parseStartsWith) _

  def sequence[A](parserList:List[Parser[A]]): Parser[List[A]] ={
    def cons (head:A)(tail:List[A]) = head::tail
    def consP = lift2 (cons) _

    parserList match {
      case Nil => returnP(List[A]())
      case head :: tail => consP(head) (sequence (tail))
    }
  }


  def charListToStr (charList:List[Char]) =
    charList.mkString



  def parseZeroOrMore[T](parser:Parser[T])(input:InputState):(List[T],InputState) ={
    def firstResult = run(parser)(input)
    firstResult match {
     case Left(f) => (List[T](),input)
     case Right(Success(firstValue,inputAfterFirstParse)) => {
       val (subsequentValues,remainingInput) = parseZeroOrMore(parser)(inputAfterFirstParse)
       def values:List[T] = firstValue::subsequentValues
       (values,remainingInput)
     }
    }
  }

  def many[T] (parser:Parser[T]) = {
    def innerFn (input:InputState) = {
      val (result,rest) = parseZeroOrMore (parser)(input)
      Right(Success(result,rest))
    }
    Parser(innerFn,"Unkown")
  }

  def many1[T] (parser:Parser[T]) = {

    def innerFn (input:InputState) = {
      def firstResult = run(parser)(input)
      firstResult match {
        case Left(f) => Left(f)
        case Right(Success(firstValue,inputAfterFirstParse)) => {
          val (subsequentValues,remainingInput) = parseZeroOrMore(parser)(inputAfterFirstParse)
          def values:List[T] = firstValue::subsequentValues
          Right(Success(values,remainingInput))
        }
      }
    }
    Parser(innerFn, "Unknown")
  }


//  def many1[T] (parser:Parser[T]) = {
//    def parse1(head:T) ={
//      def parse2(tail:T) = {
//        returnP(head::tail::Nil)
//      }
//      many(bindP(parse2)(parser))
//    }
//    bindP(parse1)(parser)
//  }


  /// Keep only the result of the left side parser
  def omitRight[T,S] (p1:Parser[T]) (p2:Parser[S]) ={
    def transformer(z:(T,S)):T =
      z match {
        case (a, b) => a
      }
    // create a pair
    mapP (transformer) (andThen(p1,p2))
  }

  /// Keep only the result of the left side parser
  def omitLeft[T,S] (p1:Parser[T]) (p2:Parser[S]) ={
    def transformer(z:(T,S)):S =
      z match {
        case (a, b) => b
      }
    // create a pair
    mapP (transformer) (andThen(p1,p2))
  }

  def opt[T] (p:Parser[T]):Parser[Option[T]] ={

    def transformerSome(z:T):Option[T] = Some(z)
    def transformerNone(z:T):Option[T] = None



    val some = mapP(transformerSome)(p)
    val none = mapP(transformerNone)(p)
    orElse(some,none)
  }

  def between[A,B,C] (p1:Parser[A])(p2:Parser[B])(p3:Parser[C]) =
    omitRight(omitLeft(p1)(p2))(p3)

  def bindP[A,B] (f:A=>Parser[B]) (p:Parser[A]) = {
    val label = "unknown"
    def innerFn (input:InputState) = {
      def result1 = run(p)(input)
      result1 match {
        case Left(f) => Left(f)
        case Right(Success(value1,remainingInput)) => {
          val p2 = f(value1)
          run (p2) (remainingInput)
        }
      }
    }
    Parser(innerFn,label)
  }



  def main(args: Array[String]): Unit = {

//    def pchar(charToMatch: Char) = {
//
//      def innerFn (s: String) = {
//        if (s.isEmpty)
//          Left(Failure("No more input"))
//        else {
//          val first = s.head
//          if (first == charToMatch) {
//            val remaining = s.tail
//            Right(Success(charToMatch, remaining))
//          }
//          else
//            Left(Failure("Expecting " + charToMatch + ". Got " + first))
//        }
//      }
//
//      new Parser(innerFn)
//
//    }

    def satisfy (predicate:Char=>Boolean) (label:String) ={
      def innerFn(input:InputState) = {
        val (remainingInput,charOpt) = input.nextChar
        charOpt match{
          case None => {
            val err = "No more input"
            val pos = input.toParserPosition
            Left(Failure (label,err,pos))
          }
          case Some(first) =>{
            if (predicate(first))
              Right(Success (first,remainingInput))
            else{
              val err = "Unexpected '" + first +"'"
              val pos = input.toParserPosition
              Left(Failure (label,err,pos))
            }
          }
        }
      }
      Parser(innerFn,label)
    }

    def pchar(charToMatch: Char) = {

      def predicate(ch:Char) = {
        ch == charToMatch
      }

      val label = "" + charToMatch

      satisfy(predicate)(label)
    }

    def anyOf(listOfChars: List[Char]) =
      choice(listOfChars.map(pchar))

    // match a specific string
    def pstring (str:String) =
      mapP (charListToStr) (sequence(str.toList.map(pchar)))

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

    def parsers = pchar('A'):: pchar('B'):: pchar('C') :: Nil

    def combined = sequence(parsers)

    def parseABC = pstring ("ABC")

    def manyA = many (pchar ('A'))

    def manyAB = many(pstring ("AB"))

    def whitespaceChar = anyOf (' '::'\t'::'\n'::Nil)

    def whitespace = many (whitespaceChar)

    // define parser for one digit
    def digit = anyOf(('0' to '9').toList)

    def digitThenSemicolon = andThen(digit, opt(pchar (';')))

    // define parser for one or more digits
    def digits = many1 (digit)


    def parseAB =
      (pchar('A') ~ pchar ('B')).setLabel("AB")

    val inputSource = Source.fromFile("source.c")

    val inputList = inputSource.getLines.toList
    inputSource.close()

    val initState = InputState(inputList,Position(0,0))


    def jUnescapedChar ={
      val label = "char"
      def trans(ch:Char) ={
        ch != '\\' && ch != '\"'
      }
      satisfy (trans) (label)

    }


    run(jUnescapedChar)(initState.copy(inputList=("i"::Nil))) match {
      case Left(s) => s.printR
      case Right(s) => s.printR
    }


    run(jUnescapedChar)(initState) match {
      case Left(s) => s.printR
      case Right(s) => s.printR
    }

//    println(run(digits)("aa111,23A"))

  }

}