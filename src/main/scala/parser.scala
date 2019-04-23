import scala.language.implicitConversions

import scala.io.Source


object parser {

  //list version orElse
  def choice[T](listOfParsers: List[Parser[T]]): Parser[T] = listOfParsers.reduce(orElse[T])

  def andThen[T, S](l: => Parser[T], r: => Parser[S]): Parser[(T, S)] = {
    def parse1(p1: T) = {
      def parse2(p2: S) = {
        returnP((p1, p2))
      }

      bindP(parse2)(r)
    }

    bindP(parse1)(l)
  }

  //list version andThen
  def  e[A](parserList: List[Parser[A]]): Parser[List[A]] = {

    def applyP[A, B](x: Parser[A => B])(y: Parser[A]) = {
      def parse1(x: A => B) = {
        def parse2(y: A) = {
          returnP(x(y))
        }

        bindP(parse2)(y)
      }

      bindP(parse1)(x)
    }

    //lift function with 2 parameter
    def lift2[A, B, C](f: A => B => C)(x: Parser[A])(y: Parser[B]) = {
      val unit = returnP(f)
      val trans = applyP(unit)(x)
      applyP(trans)(y)
    }

    def cons(head: A)(tail: List[A]) = head :: tail

    def consP = lift2(cons) _

    parserList match {
      case Nil => returnP(List[A]())
      case head :: tail => consP(head)(sequence(tail))
    }
  }

  def parseZeroOrMore[T](parser: Parser[T])(input: InputState): (List[T], InputState) = {
    def firstResult = run(parser)(input)

    firstResult match {
      case Left(f) => (List[T](), input)
      case Right(Success(firstValue, inputAfterFirstParse)) => {
        val (subsequentValues, remainingInput) = parseZeroOrMore(parser)(inputAfterFirstParse)

        def values: List[T] = firstValue :: subsequentValues

        (values, remainingInput)
      }
    }
  }

  def many[T](parser: => Parser[T]) = {
    def innerFn(input: InputState) = {
      val (result, rest) = parseZeroOrMore(parser)(input)
      Right(Success(result, rest))
    }

    Parser(innerFn, "Unkown")
  }

  def many1[T](parser: Parser[T]) = {

    def innerFn(input: InputState) = {
      def firstResult = run(parser)(input)

      firstResult match {
        case Left(f) => Left(f)
        case Right(Success(firstValue, inputAfterFirstParse)) => {
          val (subsequentValues, remainingInput) = parseZeroOrMore(parser)(inputAfterFirstParse)

          def values: List[T] = firstValue :: subsequentValues

          Right(Success(values, remainingInput))
        }
      }
    }

    Parser(innerFn, "Unknown")
  }

  /// Keep only the result of the left side parser
  def omitRight[T, S](p1: Parser[T])(p2: Parser[S]) = {
    def transformer(z: (T, S)): T =
      z match {
        case (a, b) => a
      }
    // create a pair
    mapP(transformer)(andThen(p1, p2))
  }

  /// Keep only the result of the left side parser
  def omitLeft[T, S](p1: Parser[T])(p2: Parser[S]) = {
    def transformer(z: (T, S)): S =
      z match {
        case (a, b) => b
      }
    // create a pair
    mapP(transformer)(andThen(p1, p2))
  }

  def opt[T](p: Parser[T]): Parser[Option[T]] = {

    def transformerSome(z: T): Option[T] = Some(z)

    //def transformerNone(z: T): Option[T] = None
    //val none = mapP(transformerNone)(p)

    val some = mapP(transformerSome)(p)
    val none = returnP[Option[T]](None)
    orElse(some, none)
  }

  def mapP[B, T](f: T => B)(p: Parser[T]) = {
    //scala andThen
    val composed: T => Parser[B] = f andThen returnP
    bindP(composed)(p)
  }

  // similar to unit in monad.
  // It returns a parser that does not consume the stream and just returns the supplied value.
  def returnP[T](x: T) = {
    val label = "unknown"

    def innerFn(input: InputState) = {
      Right(Success(x, input))
    }

    new Parser(innerFn, label)
  }

  def bindP[A, B](f: A => Parser[B])(p: Parser[A]) = {
    val label = "unknown"

    def innerFn(input: InputState) = {
      val result1 = run(p)(input)

      result1 match {
        case Left(f) => Left(f)
        case Right(Success(value1, remainingInput)) => {
          val p2 = f(value1)
          run(p2)(remainingInput)
        }
      }
    }

    Parser(innerFn, label)
  }

  def orElse[T](l: Parser[T], r: Parser[T]) = {
    l | r
  }

//  def between[A, B, C](p1: Parser[A])(p2: Parser[B])(p3: Parser[C]) =
//    omitRight(omitLeft(p1)(p2))(p3)

  def pchar(charToMatch: Char) = {

    def satisfy(predicate: Char => Boolean)(label: String) = {
      def innerFn(input: InputState) = {
        val (remainingInput, charOpt) = input.nextChar
        charOpt match {
          case None => {
            val err = "No more input"
            val pos = input.toParserPosition
            Left(Failure(label, err, pos))
          }
          case Some(first) => {
            if (predicate(first))
              Right(Success(first, remainingInput))
            else {
              val err = "Unexpected '" + first + "'"
              val pos = input.toParserPosition
              Left(Failure(label, err, pos))
            }
          }
        }
      }

      Parser(innerFn, label)
    }

    def predicate(ch: Char) = {
      ch == charToMatch
    }

    val label = "" + charToMatch

    satisfy(predicate)(label)
  }

  def anyOf(listOfChars: List[Char]) = choice(listOfChars.map(pchar))

  // match a specific string
  def pstring(str: String) = {
    def charListToStr(charList: List[Char]) =
      charList.mkString

    mapP(charListToStr)(sequence(str.toList.map(pchar)))
  }

  def digit = anyOf(('0' to '9').toList)

  //  def many1[T] (parser:Parser[T]) = {
  //    def parse1(head:T) ={
  //      def parse2(tail:T) = {
  //        returnP(head::tail::Nil)
  //      }
  //      many(bindP(parse2)(parser))
  //    }
  //    bindP(parse1)(parser)
  //  }

  def whitespaceChar = anyOf(' ' :: '\t' :: '\n' :: Nil)

  def run[T](parser: Parser[T])(input: InputState) =
  // call inner function with input
    parser.innerFn(input)


  case class Success[T](result: T, rest: InputState) {
    def printR = println(result)
  }

  case class Failure(label: String, msg: String, pos: ParserPosition) {
    def printR = {
      val errorLine = pos.currentLine
      val colPos = pos.col
      val linePos = pos.line
      val failureCaret = " " * colPos + "^" + msg
      println("Line:" + linePos + " Col:" + colPos + " Error parsing " + label + "\n"
        + errorLine + "\n"
        + failureCaret)
    }
  }

  case class Parser[T](innerFn: InputState => Either[Failure, Success[T]], label: String) {
    //andThen
    def ~[S](that: => Parser[S]) = {
      def innerFn(input: InputState) = {
        val result1 = run(this)(input)
        result1 match {
          case Left(f) => Left(f)

          case Right(Success(s_1, rest)) => {
            val result2 = run(that)(rest)

            result2 match {
              case Left(f) => Left(f)

              case Right(Success(s_2, rest_2)) => {
                // combine both values as a pair
                val newValue = (s_1, s_2)
                // return remaining input after parser2
                Right(Success(newValue, rest_2))
              }
            }
          }
        }

      }

      new Parser(innerFn, label)
    }

    //omit that
    def ~>[S](that: =>Parser[S]) = {
      def transformer(z: (T, S)): T =
        z match {
          case (a, b) => a
        }
      // create a pair
      mapP(transformer)(andThen(this, that))
    }

    //omit this
    def <~[S](that: =>Parser[S]) = {
      def transformer(z: (T, S)): S =
        z match {
          case (a, b) => b
        }
      // create a pair
      mapP(transformer)(andThen(this, that))
    }



    //orElse
    def |(that: => Parser[T]) = {

      def innerFn(input: InputState) = {
        val result1 = run(this)(input)
        result1 match {
          case Left(f) => {
            val result2 = run(that)(input)
            result2
          }
          case Right(s) => {
            Right(s)
          }
        }

      }

      new Parser(innerFn, label)
    }

    def setLabel(newLabel: String) = {
      def newInnerFn(input: InputState) = {
        val result = innerFn(input)
        result match {
          case Right(s) => Right(s)
          case Left(Failure(oldLabel, err, pos)) => Left(Failure(newLabel, err, pos))
        }
      }

      Parser(newInnerFn, newLabel)
    }

  }

  case class Position(line: Int, col: Int) {
    def incrCol = Position(line, col + 1)

    def incrLine = Position(line + 1, 0)
  }

  case class InputState(lines: List[String], position: Position) {
    def nextChar = {
      val linePos = position.line
      val colPos = position.col
      if (linePos >= lines.length)
        (this, None)
      else {
        val curLine = currentLine
        if (colPos < curLine.length) {
          val char = currentLine(colPos)
          val newPos = position.incrCol
          val newState = this.copy(position = newPos)
          (newState, Some(char))
        }
        else {
          val char = '\n'
          val newPos = position.incrLine
          val newState = this.copy(position = newPos)
          (newState, Some(char))
        }
      }

    }

    def currentLine = {
      val linePos = position.line
      if (linePos < lines.length)
        lines(linePos)
      else
        "end of file"
    }

    def toParserPosition = {
      ParserPosition(currentLine, position.line, position.col)
    }
  }

  case class ParserPosition(currentLine: String, line: Int, col: Int)

}