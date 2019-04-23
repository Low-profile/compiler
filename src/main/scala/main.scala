import FunctionS.prog
import parser.{InputState, Position, run}

import scala.io.Source

import java.io.{FileOutputStream,PrintStream}
import scala.Console


object main {
  def main(args: Array[String]): Unit = {
    val inputSource = Source.fromFile("source.c")
    val inputList = inputSource.getLines.toList
    inputSource.close()

    val initState = InputState(inputList, Position(0, 0))

    System.setOut(new PrintStream(new FileOutputStream("output.txt")))

    run(prog)(initState) match {
      case Left(s) => s.printR
      case Right(s) => s.printR
    }
    //    println(run(opt(digits))(initState.copy(lines = List("aa111,23A"))))

  }

}
