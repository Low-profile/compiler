import FunctionS.prog
import parser.{InputState, Position, run}

import scala.io.Source
import java.io.{FileOutputStream, PrintStream}

import llvmir.Program


import llvmir._
import llvmir.Types._
import llvmir.ILInstructions._
import llvmir.AbstractILInstructions.OOP._
import llvmir.OperationChains._

object main {
  def main(args: Array[String]): Unit = {
    val inputSource = Source.fromFile("source.c")
    val inputList = inputSource.getLines.toList
    inputSource.close()

    val initState = InputState(inputList, Position(0, 0))

    System.setOut(new PrintStream(new FileOutputStream("output.txt")))

    val progIR = new Program()

    run(prog)(initState) match {
      case Left(s) => s.printR
      case Right(s) => {
        for (t <- s.result) {
          t.codegen(progIR, Map.empty[String, Any])
        }
        s.printR
      }
    }



    progIR.writeToFile("test.ll")

  }


}
