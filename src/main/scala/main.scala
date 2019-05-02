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

    val inputSource = Source.fromFile(args(0))
    val inputList = inputSource.getLines.toList
    inputSource.close()

    val initState = InputState(inputList, Position(0, 0))

    System.setOut(new PrintStream(new FileOutputStream("output.txt")))

    val progIR = new Program()

    val namedValue = Map.empty[String, Identifier]


    def progGen(decls: List[AST], NamedValues: Map[String, Identifier]): Map[String, Identifier] = {
      decls match {
        case head :: rest => {
          val updatedNV = head.codegen(progIR,NamedValues)
          progGen(rest, updatedNV)
        }
        case Nil => {
          NamedValues
        }
      }
    }

    run(prog)(initState) match {
      case Left(s) => s.printR
      case Right(s) => {
//        for (t <- s.result) {
//          t.codegen(progIR, namedValue)
//        }
        progGen(s.result,namedValue)
        s.printR
      }
    }



    progIR.writeToFile("test.ll")

  }


}
