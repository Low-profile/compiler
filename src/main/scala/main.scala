import Parser.FunctionS.prog
import Parser.parser.{InputState, Position, run}

import scala.io.{BufferedSource, Source}
import java.io.{FileOutputStream, PrintStream}

import Parser.AST
import llvmir.Program
import llvmir._
import llvmir.Types._
import llvmir.ILInstructions._

object main {
  def main(args: Array[String]): Unit = {

    var inputSource = Source.fromFile("testcase/whileStmt.c")
    if(args.nonEmpty)
      inputSource = Source.fromFile(args(0))
    val inputList = inputSource.getLines.toList
    inputSource.close()

    val initState = InputState(inputList, Position(0, 0))

//    System.setOut(new PrintStream(new FileOutputStream("output.txt")))

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
//        s.printR
      }
    }



    if (args.length > 1)
      progIR.writeToFile(args(1))
    else
      progIR.writeToFile("test.ll")

  }


}
