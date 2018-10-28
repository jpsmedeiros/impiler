package parser

import org.parboiled2.ParseError

import scala.io.StdIn
import scala.util.{Failure, Success}

class InputHandler {
  def parse(): Unit ={
    val parser = new ImpilerParser(readInput())
    parser.InputLine.run() match {
      case Success(exprAst)       => println("Result: ")
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             => println("Unexpected error during parsing run: " + e)
    }
    parse()
  }

  def readInput(): String = {
    print("-----------\nDigite o código desejado:\n ")
    Console.out.flush()
    return StdIn.readLine()
  }
}
