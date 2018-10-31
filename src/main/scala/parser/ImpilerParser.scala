package parser

import org.parboiled2._

import scala.io.StdIn
import scala.util.{Failure, Success}

import types._

object ImpilerParser {
  def parse(): Unit ={
    val parser = new ImpilerParser("2*2+2")
    parser.InputLine.run() match {
      case Success(exprAst)       => println("Result: " + exprAst)
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             => println("Unexpected error during parsing run: " + e)
    }
  }

  def readInput(): String = {
    print("-----------\nDigite o código desejado:\n ")
    Console.out.flush()
    return StdIn.readLine()
  }

}


class ImpilerParser(val input: ParserInput) extends Parser {
  import ImpilerParser._

  def InputLine = rule { AExp ~ EOI }

  def AExp: Rule1[AExp] = rule {
    ( Term ~ zeroOrMore(
      '+' ~ Term ~> {(r:AExp,l:AExp) => Sum(r,l)}
        | '-' ~ Term ~> {(r:AExp,l:AExp) => Sub(r,l)}
    )
      | Term)
  }


  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> {(r:AExp,l:AExp) => Mul(r,l)}
        | '/' ~ Factor ~> {(r:AExp,l:AExp) => Div(r,l)}
    )
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ AExp ~ ')' }

  def Number = rule { capture(Digits) ~> { x => Num(x.toInt)} }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}