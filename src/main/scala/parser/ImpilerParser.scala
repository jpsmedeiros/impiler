package parser

import org.parboiled2._

import scala.io.StdIn
import scala.util.{Failure, Success}

import types._

object ImpilerParser {
  def parse(): Unit ={
    val parser = new ImpilerParser(readInput())
    parser.InputLine.run() match {
      case Success(exprAst)       => println("Result: " + exprAst)
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


class ImpilerParser(val input: ParserInput) extends Parser {
  import ImpilerParser._

  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> Addition
        | '-' ~ Term ~> Subtraction)
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> Multiplication
        | '/' ~ Factor ~> Division)
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number = rule { capture(Digits) ~> { x => Value(x.toInt)} }

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}