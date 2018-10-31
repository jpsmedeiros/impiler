package parser

import org.parboiled2._

import scala.io.StdIn
import scala.util.{Failure, Success}

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

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def InputLine = rule { (AExp | BExp) ~ EOI }

  def AExp: Rule1[types.AExp] = rule {
    ( ATerm ~ zeroOrMore(
      '+' ~ ATerm ~> types.Sum
        | '-' ~ ATerm ~> types.Sub
    ))
  }


  def ATerm = rule {
    AFactor ~ zeroOrMore(
      '*' ~ AFactor ~>  types.Mul
        | '/' ~ AFactor ~> types.Div
    )
  }

  def BExp: Rule1[types.BExp] = rule { BExp2 | BExp1 | BFactor}

  def BExp2 = rule{
    ( (BFactor | BExp1) ~ zeroOrMore(
      '&' ~ (BFactor | BExp1) ~> types.And
        | '|' ~ (BFactor | BExp1) ~> types.Or
        | '=' ~ (BFactor | BExp1) ~> types.Equals
    ))
  }

  def BExp1 = rule{
    '!' ~ BFactor ~> types.Not
  }


  def AFactor = rule { Number | AParens }

  def AParens = rule { '(' ~ AExp ~ ')' }

  def Number = rule { WS ~ capture(Digits) ~ WS ~> { x => types.Num(x.toInt)} }

  def WS = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }

  def Digits = rule { ("+" | "-").? ~ oneOrMore(CharPredicate.Digit) }

  def BFactor = rule { Bool | BParens }

  def BParens = rule { '(' ~ BExp ~ ')' }

  def Bool = rule { BoolT | BoolF }

  def BoolT = rule { atomic("True") ~> {() => types.Bool(true)} }

  def BoolF = rule { atomic("False") ~> {() => types.Bool(false)} }

}