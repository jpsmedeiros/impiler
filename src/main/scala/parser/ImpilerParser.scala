package parser

import org.parboiled2._

import scala.io.StdIn
import scala.util.{Failure, Success}

object ImpilerParser {

  def parse(): Unit = {
    var parse_result = parse_input(readInput())
    if(parse_result != null){
      println("PI-LIB: " + parse_result)
    }
    parse()
  }

  def parse_input(input: String): Any = {
    val parser = new ImpilerParser(input)
    parser.InputLine.run() match {
      case Success(exprAst)       => return exprAst
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

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def InputLine = rule { ((Exp) | Statement) ~ EOI }

  def Statement: Rule1[types.Statement] = rule {
    Dec | Cmd
  }

  def Dec: Rule1[types.Dec] = rule {
    "let var" ~ Identifier ~ ":=" ~ Exp ~> { (x: String, y: types.Exp) => types.Bind(types.Id(x), types.Ref(y))}
  }

  def Loop: Rule1[types.Loop] = rule { ("while" ~ WS ~ "(" ~ WS ~ BExp ~ WS ~ ")" ~ WS ~ "do " ~ WS ~ "{" ~ WS ~ Cmd ~ WS ~ "}" ~ WS
    ~> {(cond: types.BExp, cmd: types.Cmd) => types.Loop(cond, cmd)}) }

  def CSeq: Rule1[types.CSeq] = rule { CmdTerm ~ WS ~ ";" ~ WS ~ Cmd ~> types.CSeq }

  def Assign: Rule1[types.Assign] = rule { Identifier ~ ":=" ~ Exp ~> {(x: String, y: types.Exp) => types.Assign(types.Id(x), y)} }

  def Cmd = rule { CSeq | CmdTerm }

  def CmdTerm = rule { Loop | Assign }

  def Exp: Rule1[types.Exp] = rule {
    BExp | AExp
  }

  def AExp: Rule1[types.AExp] = rule {
    ATermP
  }

  def ATermP = rule {
    (ATerm ~ zeroOrMore( ('+' ~ ATerm ~>  types.Sum) | ('-' ~ ATerm ~> types.Sub)) ) | ATerm
  }

  def ATerm = rule {
    (AFactor ~ zeroOrMore( ('*' ~ AFactor ~>  types.Mul) | ('/' ~ AFactor ~> types.Div)) ) | AFactor
  }

  def BExp: Rule1[types.BExp] = rule { BExp2 | BExp1 | BExp2A | BFactor}

  def BExp2 = rule{
    ( (BFactor | BExp1) ~ oneOrMore(
      """/\""" ~ (BFactor | BExp1) ~> types.And
        | """\/""" ~ (BFactor | BExp1) ~> types.Or
        | '=' ~ (BFactor | BExp1) ~> types.Equals
    ))
  }

  def BExp2A = rule{
    (AExp ~ '>' ~ AExp ~> types.Gt) | (AExp ~ '<' ~ AExp ~> types.Lt) | (AExp ~ str(">=") ~ AExp ~> types.Ge) | (AExp ~ str("<=") ~ AExp ~> types.Le)
  }

  def BExp1 = rule{
    '!' ~ BFactor ~> types.Not
  }

  def Id: Rule1[types.Exp] = rule {
    Identifier ~> {x: String => types.Id(x) }
  }

  def AFactor = rule { AParens | Number }

  def AParens = rule { WS ~ '(' ~ AExp ~ ')' ~ WS }

  def Number = rule { WS ~ capture(Digits) ~ WS ~> { x => types.Num(x.toInt)} }

  def WS = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }

  def Digits = rule { ("+" | "-").? ~ oneOrMore(CharPredicate.Digit) }

  def Identifier = rule {
    WS ~ capture(oneOrMore(CharPredicate.AlphaNum)) ~ WS
  }

  def BFactor = rule { Bool | BParens }

  def BParens = rule { '(' ~ BExp ~ ')' }

  def Bool = rule { (atomic("true") ~> {() => types.Bool(true)}) | (atomic("false") ~> {() => types.Bool(false)} ) }

}