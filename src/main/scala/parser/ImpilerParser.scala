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

  def parse_input(input: String): types.Statement = {
    val parser = new ImpilerParser(input)
    parser.InputLine.run() match {
      case Success(exprAst)       => return exprAst
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             => println("Unexpected error during parsing run: " + e)
    }
    return null
  }

  def readInput(): String = {
    print("-----------\nLer arquivo? (s/n)\n ")
    Console.out.flush()
    var resposta = StdIn.readLine()
    if(resposta.equals("s") || resposta.equals("S")){
      print("-----------\nDigite o nome do arquivo:\n ")
      var nomeArquivo = StdIn.readLine()
      return readFileInput(nomeArquivo)
    }else{
      return readTextInput()
    }
  }

  def readFileInput(nomeArquivo: String): String = {
    try {
      val arquivo = scala.io.Source.fromFile(nomeArquivo)
      val info = try arquivo.mkString finally arquivo.close()
      return limpaInput(info)
    } catch {
      case _: Throwable => {
        println("Algo deu errado, verifique o nome do seu arquivo!")
        return readInput()
      }
    }
  }

  def readTextInput(): String = {
    print("-----------\nDigite o código desejado:\n ")
    Console.out.flush()
    return StdIn.readLine()
  }

  def limpaInput(input: String): String = {
    //println("INPUT:\n")
    //println(input + "\n")
    return input.replaceAll("(\r\n)|\r|\n", "")
  }
}

class ImpilerParser(val input: ParserInput) extends Parser {

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def InputLine = rule { (Statement | Exp ) ~ EOI }

  def Statement: Rule1[types.Statement] = rule {
    Cmd
  }

  def Bind: Rule1[types.Dec] = rule {
    "var" ~ Identifier ~ ":=" ~ Exp ~> { (x: String, y: types.Exp) => types.Bind(types.Id(x), types.Ref(y))}
  }

  def DSeq: Rule1[types.DSeq] = rule {
    Bind ~ WS ~ DTerm ~> types.DSeq
  }

  def Dec = rule { "let" ~ DTerm }

  def DTerm = rule { DSeq | Bind }

  def Loop: Rule1[types.Loop] = rule { ("while" ~ WS ~ BExp ~ WS  ~ "do" ~ WS ~ "{" ~ WS ~ Cmd ~ WS ~ "}"
    ~> {(cond: types.BExp, cmd: types.Cmd) => types.Loop(cond, cmd)}) }

  def CSeq: Rule1[types.CSeq] = rule { CmdTerm ~ WS ~ Cmd ~> types.CSeq }

  def Assign = rule { normalAssign | ValRefAssign | DeRefAssign}

  def normalAssign: Rule1[types.Assign] = rule { Identifier ~ ":=" ~ Exp ~> {(x: String, y: types.Exp) => types.Assign(types.Id(x), y)} }

  def ValRefAssign: Rule1[types.Assign] = rule { Identifier ~ ":=" ~ ValRefSymbol ~ Identifier ~> {(x: String, y: String) => types.Assign(types.Id(x), types.ValRef(types.Id(y)))} }

  def DeRefAssign: Rule1[types.Assign] = rule { Identifier ~ ":=" ~ DeRefSymbol ~ Identifier ~> {(x: String, y: String) => types.Assign(types.Id(x), types.DeRef(types.Id(y)))} }

  def ValRefSymbol = rule { WS ~ '*' ~ WS }

  def DeRefSymbol = rule { WS ~ '&' ~ WS }

  def Blk: Rule1[types.Blk] = rule { Dec ~ WS ~ "in" ~ WS ~ "{" ~ WS ~ Cmd ~ WS ~ "}" ~> types.Blk }

  def Cmd = rule { CSeq | CmdTerm }

  def CmdTerm = rule { Loop | Blk | Assign | ValRefAssign | DeRefAssign }

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
      "&&" ~ (BFactor | BExp1) ~> types.And
        | "||" ~ (BFactor | BExp1) ~> types.Or
        | '=' ~ (BFactor | BExp1) ~> types.Equals
    ))
  }

  def BExp2A = rule{
    (AExp ~ '>' ~ AExp ~> types.Gt) | (AExp ~ '<' ~ AExp ~> types.Lt) | (AExp ~ str(">=") ~ AExp ~> types.Ge) | (AExp ~ str("<=") ~ AExp ~> types.Le)
  }

  def BExp1 = rule{
    '!' ~ BFactor ~> types.Not
  }

  def Id = rule {
    Identifier ~> {x: String => types.Id(x) }
  }

  def AFactor = rule { AParens | Number | Id }

  def AParens = rule { WS ~ '(' ~ AExp ~ ')' ~ WS }

  def Number = rule { WS ~ capture(Digits) ~ WS ~> { x => types.Num(x.toInt)} }

  def WS = rule { quiet(zeroOrMore(anyOf(" \t \n"))) }

  def Digits = rule { ("+" | "-").? ~ oneOrMore(CharPredicate.Digit) }

  def Identifier = rule {
    WS ~ capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~ WS
  }

  def ArithOp = rule { WS ~ ("+" | "-" | "*" | "/") ~ WS }
  def BoolOp = rule { WS ~ ("<" | "<=" | ">" | ">=") ~ WS }

  def BFactor = rule { BParens | Bool | !(Id ~ ArithOp | Id ~ BoolOp) ~ Id}

  def BParens = rule { '(' ~ BExp ~ ')' }

  def Bool = rule { (atomic("true") ~> {() => types.Bool(true)}) | (atomic("false") ~> {() => types.Bool(false)} ) }

}