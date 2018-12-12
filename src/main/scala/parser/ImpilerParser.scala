package parser

import org.parboiled2._

import scala.io.StdIn
import scala.util.{Failure, Success}

object ImpilerParser {

  def parse_input(input: String): types.Statement = {
    if (input == null)
      return null
    val parser = new ImpilerParser(input)
    parser.InputLine.run() match {
      case Success(exprAst)       => return exprAst
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e)             => println("Unexpected error during parsing run: " + e)
    }
    return null
  }

  def readInput(nomeArquivo: String): String = {
    return readFileInput(nomeArquivo)
  }

  def readFileInput(nomeArquivo: String): String = {
    try {
      val arquivo = scala.io.Source.fromFile(nomeArquivo)
      val info = try arquivo.mkString finally arquivo.close()
      return limpaInput(info)
    } catch {
      case _: Throwable => {
        println("Algo deu errado, verifique o nome do seu arquivo!")
        return null
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
    BindVal | BindVar
  }

  def BindVal: Rule1[types.Dec] = rule {
    "val" ~ Identifier ~ ":=" ~ Exp ~> { (x: String, y: types.Exp) => types.Bind(types.Id(x), y)}
  }

  def BindVar: Rule1[types.Dec] = rule {
    "var" ~ Identifier ~ ":=" ~ Exp ~> { (x: String, y: types.Exp) => types.Bind(types.Id(x), types.Ref(y))}
  }

  def DSeq: Rule1[types.DSeq] = rule {
    Bind ~ WS ~ DTerm ~> types.DSeq
  }

  def Dec= rule { "let" ~ DTerm }

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

  def Blk: Rule1[types.Blk] = rule { DecBlk | FnBlk }

  def DecBlk = rule { Dec ~ WS ~ "in" ~ WS ~ "{" ~ WS ~ Cmd ~ WS ~ "}" ~> types.Blk }

  def FnBlk = rule { "let fn" ~ WS ~ Id ~ "(" ~ (IdFn | IdFn1) ~ ")" ~ WS ~ "= " ~ WS ~ Dec ~ WS ~ "in" ~ WS ~ "{" ~ WS ~ Cmd ~ WS ~ "}" ~ Cmd ~> {(id1: types.Id, id2: Seq[types.Id], dec: types.Dec, cmd1: types.Cmd, cmd2: types.Cmd) => types.Blk(types.BindAbs(id1, types.Abs(id2, types.Blk(dec, cmd1))), cmd2)}}
  def FnBlk0 = rule { "let fn" ~ WS ~ Id ~ "(" ~ WS ~ ")" ~ WS ~ "= " ~ WS ~ Dec ~ WS ~ "in" ~ WS ~ "{" ~ WS ~ Cmd ~ WS ~ "}" ~ Cmd ~> {(id1: types.Id, dec: types.Dec, cmd1: types.Cmd, cmd2: types.Cmd) => types.Blk(types.BindAbs(id1, types.Abs(null, types.Blk(dec, cmd1))), cmd2)}}

  def IdFn1 = rule { Id ~> {(id: types.Id) => Seq(id)} }
  def IdFn = rule { Id ~ oneOrMore(NextId) ~> {(id1: types.Id, id2: Seq[types.Id]) => Seq(id1) ++ id2} }
  def NextId = rule { "," ~ WS ~ Id ~> {(id: types.Id) => id }}


  def Call = rule { "in" ~ WS ~ Id ~ "(" ~ (ExpFn | ExpFn1) ~ ")" ~> {(id: types.Id, exp: Seq[types.Exp]) => types.Call(id, exp)}}
  def Call0 = rule { "in" ~ WS ~ Id ~ "(" ~ WS ~ ")" ~> { (id:types.Id) => types.Call(id, null) } }
  def ExpFn1 = rule { Exp ~> {(Exp: types.Exp) => Seq(Exp)} }
  def ExpFn = rule { Exp ~ oneOrMore(NextExp) ~> {(Exp1: types.Exp, Exp2: Seq[types.Exp]) => Seq(Exp1) ++ Exp2} }
  def NextExp = rule { "," ~ WS ~ Exp ~> {(Exp: types.Exp) => Exp }}

  def Cmd = rule { CSeq | CmdTerm }

  def CmdTerm = rule { Blk | Loop | Assign | ValRefAssign | DeRefAssign | Call }

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