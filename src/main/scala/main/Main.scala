package main
import automata.PiAutomata
import parser.ImpilerParser
import types._

object Main extends App {
  val a:Exp = Div(Mul(Sub(Num(2), Num(4)),Sum(Num(2), Num(5))), Num(7))
  var b:Exp = And(Bool(true), Bool(false))
  var c:Exp = Seq(Assign("x", a), Id("x"))
  var d:Exp = And(Lt(Num(3),Num(5)),Ge(Num(3),Num(3)))

  var aut:PiAutomata = new PiAutomata(d)
  aut.solve()
  aut.printAut()

  ImpilerParser.parse()
}
