package main
import automata.PiAutomata
import types._

object Main extends App {
  val a:Exp = Div(Mul(Sub(Num(2), Num(4)),Sum(Num(2), Num(5))), Num(7))
  var b:Exp = And(Bool(true), Bool(false))
  var c:Exp = Seq(Assign("x", a), Id("x"))
  var aut:PiAutomata = new PiAutomata(a)
  aut.solve()
  println(aut.getResult())
  var aut2:PiAutomata = new PiAutomata(b)
  aut2.solve()
  println(aut2.getResult())
  var aut3:PiAutomata = new PiAutomata(c)
  aut3.solve()
  println(aut3.getResult())
}
