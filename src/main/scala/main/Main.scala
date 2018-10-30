package main
import automata.PiAutomata
import types._

object Main extends App {
  val a:Exp = Div(Mul(Sub(Num(2), Num(4)),Sum(Num(2), Num(5))), Num(7))
  var b:Exp = And(Bool(true), Bool(false))
  //Is Id a command or expression?
  //var c:Cmd = Seq(Assign("x", a), Id("x"))
  var d:Exp = And(Lt(Num(3),Num(5)),Ge(Num(3),Num(3)))
  var e:Cmd = Seq(Assign("x",Num(3)),Loop(Lt(AId("x"),Num(5)),Assign("x",Sum(AId("x"),Num(1)))))

  var aut:PiAutomata = new PiAutomata(e)
  aut.solve()
  aut.printAut()
}
