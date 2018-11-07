package main
import automata.PiAutomata
import parser.ImpilerParser
import types._

import scala.collection.mutable.{HashMap}

import scala.collection.mutable

object Main extends App {
  val a:Exp = Div(Mul(Sub(Num(2), Num(4)),Sum(Num(2), Num(5))), Num(7))
  var b:Exp = And(Bool(true), Bool(false))
  var d:Exp = And(Lt(Num(3),Num(5)),Ge(Num(3),Num(3)))

  var e:Dec = Bind(Id("x"),Ref(Num(2)))

  var f:Cmd = Blk(Bind(Id("x"),Ref(Num(2))),CSeq(Assign(Id("x"),Num(3)),Loop(Lt(Id("x"),Num(5)),Assign(Id("x"),Sum(Id("x"),Num(1))))))
  var g:Cmd = Blk(Bind(Id("x"),Ref(Num(2))),Assign(Id("x"),Num(3)))
  var h:Cmd = Blk(DSeq(Bind(Id("x"),Ref(Num(100))),Bind(Id("val"),Ref(Num(0)))),CSeq(Blk(Bind(Id("y"),Ref(Bool(true))),Assign(Id("x"),DeRef(Id("y")))),Assign(Id("val"),ValRef(Id("x")))))

  var aut:PiAutomata = new PiAutomata(h)
  aut.solve()
  aut.printAut()

  ImpilerParser.parse()
}
