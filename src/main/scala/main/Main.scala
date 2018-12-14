package main
import automata.PiAutomata
import parser.ImpilerParser
import parser.ImpilerParser.{parse_input, readInput}
import types._

import scala.io.StdIn


object Main extends App {


  //var dangp:Cmd = Blk(DSeq(Bind(Id("x"),Ref(Num(100))),Bind(Id("val"),Ref(Num(0)))),CSeq(Blk(Bind(Id("y"),Ref(Bool(true))),Assign(Id("x"),DeRef(Id("y")))),Assign(Id("val"),ValRef(Id("x")))))
  var teste = Blk(DSeq(Bind(Id("val100"),Num(100)),Bind(Id("var"),Ref(Num(0)))),Assign(Id("var"),Id("val100")))
  //var teste = Blk(Bind(Id("z"),Ref(Num(1.0))),Blk(BindAbs(Id("f"),Abs(Seq(Id("x")),Blk(Bind(Id("y"),Ref(Id("x"))),Loop(Not(Lt(Id("y"),Num(1.0))),CSeq(Assign(Id("z"),Mul(Id("z"),Id("y"))),Assign(Id("y"),Sub(Id("y"),Num(1.0)))))))),Call(Id("f"),Seq(Num(10.0)))))
  var aut:PiAutomata = new PiAutomata(teste)
  aut.solve()
  aut.printAut()

  /*
  if(args.length > 0) {
    println("File: " + args(0))
    var parse_result = parse_input(readInput(args(0)))
    if(parse_result != null){
      println("PI-LIB: " + parse_result)
      var aut:PiAutomata = new PiAutomata(parse_result)
      aut.solve()
      aut.printAut()
    }
  } else {
    println("O programa precisa de pelo menos 1 argumento para executar")
  }
  */

}
