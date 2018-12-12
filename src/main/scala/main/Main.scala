package main
import automata.PiAutomata
import parser.ImpilerParser
import parser.ImpilerParser.{parse_input, readInput}
import types._

import scala.io.StdIn


object Main extends App {


  //var dangp:Cmd = Blk(DSeq(Bind(Id("x"),Ref(Num(100))),Bind(Id("val"),Ref(Num(0)))),CSeq(Blk(Bind(Id("y"),Ref(Bool(true))),Assign(Id("x"),DeRef(Id("y")))),Assign(Id("val"),ValRef(Id("x")))))
  //var const_test = Blk(DSeq(Bind(Id("val100"),Num(100)),Bind(Id("var"),Ref(Num(0)))),Assign(Id("var"),Id("val100")))
  //var aut:PiAutomata = new PiAutomata(const_test)
  //aut.solve()
  //aut.printAut()



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


}
