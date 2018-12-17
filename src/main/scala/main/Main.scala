package main
import automata.PiAutomata
import parser.ImpilerParser
import parser.ImpilerParser.{parse_input, readInput, readInputFromUser}
import types._

import scala.io.StdIn


object Main extends App {


  //var dangp:Cmd = Blk(DSeq(Bind(Id("x"),Ref(Num(100))),Bind(Id("val"),Ref(Num(0)))),CSeq(Blk(Bind(Id("y"),Ref(Bool(true))),Assign(Id("x"),DeRef(Id("y")))),Assign(Id("val"),ValRef(Id("x")))))
  //var const_teste = Blk(DSeq(Bind(Id("val100"),Num(100)),Bind(Id("var"),Ref(Num(0)))),Assign(Id("var"),Id("val100")))

  /*
  var aut:PiAutomata = new PiAutomata(dangp)
  aut.solve()
  aut.printAut()
  */

  var loop = true
  var firstRun = true
  while(loop) {
    if (args.length > 0 && firstRun) {
      println("File: " + args(0))
      var parse_result = parse_input(readInput(args(0)))
      if (parse_result != null) {
        println("PI-LIB: " + parse_result)
        var aut: PiAutomata = new PiAutomata(parse_result)
        aut.solve()
        aut.printAut()
        firstRun = false
      }
    } else {
      var parse_result = parse_input(readInputFromUser())
      if(parse_result != null) {
        println("PI-LIB: " + parse_result)
        var aut: PiAutomata = new PiAutomata(parse_result)
        aut.solve()
        aut.printAut()
        firstRun = false
      }
    }
    loop = exitInput()
  }

  def exitInput(): Boolean = {
    print("-----------\nDeseja sair? (s/n)\n ")
    Console.out.flush()
    var resposta = StdIn.readLine()
    if (resposta.equals("s") || resposta.equals("S")) {
      return false
    } else {
      return true
    }
  }


}
