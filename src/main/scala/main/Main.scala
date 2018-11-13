package main
import automata.PiAutomata
import parser.ImpilerParser
import parser.ImpilerParser.{parse_input, readInput}
import types._

import scala.io.StdIn


object Main extends App {
  var loop = true
  while(loop) {
    var parse_result = parse_input(readInput())
    if(parse_result != null){
      println("PI-LIB: " + parse_result)
      var aut:PiAutomata = new PiAutomata(parse_result)
      aut.solve()
      aut.printAut()
      loop = exitInput()
    }
  }

  def exitInput(): Boolean = {
    print("-----------\nDeseja sair? (s/n)\n ")
    Console.out.flush()
    var resposta = StdIn.readLine()
    if(resposta.equals("s") || resposta.equals("S")){
      return false
    }else{
      return true
    }
  }
}
