package main
import automata.PiAutomata
import parser.ImpilerParser
import parser.ImpilerParser.{parse_input, readInput}
import types._


object Main extends App {
  var parse_result = parse_input(readInput())
  if(parse_result != null){
    println("PI-LIB: " + parse_result)
    var aut:PiAutomata = new PiAutomata(parse_result)
    aut.solve()
    aut.printAut()
  }
}
