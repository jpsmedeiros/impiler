import org.scalatest.FunSuite
import parser.ImpilerParser
import types._

class ParserTest extends FunSuite {
  test("Operação aritmética: soma") {
    var result = ImpilerParser.parse_input("2+5")
    var expected = Sum(Num(2), Num(5))
    assert(result === expected)
  }

  test("Operação aritmética: multipicação") {
    var result = ImpilerParser.parse_input("5*7")
    var expected = Mul(Num(5), Num(7))
    assert(result === expected)
  }

  test("Operação aritmética: divisão") {
    var result = ImpilerParser.parse_input("10/5")
    var expected = Div(Num(10), Num(5))
    assert(result === expected)
  }

  test("Operação aritmética: operação com números negativos") {
    var result = ImpilerParser.parse_input("5*-1")
    var expected = Mul(Num(5), Num(-1))
    assert(result === expected)
  }

  test("Operação aritmética: prioridade de operacoes") {
    var result = ImpilerParser.parse_input("2+4*3")
    var expected = Sum(Num(2), Mul(Num(4), Num(3)))
    assert(result === expected)
  }

  test("Operação aritmética: operação com variáveis") {
    var result = ImpilerParser.parse_input("x*5")
    var expected = Mul(Id("x"), Num(5))
    assert(result === expected)
  }

  test("Operação aritmética: operação com variáveis 2") {
    var result = ImpilerParser.parse_input("(x*y)+(5*z)/20")
    var expected = Sum(Mul(Id("x"),Id("y")),Div(Mul(Num(5.0),Id("z")),Num(20.0)))
    assert(result === expected)
  }

  test("Operação booleana: e") {
    var result = ImpilerParser.parse_input("""true&&false""")
    var expected = And(Bool(true), Bool(false))
    assert(result === expected)
  }

  test("Operação booleana: ou") {
    var result = ImpilerParser.parse_input("""true||false""")
    var expected = Or(Bool(true), Bool(false))
    assert(result === expected)
  }

  test("Operação booleana: igualdade") {
    var result = ImpilerParser.parse_input("true=false")
    var expected = Equals(Bool(true), Bool(false))
    assert(result === expected)
  }

  test("Operação booleana: negação") {
    var result = ImpilerParser.parse_input("!true")
    var expected = Not(Bool(true))
    assert(result === expected)
  }

  test("Operação booleana: operação com negação") {
    var result = ImpilerParser.parse_input("""true&&!false""")
    var expected = And(Bool(true), Not(Bool(false)))
    assert(result === expected)
  }

  test("Operação booleana: operação com variáveis") {
    var result = ImpilerParser.parse_input("""x&&!false""")
    var expected = And(Id("x"), Not(Bool(false)))
    assert(result === expected)
  }

  test("Operação de desigualdade: maior que") {
    var result = ImpilerParser.parse_input("5 > 2")
    var expected = Gt(Num(5), Num(2))
    assert(result === expected)
  }

  test("Operação de desigualdade: menor que") {
    var result = ImpilerParser.parse_input("5 < 2")
    var expected = Lt(Num(5), Num(2))
    assert(result === expected)
  }

  test("Operação de desigualdade: maior igual") {
    var result = ImpilerParser.parse_input("5 >= 2")
    var expected = Ge(Num(5), Num(2))
    assert(result === expected)
  }

  test("Operação de desigualdade: menor igual") {
    var result = ImpilerParser.parse_input("5 <= 2")
    var expected = Le(Num(5), Num(2))
    assert(result === expected)
  }

  test("Operação de desigualdade: maior que com calculos") {
    var result = ImpilerParser.parse_input("(3*2) > 5+1")
    var expected = Gt(Mul(Num(3), Num(2)), Sum(Num(5), Num(1)))
    assert(result === expected)
  }

  test("Comando Assign: atribuição de número") {
    var result = ImpilerParser.parse_input("x := 3;")
    var expected = Assign(Id("x"), Num(3))
    assert(result === expected)
  }

  test("Comando Assign: atribuição de operação com booleano") {
    var result = ImpilerParser.parse_input("x := x&&true;")
    var expected = Assign(Id("x"), And(Id("x"), Bool(true)))
    assert(result === expected)
  }

  test("Comando Assign: operação entre variáveis") {
    var result = ImpilerParser.parse_input("x := x * y;")
    var expected = Assign(Id("x"), Mul(Id("x"), Id("y")))
    assert(result === expected)
  }

  test("Comando Assign: ValRef") {
    var result = ImpilerParser.parse_input("x := * y;")
    var expected = Assign(Id("x"), ValRef(Id("y")))
    assert(result === expected)
  }

  test("Comando Assign: DeRef") {
    var result = ImpilerParser.parse_input("x := & y;")
    var expected = Assign(Id("x"), DeRef(Id("y")))
    assert(result === expected)
  }

  test ("Programa: Fatorial"){
    var result = ImpilerParser.parse_input("let var z := 1 in let var y := 10 in while !(y < 1) do z := z * y; y := y - 1;")
    var expected = Blk(Bind(Id("z"),Ref(Num(1.0))),Blk(Bind(Id("y"),Ref(Num(10.0))),Loop(Not(Lt(Id("y"),Num(1.0))),CSeq(Assign(Id("z"),Mul(Id("z"),Id("y"))),Assign(Id("y"),Sub(Id("y"),Num(1.0)))))))
    assert(result === expected)
  }

  test ("Leitura de Arquivo: Fatorial"){
    var result = ImpilerParser.parse_input(ImpilerParser.readFileInput("src/tests/testFiles/fatorial.txt"))
    var expected = Blk(Bind(Id("z"),Ref(Num(1.0))),Blk(Bind(Id("y"),Ref(Num(10.0))),Loop(Not(Lt(Id("y"),Num(1.0))),CSeq(Assign(Id("z"),Mul(Id("z"),Id("y"))),Assign(Id("y"),Sub(Id("y"),Num(1.0)))))))
    assert(result === expected)
  }

}