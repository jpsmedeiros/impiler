package automata

import types._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayStack, HashMap}

class PiAutomata(input:Statement) {

  var ctr_stack: ArrayStack[ImpType] = new ArrayStack()
  var value_stack: ArrayStack[Any] = new ArrayStack()
  var env: HashMap[String,Int] = new HashMap()
  var mem: ArrayBuffer[Any] = new ArrayBuffer()
  var block_locks: ArrayBuffer[Int] = new ArrayBuffer()

  def solve(): Unit ={
    this.ctr_stack+=(this.input)
    while (this.ctr_stack.nonEmpty) {
      this.printAut()
      val top:ImpType = this.ctr_stack.pop()
      top match {
        // AExp
        case Sum(l, r) => {this.ctr_stack+=CtrlSum(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Sub(l, r) => {this.ctr_stack+=CtrlSub(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Div(l, r) => {this.ctr_stack+=CtrlDiv(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Mul(l, r) => {this.ctr_stack+=CtrlMul(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Num(v) => {this.value_stack += v}
        case AId(v) => {this.value_stack+= this.mem(this.env(v))}

        // BExp
        case And(l, r) => {this.ctr_stack+=CtrlAnd(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Or(l, r) => {this.ctr_stack+=CtrlOr(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Not(l, r) => {this.ctr_stack+=CtrlNot(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Equals(l, r) => {this.ctr_stack+=CtrlEquals(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Bool(v) => {this.value_stack += v}
        case Lt(l, r) => {this.ctr_stack+=CtrlLt(); this.ctr_stack += l; this.ctr_stack += r;}
        case Gt(l, r) => {this.ctr_stack+=CtrlGt(); this.ctr_stack += l; this.ctr_stack += r;}
        case Le(l, r) => {this.ctr_stack+=CtrlLe(); this.ctr_stack += l; this.ctr_stack += r;}
        case Ge(l, r) => {this.ctr_stack+=CtrlGe(); this.ctr_stack += l; this.ctr_stack += r;}
        case BId(v) => {this.value_stack+= this.mem(this.env(v))}

        case Ref(e) => {this.ctr_stack+=CtrlRef(); this.ctr_stack+= e}
        case DeRef(id) => {val l = this.env(id); this.value_stack+= l}
        case ValRef(id) => {val v = this.mem(this.env(id)); this.value_stack+= v}

        // Cmds
        case Assign(id ,e) => {this.ctr_stack+=CtrlAssign(); this.value_stack+= id; this.ctr_stack+= e}
        case CSeq(l, r) => {this.ctr_stack+= r; this.ctr_stack+=l}
        case Loop(check, cmd) => {this.ctr_stack+=CtrlLoop(); this.value_stack+= Loop(check, cmd); this.ctr_stack+= check}

        //Decs
        case Bind(id,e) => {this.ctr_stack+=CtrlBind(); this.ctr_stack+= e; this.value_stack+= id}
        case DSeq(l, r) => {this.ctr_stack+= r; this.ctr_stack+= l;}

        // Controles
        case CtrlSum() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v1.asInstanceOf[Double]+ v0.asInstanceOf[Double])}
        case CtrlSub() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v1.asInstanceOf[Double]- v0.asInstanceOf[Double])}
        case CtrlDiv() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v1.asInstanceOf[Double]/ v0.asInstanceOf[Double])}
        case CtrlMul() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v1.asInstanceOf[Double]* v0.asInstanceOf[Double])}
        case CtrlAnd() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v1.asInstanceOf[Boolean] && v0.asInstanceOf[Boolean])}
        case CtrlOr()  => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v1.asInstanceOf[Boolean] || v0.asInstanceOf[Boolean])}
        case CtrlNot() => { val v = value_stack.pop(); value_stack+= !v.asInstanceOf[Boolean] }
        case CtrlEquals() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v1.asInstanceOf[Boolean] == v0.asInstanceOf[Boolean])}
        case CtrlLt() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v0.asInstanceOf[Double] < v1.asInstanceOf[Double]) }
        case CtrlGt() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v0.asInstanceOf[Double] > v1.asInstanceOf[Double]) }
        case CtrlLe() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v0.asInstanceOf[Double] <= v1.asInstanceOf[Double]) }
        case CtrlGe() => { val v0 = value_stack.pop(); val v1 = value_stack.pop(); value_stack+= (v0.asInstanceOf[Double] >= v1.asInstanceOf[Double]) }
        case CtrlLoop()   => {
          val b = this.value_stack.pop();
          val loop = this.value_stack.pop();
            if(b.asInstanceOf[Boolean]){
              loop match{
                case Loop(check,cmd) => { this.ctr_stack.push(loop.asInstanceOf[Loop]); this.ctr_stack.push(cmd); }
              }
            }
        }
        case CtrlAssign() => { val v = value_stack.pop(); val id = value_stack.pop(); this.mem += v; this.env(id.asInstanceOf[String]) = this.mem.length - 1}
        case CtrlRef() => { val v = this.value_stack.pop(); this.mem+= v; val id = (this.mem.length - 1); this.value_stack.push(id); this.block_locks+= id}
        case CtrlBind() => {
          val head = this.value_stack.pop()
          val v1 = this.value_stack.pop()
          val v0 = this.value_stack.pop()

          head match{
            case HashMap[String,Int] => head(v0) = v1;
            case _ => val e:mutable.HashMap[String,Int] = new HashMap(); e(v0) = v1;
          }

        }
      }
    }
  }

  def getResult(): Any ={
    return this.value_stack.head
  }

  def printCtrlStack(stack: ArrayStack[ImpType]): Unit ={
    var l = stack.size
    print("CtrlStack: ")
    if(l==0) {println("Empty"); return }
    for( i <- 0 to l-1 ){
      print(s"${stack(i)}, ")
    }
    println()
  }

  def printValueStack(stack: ArrayStack[Any]): Unit ={
    var l = stack.size
    print("ValueStack: ")
    if(l==0) {println("Empty"); return }
    for( i <- 0 to l-1 ){
      print(s"${stack(i)}, ")
    }
    println()
  }

  def printEnv(env: HashMap[String,Int]): Unit ={
    print("Environment: ")
    if (env.isEmpty) {println("Empty"); return}
    for ((k,v) <- env) printf("{%s, %s} ", k, v)
    println()
  }

  def printMem(mem: ArrayBuffer[Any]): Unit ={
    print("Memory: ")
    val l = mem.size
    if(l==0) {println("Empty"); return }
    for( i <- 0 to l-1 ){
      print(s"${mem(i)}, ")
    }
    println()
  }

  def printBlockLocs(bl: ArrayBuffer[Int]): Unit ={
    print("BlockLocs: ")
    val l = bl.size
    if(l==0) {println("Empty"); return }
    for( i <- 0 to l-1 ){
      print(s"${bl(i)}, ")
    }
    println()
  }

  def printAut(): Unit ={
    printCtrlStack(this.ctr_stack)
    printValueStack(this.value_stack)
    printEnv(this.env)
    printMem(this.mem)
    printBlockLocs(this.block_locks)
    println()
  }

}
