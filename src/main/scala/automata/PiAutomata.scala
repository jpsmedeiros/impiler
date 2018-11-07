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
        case Not(l) => {this.ctr_stack+=CtrlNot(); this.ctr_stack+= l;}
        case Equals(l, r) => {this.ctr_stack+=CtrlEquals(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Bool(v) => {this.value_stack += v}
        case Lt(l, r) => {this.ctr_stack+=CtrlLt(); this.ctr_stack += l; this.ctr_stack += r;}
        case Gt(l, r) => {this.ctr_stack+=CtrlGt(); this.ctr_stack += l; this.ctr_stack += r;}
        case Le(l, r) => {this.ctr_stack+=CtrlLe(); this.ctr_stack += l; this.ctr_stack += r;}
        case Ge(l, r) => {this.ctr_stack+=CtrlGe(); this.ctr_stack += l; this.ctr_stack += r;}
        case BId(v) => {this.value_stack+= this.mem(this.env(v))}

        case Id(v) => {this.value_stack+= this.mem(this.env(v))}

        case Ref(e) => {this.ctr_stack+=CtrlRef(); this.ctr_stack+= e}
        case DeRef(id) => {val l = this.env(id.v); this.value_stack+= l}
        case ValRef(id) => {val v = this.mem(this.mem(this.env(id.v)).asInstanceOf[Int]); this.value_stack+= v}

        // Cmds
        case Assign(id ,e) => {this.ctr_stack+=CtrlAssign(); this.value_stack+= id.v; this.ctr_stack+= e}
        case CSeq(l, r) => {this.ctr_stack+= r; this.ctr_stack+=l}
        case Loop(check, cmd) => {this.ctr_stack+=CtrlLoop(); this.value_stack+= Loop(check, cmd); this.ctr_stack+= check}
        case Blk(dec, cmd) => {
          this.ctr_stack+= CtrlBlk(); this.ctr_stack+= cmd; this.ctr_stack+= CtrlDec(); this.ctr_stack+= dec;
          this.value_stack+= this.block_locks.clone(); this.block_locks = new ArrayBuffer()
        }

        //Decs
        case Bind(id,e) => {this.ctr_stack+=CtrlBind(); this.ctr_stack+= e; this.value_stack+= id.v}
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
        case CtrlAssign() => { val v = value_stack.pop(); val id = value_stack.pop(); this.mem(this.env(id.asInstanceOf[String])) = v; } //this.mem += v; this.env(id.asInstanceOf[String]) = this.mem.length - 1}
        case CtrlRef() => { val v = this.value_stack.pop(); this.mem+= v; val id = this.mem.length - 1; this.value_stack.push(id); this.block_locks+= id}
        case CtrlBind() => {
          val v1 = this.value_stack.pop()
          val v0 = this.value_stack.pop()

          if(!this.value_stack.isEmpty) {
            val head = this.value_stack.pop()
            head match {
              case t: HashMap[String, Int] => t(v0.asInstanceOf[String]) = v1.asInstanceOf[Int]; this.value_stack+= t;
              case _ => this.value_stack+= head; val t: mutable.HashMap[String, Int] = new HashMap(); t(v0.asInstanceOf[String]) = v1.asInstanceOf[Int]; this.value_stack+= t;
            }
          }
          else{
            val t: mutable.HashMap[String, Int] = new HashMap(); t(v0.asInstanceOf[String]) = v1.asInstanceOf[Int]; this.value_stack+= t;
          }
        }
        case CtrlDec() => {
          var e1:mutable.HashMap[String,Int] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Int]];
          this.value_stack+= this.env.clone();
          for ((k,v) <- e1) this.env(k) = v;
        }
        case CtrlBlk() => {
          var e:mutable.HashMap[String,Int] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Int]];
          var bl:ArrayBuffer[Int] = this.value_stack.pop().asInstanceOf[ArrayBuffer[Int]]

          this.env = e;
          var aux:ArrayBuffer[Int] = this.block_locks;
          this.block_locks = bl;

          val l = aux.size
          if(l!=0) {
            for (i <- 0 to l - 1) {
              this.mem(aux(i)) = null
            }
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
