package automata

import types._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayStack, HashMap, WrappedArray}

class PiAutomata(input:Statement) {

  var ctr_stack: ArrayStack[Any] = new ArrayStack()
  var value_stack: ArrayStack[Any] = new ArrayStack()
  //var env: HashMap[String,Int] = new HashMap()
  var env: HashMap[String,Any] = new HashMap()
  var mem: HashMap[Int, Any] = new HashMap()
  var block_locks: ArrayBuffer[Int] = new ArrayBuffer()
  var next_index: Int = 0

  def solve(): Unit ={
    this.ctr_stack+=(this.input)
    while (this.ctr_stack.nonEmpty) {
      this.printAut()
      //val top:ImpType = this.ctr_stack.pop()
      val top = this.ctr_stack.pop()
      top match {
        // AExp
        case Sum(l, r) => {this.ctr_stack+=CtrlSum(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Sub(l, r) => {this.ctr_stack+=CtrlSub(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Div(l, r) => {this.ctr_stack+=CtrlDiv(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Mul(l, r) => {this.ctr_stack+=CtrlMul(); this.ctr_stack+= r; this.ctr_stack+=l}
        case Num(v) => {this.value_stack += v}

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

        case Id(v) => {
          var aux = this.env(v)
          aux match{
            case Location(l) => this.value_stack+= this.mem(l)
            //case Num(v) => this.ctr_stack+= v
            //case Bool(v) => this.ctr_stack+= v
            case d: Double => this.value_stack+= d
            case b: Boolean => this.value_stack+= b
            case c: Closure => this.value_stack+= c
            case _ => {println("Error: Improper type in Environment"); System.exit(1) }
          }

          //this.value_stack+= this.mem(this.env(v))
        }

        case Ref(e) => {this.ctr_stack+=CtrlRef(); this.ctr_stack+= e}
        case DeRef(id) => {
          val l = this.env(id.v)
          l match{
            case Location(l) => this.value_stack+= l
            case _  => { printf("Error: DeRef to a constant"); System.exit(1) }
          }
          //this.value_stack+= l
        }

        case ValRef(id) => {
          //val v = this.mem(this.mem(this.env(id.v)).asInstanceOf[Int]);
          val aux = this.env(id.v)
          aux match{
            case Location(l) => {
              val key = this.mem(l).asInstanceOf[Int] // change to location
              if(!this.mem.contains(key)){
                printf("Dangling pointer exception in %s",id.v)
                System.exit(1)
              }
              val v = this.mem(key)
              this.value_stack+= v
            }
            case _  => { printf("Error: ValRef to a constant"); System.exit(1) }
          }

          /*
          val key = this.mem(this.env(id.v)).asInstanceOf[Int]
          if(!this.mem.contains(key)){
            printf("Dangling pointer exception in %s",id.v)
            System.exit(1)
          }
          val v = this.mem(key)
          this.value_stack+= v
          */
        }

        // Cmds
        case Assign(id ,e) => {this.ctr_stack+=CtrlAssign(); this.value_stack+= id.v; this.ctr_stack+= e}
        case CSeq(l, r) => {this.ctr_stack+= r; this.ctr_stack+=l}
        case Loop(check, cmd) => {this.ctr_stack+=CtrlLoop(); this.value_stack+= Loop(check, cmd); this.ctr_stack+= check}
        case Blk(dec, cmd) => {
          this.ctr_stack+= CtrlBlk();
          this.ctr_stack+= cmd;

          if(dec != null){
            this.ctr_stack+= CtrlDec();
            this.ctr_stack+= dec;

          }

          this.value_stack+= this.block_locks.clone();
          this.block_locks = new ArrayBuffer()
        }

        case Call(id, actuals) => {
          this.ctr_stack += CtrlCall(id, actuals.length)
          actuals.foreach((e) => {this.ctr_stack += e})
        }

        //Decs
        case Bind(id,e) => {this.ctr_stack+=CtrlBind(); this.ctr_stack+= e; this.value_stack+= id.v}
        case BindAbs(id, abs) => {this.ctr_stack+=CtrlBind(); this.ctr_stack+= abs; this.value_stack+= id.v}

        case DSeq(l, r) => {this.ctr_stack+= r; this.ctr_stack+= l;}

        //Abstractions
        case Abs(f,b) => { val c:Closure = Closure(f,b,this.env); this.value_stack+= c; }

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
        case CtrlAssign() => {
          val v = value_stack.pop();
          val id = value_stack.pop();

          val aux = this.env(id.asInstanceOf[String])
          aux match{
            case Location(l) => this.mem(l) = v
            case _  => { printf("Error: Assign to a constant"); System.exit(1) }
          }
          //this.mem(this.env(id.asInstanceOf[String])) = v;
        }

        //case CtrlRef() => { val v = this.value_stack.pop(); this.mem+= v; val id = this.mem.length - 1; this.value_stack.push(id); this.block_locks+= id}
        case CtrlRef() => {
          //val v = this.value_stack.pop(); val id = this.next_index; this.next_index+= 1; this.mem(id) = v; this.value_stack.push(id); this.block_locks+= id;
          val v = this.value_stack.pop()
          val id = this.next_index
          this.next_index+= 1
          this.mem(id) = v
          var l: Location = Location(id)
          this.value_stack.push(l)
          this.block_locks+= id
        }
        case CtrlBind() => {
          val v1 = this.value_stack.pop()
          val v0 = this.value_stack.pop()

          if(!this.value_stack.isEmpty) {
            val head = this.value_stack.pop()
            head match {
              //case t: HashMap[String, Int] => t(v0.asInstanceOf[String]) = v1.asInstanceOf[Int]; this.value_stack+= t;
              case t: HashMap[String, Any] => t(v0.asInstanceOf[String]) = v1; this.value_stack+= t;
              //case _ => this.value_stack+= head; val t: mutable.HashMap[String, Int] = new HashMap(); t(v0.asInstanceOf[String]) = v1.asInstanceOf[Int]; this.value_stack+= t;
              case _ => {
                this.value_stack+= head;
                val t: mutable.HashMap[String, Any] = new HashMap();
                //val n:Num = Num(v1.asInstanceOf[Double]);
                t(v0.asInstanceOf[String]) = v1;
                this.value_stack+= t;
              }
            }
          }
          else{
            //val t: mutable.HashMap[String, Int] = new HashMap(); t(v0.asInstanceOf[String]) = v1.asInstanceOf[Int]; this.value_stack+= t;
            val t: mutable.HashMap[String, Any] = new HashMap(); t(v0.asInstanceOf[String]) = v1; this.value_stack+= t;
          }
        }
        case CtrlDec() => {
          //var e1:mutable.HashMap[String,Int] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Int]];
          //var e1:mutable.HashMap[String,Bindable] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Bindable]];
          var e1:mutable.HashMap[String,Any] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Any]];
          this.value_stack+= this.env.clone();
          for ((k,v) <- e1) this.env(k) = v;
        }
        case CtrlBlk() => {
          //var e:mutable.HashMap[String,Int] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Int]];
          //var e:mutable.HashMap[String,Bindable] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Bindable]];
          var e:mutable.HashMap[String,Any] = this.value_stack.pop().asInstanceOf[mutable.HashMap[String,Any]];
          var bl:ArrayBuffer[Int] = this.value_stack.pop().asInstanceOf[ArrayBuffer[Int]]

          this.env = e;
          var aux:ArrayBuffer[Int] = this.block_locks;
          this.block_locks = bl;

          val l = aux.size
          if(l!=0) {
            for (i <- 0 to l - 1) {
              //this.mem(aux(i)) = null
              this.mem -= aux(i)
            }
          }
        }

        case CtrlCall(id, n) => {
          val closure = this.env(id.v).asInstanceOf[Closure]
          if(closure.f.length == n) {
            //var seq = (0 to n).map(index => Bind(closure.f(index), this.value_stack.pop().asInstanceOf[Exp])).reduce(DSeq(_, _))
            //val seq = (0 to n).map((index) => this.value_stack.pop().isInstanceOf[Bindable])

            val seq = (1 to n).map((index) => this.value_stack.pop())
            this.ctr_stack+= CtrlBlk()
            this.value_stack+= this.block_locks.clone();
            this.block_locks = new ArrayBuffer()
            this.ctr_stack+=  closure.b
            this.value_stack += this.env
            //this.env = closure.e
            this.env = pimatch(closure.f,seq,closure.e.clone())
          }
          else{ printf("Error: Different number of formals"); System.exit(1) }
        }

        case _ => {printf("Error: Invalid token in ControlStack"); System.exit(1) }

      }
    }
  }

  def pimatch(f: Seq[Id], values: Seq[Any], e: HashMap[String,Any]): HashMap[String,Any] ={

    for(i <- 0 to f.length-1){
      e(f(i).v) = values(i)
    }

    return e
  }

  def getResult(): Any ={
    return this.value_stack.head
  }

  //def printCtrlStack(stack: ArrayStack[ImpType]): Unit ={
  def printCtrlStack(stack: ArrayStack[Any]): Unit ={
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

  //def printEnv(env: HashMap[String,Bindable]): Unit ={
  def printEnv(env: HashMap[String,Any]): Unit ={
    print("Environment: ")
    if (env.isEmpty) {println("Empty"); return}
    for ((k,v) <- env) printf("{%s, %s} ", k, v)
    println()
  }

  def printMem(mem: HashMap[Int, Any]): Unit ={
    print("Memory: ")
    if (mem.isEmpty) {println("Empty"); return}
    for ((k,v) <- mem) printf("{%s, %s} ", k, v)
    println()
  }

  /*
  def printMem(mem: ArrayBuffer[Any]): Unit ={
    print("Memory: ")
    val l = mem.size
    if(l==0) {println("Empty"); return }
    for( i <- 0 to l-1 ){
      print(s"${mem(i)}, ")
    }
    println()
  }
  */

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
