package types

import scala.collection.mutable.{ArrayBuffer,HashMap}


trait ImpType

trait CtrlType extends ImpType

case class CtrlSum() extends CtrlType
case class CtrlSub() extends CtrlType
case class CtrlDiv() extends CtrlType
case class CtrlMul() extends CtrlType

case class CtrlAnd() extends CtrlType
case class CtrlOr() extends CtrlType
case class CtrlNot() extends CtrlType
case class CtrlEquals() extends CtrlType
case class CtrlLt() extends CtrlType
case class CtrlGt() extends CtrlType
case class CtrlLe() extends CtrlType
case class CtrlGe() extends CtrlType

case class CtrlId() extends CtrlType
case class CtrlAssign() extends CtrlType
case class CtrlLoop() extends CtrlType

case class CtrlRef() extends CtrlType
case class CtrlDeRef() extends CtrlType
case class CtrlValRef() extends CtrlType

case class CtrlBind() extends CtrlType
case class CtrlDec() extends CtrlType
case class CtrlBlk() extends CtrlType


trait Statement extends ImpType

trait Bindable extends Statement
//trait Bindable extends ImpType
//trait Exp extends Statement with Bindable
trait Exp extends Bindable with Actuals
case class Location(l: Int) extends Bindable

case class Id(v:String) extends AExp with BExp with Formals

trait AExp extends Exp
case class Sum(l: AExp, r:AExp) extends AExp
case class Sub(l: AExp, r:AExp) extends AExp
case class Div(l: AExp, r:AExp) extends AExp
case class Mul(l: AExp, r:AExp) extends AExp
case class Num(v: Double) extends AExp

trait BExp extends Exp
case class And(l: BExp, r:BExp) extends BExp
case class Or(l: BExp, r:BExp) extends BExp
case class Not(l: BExp) extends BExp
case class Equals(l: BExp, r:BExp) extends BExp
case class Bool(v: Boolean) extends BExp
case class Lt(l: AExp, r:AExp) extends BExp
case class Gt(l: AExp, r:AExp) extends BExp
case class Le(l: AExp, r:AExp) extends BExp
case class Ge(l: AExp, r:AExp) extends BExp

case class Ref(e: Exp) extends Exp
case class DeRef(id: Id) extends Exp
case class ValRef(id: Id) extends Exp

trait Cmd extends Statement
case class Assign(id: Id, e:Exp) extends Cmd
case class CSeq(r:Cmd, l:Cmd) extends Cmd
case class Loop(check:BExp, cmd:Cmd) extends Cmd
case class Blk(dec:Dec, cmd: Cmd) extends Cmd
case class Call(id: Id, actuals: Actuals) extends Cmd

trait Dec extends Statement
//case class Bind(id: Id, e:Exp) extends Dec
case class Bind(id: Id, e: Bindable) extends Dec
case class BindAbs(id: Id, abs: Abs) extends Dec
case class DSeq(r:Dec, l:Dec) extends Dec

case class Abs(f: Formals, b: Blk) extends Bindable

trait Formals
//case class Formal(id: Id) extends Formals
case class FSeq(f1: Formals, f2: Formals) extends Formals

trait Actuals
//case class Actual(exp: Exp) extends Actuals
case class ASeq(a1: Actuals, a2: Actuals) extends Actuals

//case class Formals(id_list: ArrayBuffer[Id])
//case class Actuals(exp_list: ArrayBuffer[Exp])

//case class Closure(f: Formals, b: Blk, e: HashMap[String,Int])
case class Closure(f: Formals, b: Blk, e: HashMap[String,Bindable]) extends Bindable