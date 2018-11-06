package types

import javax.sql.StatementEvent

import scala.collection.mutable
import scala.collection.mutable.HashMap

abstract class ImpType

abstract class CtrlType extends ImpType

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


abstract class Statement extends ImpType

abstract class Exp extends Statement

case class AId(v:String) extends AExp
case class BId(v:String) extends BExp

abstract class AExp extends Exp
case class Sum(l: AExp, r:AExp) extends AExp
case class Sub(l: AExp, r:AExp) extends AExp
case class Div(l: AExp, r:AExp) extends AExp
case class Mul(l: AExp, r:AExp) extends AExp
case class Num(v: Double) extends AExp

abstract class BExp extends Exp
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
case class DeRef(id:String) extends Exp
case class ValRef(id:String) extends Exp

abstract class Cmd extends Statement
case class Assign(id:String ,e:Exp) extends Cmd
case class CSeq(r:Cmd, l:Cmd) extends Cmd
case class Loop(check:BExp, cmd:Cmd) extends Cmd
case class Blk(dec:Dec, cmd: Cmd) extends Cmd

abstract class Dec extends Statement
case class Bind(id: Id, e:Exp) extends Dec
case class DSeq(r:Dec, l:Dec) extends Dec

case class Id(id: String)