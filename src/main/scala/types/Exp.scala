package types



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

abstract class Exp extends ImpType
abstract class AExp extends Exp
case class Sum(l: AExp, r:AExp) extends AExp
case class Sub(l: AExp, r:AExp) extends AExp
case class Div(l: AExp, r:AExp) extends AExp
case class Mul(l: AExp, r:AExp) extends AExp
case class Num(v: Double) extends AExp

abstract class BExp extends Exp
case class And(l: BExp, r:BExp) extends BExp
case class Or(l: BExp, r:BExp) extends BExp
case class Not(l: BExp, r:BExp) extends BExp
case class Equals(l: BExp, r:BExp) extends BExp
case class Bool(v: Boolean) extends BExp
case class Lt(l: AExp, r:AExp) extends BExp
case class Gt(l: AExp, r:AExp) extends BExp
case class Le(l: AExp, r:AExp) extends BExp
case class Ge(l: AExp, r:AExp) extends BExp

abstract class Cmd extends Exp
case class Assign(id:String ,e:Exp) extends Cmd
case class Seq(r:Cmd, l:Cmd) extends Cmd
case class Loop(check:BExp, cmd:Cmd) extends Cmd
case class Id(v:String) extends Cmd

abstract class Expr
case class Value(value: Int) extends Expr
case class Addition(lhs: Expr, rhs: Expr) extends Expr
case class Subtraction(lhs: Expr, rhs: Expr) extends Expr
case class Multiplication(lhs: Expr, rhs: Expr) extends Expr
case class Division(lhs: Expr, rhs: Expr) extends Expr
