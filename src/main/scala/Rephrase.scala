package ohnosequences

case object lang {

  sealed trait AnyExpr { type Repr }
  trait Expr[R] extends AnyExpr { type Repr = R }

  sealed trait BoolExpr extends Expr[Boolean]
  sealed trait IntExpr extends Expr[Int]

  case class BoolVar(sym: Symbol) extends BoolExpr
  case class Not[E <: BoolExpr](e: E) extends BoolExpr
  case class And[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr
  case class Or[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr

  case class IntIte[C <: BoolExpr, T <: IntExpr, E <: IntExpr](cond: C, thenExpr: T, elseExpr: E) extends IntExpr
  case class IntConst(value: Int) extends IntExpr
  case class IntVar(sym: Symbol) extends IntExpr

}

case object rephrase {
  import lang._

  trait AnyRephrase {

    type InExpr <: AnyExpr
    type OutExpr <: AnyExpr { type Repr = InExpr#Repr }

    def apply(expr: InExpr): OutExpr
  }

  trait Rephrase[IE <: AnyExpr, OE <: AnyExpr { type Repr = IE#Repr }] extends AnyRephrase {

    type InExpr = IE
    type OutExpr = OE
  }

  def rephrase[IE <: AnyExpr, OE <: AnyExpr { type Repr = IE#Repr }](e: IE)
    (implicit rephr: Rephrase[IE, OE]): OE = rephr(e)
}


case object rules {
  import lang._
  import rephrase._

  implicit def doubleNegation[E <: BoolExpr]:
      Rephrase[Not[Not[E]], E] =
  new Rephrase[Not[Not[E]], E] {

    def apply(expr: InExpr): OutExpr = expr.e.e
  }

  implicit def switchCondition[C <: BoolExpr, T <: IntExpr, E <: IntExpr]:
      Rephrase[IntIte[Not[C], T, E], IntIte[C, E, T]] =
  new Rephrase[IntIte[Not[C], T, E], IntIte[C, E, T]] {

    def apply(expr: InExpr): OutExpr = IntIte(expr.cond.e, expr.elseExpr, expr.thenExpr)
  }
}
