package ohnosequences

// Here we define our DSL
case object gadt {

  sealed trait Expr { type Repr }

  sealed trait BoolExpr extends Expr { type Repr = Boolean }
  sealed trait IntExpr extends Expr { type Repr = Int }

  case class BoolVar(sym: Symbol) extends BoolExpr
  trait AnyNot extends BoolExpr {
    type Inside <: BoolExpr
    val  inside: Inside
  }
  case class Not[E <: BoolExpr](val inside: E) extends AnyNot { type Inside = E }
  case class And[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr
  case class Or[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr

  // This can be much more general, but I leave close to how it was in the original example:
  // case class Ite[C <: BoolExpr, E <: Expr](cond: C, thenExpr: E, elseExpr: E) extends Expr { type Repr = E#Repr }
  case class IntIte[C <: BoolExpr, T <: IntExpr, E <: IntExpr](cond: C, thenExpr: T, elseExpr: E) extends IntExpr
  case class IntConst(value: Int) extends IntExpr
  case class IntVar(sym: Symbol) extends IntExpr

  // Just an alias for expression with the same representation
  type SameAs[E <: Expr] = Expr { type Repr <: E#Repr }

}


// Here are some rewriting rules, which can be applied just once (no recursion here)
case object rules { //extends rewrites.RecursiveStrategy {
  import gadt._
  import rewrites._

  implicit def doubleNegation[E <: BoolExpr]:
      RewriteRule[Not[Not[E]], E] =
  new RewriteRule[Not[Not[E]], E] {

    def apply(expr: InExpr): OutExpr = expr.inside.inside
  }

  implicit def switchCondition[C <: BoolExpr, T <: IntExpr, E <: IntExpr]:
      RewriteRule[IntIte[Not[C], T, E], IntIte[C, E, T]] =
  new RewriteRule[IntIte[Not[C], T, E], IntIte[C, E, T]] {

    def apply(expr: InExpr): OutExpr = IntIte(expr.cond.inside, expr.elseExpr, expr.thenExpr)
  }
}
