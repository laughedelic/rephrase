package ohnosequences

// Here we define our DSL
case object expr {

  sealed trait AnyExpr { type Repr }

  sealed trait BoolExpr extends AnyExpr { type Repr = Boolean }

  case class BoolVar(sym: Symbol) extends BoolExpr
  case class Not[E <: BoolExpr](val inside: E) extends BoolExpr
  case class And[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr
  case class Or[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr

  sealed trait IntExpr extends AnyExpr { type Repr = Int }
  case class IntConst(value: Int) extends IntExpr
  case class IntVar(sym: Symbol) extends IntExpr
  // This can be much more general, but I leave close to how it was in the original example:
  // case class Ite[C <: BoolExpr, E <: AnyExpr](cond: C, thenExpr: E, elseExpr: E) extends AnyExpr { type Repr = E#Repr }
  case class IfThenElse[C <: BoolExpr, T <: IntExpr, E <: IntExpr](cond: C, thenExpr: T, elseExpr: E) extends IntExpr

  // Just an alias for expression with the same representation
  type SameAs[E <: AnyExpr] = AnyExpr { type Repr <: E#Repr }

}
