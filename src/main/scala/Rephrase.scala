package ohnosequences

// Here we define our DSL GADT
case object lang {

  sealed trait Expr { type Repr }

  sealed trait BoolExpr extends Expr { type Repr = Boolean }
  sealed trait IntExpr extends Expr { type Repr = Int }

  case class BoolVar(sym: Symbol) extends BoolExpr
  case class Not[E <: BoolExpr](e: E) extends BoolExpr
  case class And[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr
  case class Or[L <: BoolExpr, R <: BoolExpr](l: L, r: R) extends BoolExpr

  // This can be much more general, but I leave close to how it was in the original example:
  // case class Ite[C <: BoolExpr, E <: Expr](cond: C, thenExpr: E, elseExpr: E) extends Expr { type Repr = E#Repr }
  case class IntIte[C <: BoolExpr, T <: IntExpr, E <: IntExpr](cond: C, thenExpr: T, elseExpr: E) extends IntExpr
  case class IntConst(value: Int) extends IntExpr
  case class IntVar(sym: Symbol) extends IntExpr

}


case object rewrite {
  // We could make all this a trait and parametrise by some generic type instead of Expr,
  // but let's keep it simple
  import lang._

  // It's a good practice in general to separate type members and type parameters
  // That's why we have here two traits: AnyRewrite and Rewrite
  trait AnyRewrite {

    type InExpr <: Expr
    type OutExpr <: Expr { type Repr <: InExpr#Repr }

    def apply(expr: InExpr): OutExpr
  }

  // This trait just sets type members through type parameters
  trait Rewrite[IE <: Expr, OE <: Expr { type Repr <: IE#Repr }] extends AnyRewrite {

    type InExpr = IE
    type OutExpr = OE
  }

  trait RewriteRule[IE <: Expr, OE <: Expr { type Repr <: IE#Repr }] extends Rewrite[IE, OE]
  trait RecRewrite[IE <: Expr, OE <: Expr { type Repr <: IE#Repr }] extends Rewrite[IE, OE]
  // trait IdRewrite[E <: Expr] extends RecRewrite[E, E]

  // This method looks for an implicit rewriting rule and applies it
  def rewrite[IE <: Expr, OE <: Expr { type Repr <: IE#Repr }](e: IE)
    (implicit rephr: RecRewrite[IE, OE]): OE = rephr(e)
}


case object strategies {
  import lang._
  import rewrite._

  trait AnyRewriteStrategy {

    implicit def idRewrite[E <: Expr]:
        RecRewrite[E, E] =
    new RecRewrite[E, E] { def apply(expr: InExpr): OutExpr = expr }
  }

  trait AnyRecursiveRewriteStrategy extends AnyRewriteStrategy {

    implicit def recRewrite[
      E <: Expr,
      F <: Expr { type Repr = E#Repr },
      G <: Expr { type Repr = F#Repr }
    ](implicit
      rewrE: RewriteRule[E, F],
      rewrF: RecRewrite[F, G]
    ):  RecRewrite[E, G] =
    new RecRewrite[E, G] {

      def apply(expr: InExpr): OutExpr = rewrF(rewrE(expr))
    }
  }
}

// Here are some rewriting rules, which can be applied just once (no recursion here)
case object rules extends strategies.AnyRecursiveRewriteStrategy {
  import lang._
  import rewrite._

  implicit def doubleNegation[E <: BoolExpr]:
      RewriteRule[Not[Not[E]], E] =
  new RewriteRule[Not[Not[E]], E] {

    def apply(expr: InExpr): OutExpr = expr.e.e
  }

  implicit def switchCondition[C <: BoolExpr, T <: IntExpr, E <: IntExpr]:
      RewriteRule[IntIte[Not[C], T, E], IntIte[C, E, T]] =
  new RewriteRule[IntIte[Not[C], T, E], IntIte[C, E, T]] {

    def apply(expr: InExpr): OutExpr = IntIte(expr.cond.e, expr.elseExpr, expr.thenExpr)
  }
}
