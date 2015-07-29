package ohnosequences

case object rewrites {
  import expr._

  trait AnyRewrite {

    type InExpr <: Expr
    type OutExpr <: SameAs[InExpr]

    def apply(expr: InExpr): OutExpr
  }

  abstract class RewriteOf[IE <: Expr] extends AnyRewrite { type InExpr = IE }
  abstract class Rewrite[IE <: Expr, OE <: SameAs[IE]] extends RewriteOf[IE] { type OutExpr = OE }

  // One rewrite rule defines one step of rewriting
  trait RewriteRuleFor[IE <: Expr] extends RewriteOf[IE]

  class RewriteRule[IE <: Expr, ME <: SameAs[IE], Rec <: RewriteOf[ME]](rec: Rec)(rule: IE => ME) extends RewriteRuleFor[IE] {
    type OutExpr = Rec#OutExpr

    def apply(expr: InExpr): OutExpr = rec(rule(expr))
  }

  case class IdRule[E <: Expr]() extends Rewrite[E, E]{
    def apply(expr: InExpr): OutExpr = expr
  }


  // Rewrite strategy is just a set of rewrite rules prioritised by the traits hierarchy
  trait AnyRewriteStrategy {

    // This is the fallback case for recursive rewriting
    implicit def idRewrite[E <: Expr]: IdRule[E] = IdRule[E]()
  }

  // This method looks for an implicit rewriting strategy and applies it
  def rewrite[IE <: Expr, ME <: SameAs[IE]](e: IE)
    (implicit rule: RewriteRuleFor[IE] { type OutExpr = ME }, rec: RewriteOf[ME]): rec.OutExpr = rec(rule(e))

}
