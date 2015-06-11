package ohnosequences

case object rewrites {
  // We could make all this a trait and parametrise by some generic type instead of Expr,
  // but let's keep it simple
  import gadt._

  // It's a good practice in general to separate type members and type parameters
  // That's why we have here two traits: AnyRewrite and Rewrite
  trait AnyRewrite {

    type InExpr <: Expr
    type OutExpr <: SameAs[InExpr]

    def apply(expr: InExpr): OutExpr
  }


  // One rewrite rule define one step of rewriting
  trait AnyRewriteRule extends AnyRewrite
  trait RewriteRuleFor[IE <: Expr] extends AnyRewriteRule { type InExpr = IE }
  trait RewriteRule[IE <: Expr, OE <: SameAs[IE]] extends RewriteRuleFor[IE] { type OutExpr = OE }

  // Recursive rewrite of some expression
  trait AnyRecRewrite extends AnyRewrite
  trait RecRewriteOf[IE <: Expr] extends AnyRecRewrite { type InExpr = IE }
  trait RecRewrite[IE <: Expr, OE <: SameAs[IE]] extends RecRewriteOf[IE] { type OutExpr = OE }
  trait IdRecRewrite[IE <: Expr] extends RecRewrite[IE, IE]

  object AnyRewrite extends AnyStrategyStrategy {

    // This implicit is in another trait to make its priority higher
    implicit def recRewrite[
      E <: Expr,
      Rule <: RewriteRuleFor[E],
      Rec <: RecRewriteOf[Rule#OutExpr]
    ](implicit
      rule: Rule,
      rec: Rec
    ):  RecRewrite[E, Rec#OutExpr] =
    new RecRewrite[E, Rec#OutExpr] {

      def apply(expr: InExpr): OutExpr = rec(rule(expr))
    }
  }

  trait AnyStrategyStrategy {

    // This is the fallback case for recursive rewriting
    implicit def idRewrite[E <: Expr]:
        IdRecRewrite[E] =
    new IdRecRewrite[E] { def apply(expr: InExpr): OutExpr = expr }
  }


  // This method looks for an implicit rewriting strategy and applies it
  def rewrite[IE <: Expr, OE <: SameAs[IE]](e: IE)
    (implicit rewr: RecRewriteOf[IE] { type OutExpr = OE }): OE = rewr(e)

}
