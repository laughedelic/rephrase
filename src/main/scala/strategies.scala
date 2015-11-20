package ohnosequences

import ohnosequences.expr._
import ohnosequences.rewriting._
import ohnosequences.cosas._, fns._

case object strategies {

  type doubleNegation = doubleNegation.type
  case object doubleNegation extends AnyRewriteStrategy with RewritesConjunction {

    implicit def eliminate[
      E <: BoolExpr,
      O <: BoolExpr
    ](implicit
      rec: Rewrite[doubleNegation, E, O]
    ): Rewrite[doubleNegation, Not[Not[E]], O] =
       Rewrite[doubleNegation, Not[Not[E]], O]{ expr => rec(expr.inside.inside) }
  }

}
