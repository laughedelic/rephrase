package ohnosequences

import expr._
import ohnosequences.cosas._, fns._

case object rewriting {

  trait AnyRewriteStrategy extends AnyDepFn1 {

    type In1 = AnyExpr
    type Out = AnyExpr
  }

  // This is a synonim for App1
  case class Rewrite[
    S <: AnyRewriteStrategy{
      type In1 >: IE
      type Out >: OE
    },
    IE <: AnyExpr,
    OE <: AnyExpr //SameAs[IE]
  ](val rewr: IE => OE) extends AnyApp1At[S, IE] {
    type Y = OE

    final def apply(in: X1): Y = rewr(in)
  }

  case object AnyRewriteStrategy {

    // this is a fallback case for any strategy:
    implicit def default[
      S <: AnyRewriteStrategy,
      E <: AnyExpr
    ]: Rewrite[S, E, E] =
       Rewrite[S, E, E] { ie: E => ie }
  }


  trait RewritesConjunction extends AnyRewriteStrategy

  case object RewritesConjunction {

    implicit def default[
      S <: RewritesConjunction,
      L <: BoolExpr, R <: BoolExpr,
      LO <: BoolExpr, RO <: BoolExpr
    ](implicit
      recL: Rewrite[S, L, LO],
      recR: Rewrite[S, R, RO]
    ): Rewrite[S, L And R, LO And RO] =
       Rewrite[S, L And R, LO And RO]{ expr =>
         And(recL(expr.l), recR(expr.r))
       }
  }

}
