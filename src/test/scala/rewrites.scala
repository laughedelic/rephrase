package ohnosequences.test

import ohnosequences.expr._
import ohnosequences.rewriting._
import ohnosequences.strategies._
import ohnosequences.cosas._, fns._

class RewriteTest extends org.scalatest.FunSuite {

  val boo = BoolVar('boo)

  val neg2 = Not(Not(boo))
  val neg3 = Not(neg2)
  val neg4 = Not(neg3)

  test("rewriting double negation") {

    assert{ doubleNegation(neg2) == boo }
    assert{ doubleNegation(neg3) == Not(boo) }
  }

  // NOTE: this is handled by the doubleNegation.eliminate rule
  test("rewriting double negation recursively") {

    assert{ doubleNegation(neg4) == boo }
    assert{ doubleNegation(Not(Not(neg4))) == boo }
  }

  // NOTE: this is handled by the rule from RewritesConjunction trait
  test("rewriting double negation inside of a conjunction") {

    assert{ doubleNegation(And(neg2, neg3)) == And(boo, Not(boo)) }

    assertResult(And(boo, And(boo, Not(boo)))){
      doubleNegation(
        Not(Not(
          And(
            neg2,
            And(neg4, neg3)
          )
        ))
      )
    }

  }

}
