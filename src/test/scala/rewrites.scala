package ohnosequences.test

import ohnosequences.gadt._
import ohnosequences.rewrites._

class RewriteTest extends org.scalatest.FunSuite {
  import ohnosequences.rules._

    val boo = BoolVar('boo)

    val neg2 = Not(Not(boo))
    val neg3 = Not(neg2)
    val neg4 = Not(neg3)

  test("rewriting double negation") {

    assert{ rewrite(neg2) == boo }
    assert{ rewrite(neg3) == Not(boo) }
  }

  test("rewriting double negation recursively") {
    import AnyRewrite._
    assert{ rewrite(neg4) == boo }
    // assert{ rewrite(neg4)(recRewrite(doubleNegation, recRewrite(doubleNegation, idRewrite[BoolVar]))) == boo }
    // println(doubleNegation(neg4))
  }

  // test("rewriting if-then-else") {
  //   import ohnosequences.rules._
  //
  //   val c = BoolVar('c)
  //   val t = IntVar('t)
  //   val f = IntVar('f)
  //
  //   val ite = IntIte(Not(c), t, f)
  //   assert{ rewrite(ite) == IntIte(c, f, t) }
  // }
}
