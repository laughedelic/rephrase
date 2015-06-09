package ohnosequences.test

import ohnosequences.lang._
import ohnosequences.rephrase._

class RephraseTest extends org.scalatest.FunSuite {

  test("rewriting double negation") {
    import ohnosequences.rules._

    val boo = BoolVar('boo)

    val neg2 = Not(Not(boo))
    assert{ rephrase(neg2) == boo }

    val neg3 = Not(neg2)
    assert{ rephrase(neg3) == Not(boo) }
  }

  test("rewriting if-then-else") {
    import ohnosequences.rules._

    val c = BoolVar('c)
    val t = IntVar('t)
    val f = IntVar('f)

    val ite = IntIte(Not(c), t, f)
    assert{ rephrase(ite) == IntIte(c, f, t) }
  }
}
