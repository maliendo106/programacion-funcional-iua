package rational

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import org.scalacheck.Prop.{BooleanOperators, propBoolean}
import scala.language.adhocExtensions
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.language.implicitConversions

def rationalGen = for
  n <- Gen.chooseNum(-1000, 1000)
  d <- Gen.chooseNum(1, 1000)
yield Rational(n, d)

val Zero = Rational(0)
val One = Rational(1)

given Arbitrary[Rational] = Arbitrary(rationalGen)

class TestRationalExtension extends AnyFunSuite with Checkers:
  test(
    "RationalString debe reconocer strings que representan racionales bien formados"
  ) {
    check { (q: Rational) =>
      val expected = Some(q)
      val was = RationalString.unapply(q.toString)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("RationalString debe rechazar strings que no son racionales válidos") {
    for
      s <- List(
        "",
        "x",
        "/",
        "1/",
        "/2",
        "a/b",
        "x/3",
        "3/z",
        "1.2",
        "1.2/2",
        "3/1.5"
      )
    do
      val expected = None
      val was = RationalString.unapply(s)
      assert(expected == was, s"on input '$s'\n  expected: $expected\n\twas: $was")
  }

  test("RationalString debe rechazar strings con denominador cero") {
    for n <- -10 to 10 do
      val expected = None
      val was = RationalString.unapply(s"$n/0")
      assert(expected == was, s"on input '$n/0'\n  expected: $expected\n\twas: $was")
  }

  test("Rational.unapply debe descomponer un racional") {
    check { (q: Rational) =>
      val expected = Some((q.numer, q.denom))
      val was = Rational.unapply(q)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Rational.unapply debe ser irrefutable") {
    check { (q: Rational) =>
      val Rational(n, d) = q
      val expected = (q.numer, q.denom)
      val was = (n, d)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("abs debe devolver el valor absoluto") {
    check { (q: Rational) =>
      val expected = if q < Zero then -q else q
      val was = q.abs
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("""x.pow(0) == one""") {
    check{(x: Rational) => 
      val expected = One
      val was = x.pow(Zero)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("""x.pow(1) == x""") {
    check{(x: Rational) => 
      val expected = x
      val was = x.pow(One)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("""si x != zero, x.pow(-1) == x.inverse""") {
    check{(x: Rational) => 
      (x != Zero) ==> {
        val expected = x.inverse
        val was = x.pow(-One)
        (expected == was) :| s"expected: $expected\n\t  was: $was"
      }
    }
  }

  test("""Rational(n).pow(e) == Rational(math.pow(n,e).toInt)""") {
    check { (a: Byte, b: Byte) =>
      val e = b.abs % 4 + 1
      val rat = Rational(a)
      val expected = Rational(math.pow(a, e).toInt)
      val was = rat.pow(Rational(e))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("""Potencias de números conocidos""") {
    for n <- -10 to 10; d <- 1 to 10; e <- 1 to 6 do
      val expected = Rational(math.pow(n, e).toInt, math.pow(d, e).toInt)
      val was = Rational(n, d).pow(Rational(e))
      assert(
        expected == was,
        s"on input pow(Rational($n,$d), $e)\n  expected: $expected\n\twas: $was"
      )
  }

  test("""Potencias negativas de números conocidos""") {
    for n <- (-10 to -1) ++ (1 to 10); d <- 1 to 10; e <- 1 to 6 do
      val expected = Rational(math.pow(d, e).toInt, math.pow(n, e).toInt)
      val was = Rational(n, d).pow(Rational(-e))
      assert(
        expected == was,
        s"on input pow(Rational($n,$d), -$e)\n  expected: $expected\n\twas: $was"
      )
  }

  test("Potencias no enteras lanzan IllegalArgumentException") {
    for n <- (-10 to -1) ++ (1 to 10); d <- 1 to 10; e <- 2 to 6 do
      assertThrows[IllegalArgumentException](
        Rational(n, d).pow(Rational(1, e))
      )
  }

  test("Potencias negativas de cero lanzan IllegalArgumentException"){
    for e <- 1 to 10 do
      assertThrows[IllegalArgumentException]{
        Zero.pow(Rational(-e))
      }
  }
