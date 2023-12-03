package rational
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.{propBoolean, BooleanOperators}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.language.adhocExtensions

val ZERO = Rational(0)
val ONE = Rational(1)

def gcd(a: Int, b: Int): Int =
  if b == 0 then a
  else gcd(b, a % b)

def ratInverse(q: Rational) = Rational(q.denom, q.numer)

def rationalGen = for
  n <- Gen.chooseNum(-1000, 1000)
  d <- Gen.chooseNum(1, 1000)
yield Rational(n, d)

given Arbitrary[Rational] = Arbitrary(rationalGen)

def reduce(n: Int) = if n < 0 then -1 else if n > 0 then 1 else 0

class TestRationalNumeric extends AnyFunSuite with Checkers:
  val num = summon[Numeric[Rational]]
  test("Conversión de enteros") {
    check((a: Int) =>
      val expected = Rational(a)
      val was = num.fromInt(a)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $a"
    )
  }
  test("zero en Numeric[Rational] debe ser Rational(0)") {
    assert(num.zero === Rational(0))
  }
  test("one en Numeric[Rational] debe ser Rational(1)") {
    assert(num.one === Rational(1))
  }
  test("toInt(fromInt(n)) debe ser n") {
    check((x: Int) => 
      val expected = x
      val was = num.toInt(num.fromInt(x))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $x"
    )
  }
  test("toDouble(Rational(n,d)) debe ser n/d: Double") {
    check((n: Byte, d: Byte) =>
      (d != 0) ==> {
        val expected = n.toDouble / d.toDouble
        val was = num.toDouble(Rational(n, d))
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $n/$d"
      }
    )
  }
  test("toFloat(Rational(n,d)) debe ser n/d: Float") {
    check((n: Byte, d: Byte) =>
      (d != 0) ==> {
        val expected = n.toFloat / d.toFloat
        val was = num.toFloat(Rational(n, d))
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $n/$d"
      }
    )
  }
  test("toInt(Rational(n,d)) debe ser n/d : Int") {
    check((n: Byte, d: Byte) =>
      (d != 0) ==> {
        val expected = n.toInt / d.toInt
        val was = num.toInt(Rational(n, d))
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $n/$d"
      }
    )
  }
  test("toLong(Rational(n,d)) debe ser n/d: Long") {
    check((n: Byte, d: Byte) =>
      (d != 0) ==> {
        val expected = n.toLong / d.toLong
        val was = num.toLong(Rational(n, d))
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $n/$d"
      }
    )
  }
  test("plus(a,b) debe ser a + b") {
    check((a: Rational, b: Rational) =>
      val expected = a + b
      val was = num.plus(a, b)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $a, $b"
    )
  }
  test("minus(a,b) debe ser a - b") {
    check((a: Rational, b: Rational) =>
      val expected = a - b
      val was = num.minus(a, b)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $a, $b"
    )
  }
  test("negate(a) debe ser -a") {
    check((a: Rational, b: Rational) =>
      val expected = -a
      val was = num.negate(a)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $a"
    )
  }
  test("times(a,b) debe ser a * b") {
    check((a: Rational, b: Rational) =>
      val expected = a * b
      val was = num.times(a, b)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $a, $b"
    )
  }
  test("compare(a,b) debe ser correcto") {
    check((a: Rational, b: Rational) =>
      val expected = reduce(a.compare(b))
      val was = reduce(num.compare(a, b))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $a, $b"
    )
  }
  test("parseString(s) debe producir el numero racional correcto") {
    check((a: Rational) => 
      val expected = Some(a)
      val was = num.parseString(a.toString)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $a"
    )
  }
end TestRationalNumeric
