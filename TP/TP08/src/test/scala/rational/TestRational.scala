package rational

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import org.scalacheck.Prop.{BooleanOperators, propBoolean}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.language.implicitConversions

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

class TestRational extends AnyFunSuite with Checkers:
  test("""Rational(n,d).numer/Rational(n,d).denom debe ser n/d""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val rat = Rational(n, d)
        n * rat.denom === rat.numer * d
      }
    }
  }
  test("""gcd(Rational(n,d).numer,Rational(n,d).denom) debe ser 1""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val rat = Rational(n, d)
        1 === gcd(rat.numer.abs, rat.denom.abs)
      }
    }
  }
  test("""Rational(n,d).toString debe ser "numer/denom" """) {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val rat = Rational(n, d)
        (rat.denom != 1) ==> (s"${rat.numer}/${rat.denom}" === rat.toString)
      }
    }
  }

  test("""Los enteros tienen que tener denominador 1""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val rat = Rational(n * d, d)
        rat.numer == n && rat.denom == 1
      }
    }
  }

  test("""Rational(n).denom debe ser 1 y Rational(n).numer debe ser n""") {
    check { (n: Short) =>
      val rat = Rational(n)
      rat.numer == n && rat.denom == 1
    }
  }
  test("""Rational(n).toString debe ser "numer" """) {
    check { (n: Short) =>
      val rat = Rational(n)
      s"${rat.numer}" === rat.toString
    }
  }

  test("""Rational(Rational(a,b).toString) debe ser Rational(a,b)"""){
    check { (q: Rational) =>
      val r = Rational(q.toString)
      r.numer == q.numer && r.denom == q.denom
    }
  }

  test("""Rational(n.toString) debe ser Rational(n) con n:Int"""){
    check { (n: Int) =>
      val r = Rational(n.toString)
      r.numer == n && r.denom == 1
    }
  }

  test("""Si el denominador es 0, lanza IllegalArgumentException""") {
    for i <- -50 to 50 do
      assertThrows[IllegalArgumentException] {
        Rational(i, 0)
      }
  }

  test("""Si apply recibe una string inválida, lanza NumberFormatException""") {
    for s <- List("", "x", "/", "1/", "/2", "a/b", "x/3", "3/z", "1.2", "1.2/2", "3/1.5") do
      println(s)
      assertThrows[NumberFormatException]{
        Rational(s)
      }
  }

  test("""Si apply recibe una string con denominador  0, lanza IllegalArgumentException""") {
    for i <- -50 to 50 do
      assertThrows[IllegalArgumentException] {
        Rational(s"$i/0")
      }
  }

  
  test(
    """Si n >=0 && d<0, numer tiene que ser negativo y denom tiene que ser positivo"""
  ) {
    check { (a: Short, b: Short) =>
      val n: Int = a.toInt.abs
      val d = -1 - b.toInt.abs
      val rat = Rational(n, d)
      n * rat.denom == d * rat.numer && rat.numer <= 0 && rat.denom > 0
    }
  }

  test(
    """Si n <=0 && d>0, numer tiene que ser negativo y denom tiene que ser positivo"""
  ) {
    check { (a: Short, b: Short) =>
      val n: Int = -(a.toInt.abs)
      val d: Int = 1 + b.toInt.abs
      val rat = Rational(n, d)
      n * rat.denom == d * rat.numer && rat.numer <= 0 && rat.denom > 0
    }
  }

  test("""Si n == 0, numer tiene que ser 0 y denom 1""") {
    check { (d: Int) =>
      (d != 0) ==> {
        val rat = Rational(0, d)
        rat.numer == 0 && rat.denom == 1
      }
    }
  }

  test("""equals: x == x""") {
    check((a: Short, b: Short) =>
      (b != 0) ==> (Rational(a, b) == Rational(a, b))
    )
  }

  test("""equals: Rational(k*n,k*d) == Rational(n,d)""") {
    check((a: Short, b: Short, k: Short) =>
      (b != 0 && k != 0) ==> (Rational(a * k, b * k) == Rational(a, b))
    )
  }

  test("""equals: x == y ==> x.numer == y.numer && x.denom == y.denom""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val x = Rational(n, d)
        val y = Rational(2 * n, 2 * d)
        x == y && x.denom == y.denom && x.numer == y.numer
      }
    }
  }

  test("""hashCode: x == y ==> x.hashCode == y.hashCode""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val x = Rational(n, d)
        val y = Rational(2 * n, 2 * d)
        x.hashCode == y.hashCode
      }
    }
  }

  test("""hashCode: números cercanos deberían tener hashCodes distintos""") {
    check { (n: Short, d: Short) =>
      (d > 1) ==> {
        val set = Set(
          Rational(n, d),
          Rational(n + 1, d),
          Rational(n, d + 1),
          Rational(n - 1, d + 1),
          Rational(n + 1, d - 1),
          Rational(n - 1, d),
          Rational(n, d - 1)
        )
        set.size == set.map(_.hashCode).size
      }
    }
  }

  test("""equals: x != y ==> (x.numer != y.numer || x.denom != y.denom)""") {
    check((x: Rational, y: Rational) =>
      (x != y) ==> ((x.numer != y.numer) || (x.denom != y.denom))
    )
  }

  test("""x.numer != y.numer ==> x != y""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val x = Rational(n, d)
        val y = Rational(n + 1, d)
        x != y
      }
    }
  }

  test("""x.numer == y.numer != 0 && x.denom != y.denom ==> x != y""") {
    check { (n: Short, d: Short) =>
      (n != 0 && d > 0) ==> {
        val x = Rational(n, d)
        val y = Rational(n, d + 1)
        x != y
      }
    }
  }

  test("""n/d + 0/1 == n/d""") {
    check { (x: Rational) => x + ZERO == x }
  }

  test("""0/1 + n/d == n/d""") {
    check((x: Rational) => ZERO + x == x)
  }

  test("""n/d + 0 == n/d""") {
    check((x: Rational) => x + 0 == x)
  }

  test("""0 + n/d == n/d""") {
    check((x: Rational) => 0 + x == x)
  }

  test("""Rational(n,d)+Rational(n,d) == Rational(n+n,d)""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val rat = Rational(n, d)
        rat + rat == Rational(n + n, d)
      }
    }
  }

  test("""Rational(n,d)+Rational(-n,d) == 0""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        Rational(n, d) + Rational(-n, d) == ZERO
      }
    }
  }

  test("""n/d - 0/1 == n/d""") {
    check((x: Rational) => x - ZERO == x)
  }

  test("""0/1 - n/d == -n/d""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        ZERO - Rational(n, d) == Rational(-n, d)
      }
    }
  }

  test("""n/d - 0 == n/d""") {
    check((x: Rational) => x - 0 == x)
  }

  test("""0 - n/d == n/d""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        val rat = Rational(n, d)
        0 - Rational(n, d) == Rational(-n, d)
      }
    }
  }

  test("""x-x == zero""") {
    check((x: Rational) => x - x == ZERO)
  }

  test("""Rational(n,d)-Rational(-n,d) == Rational(n+n,d)""") {
    check { (n: Short, d: Short) =>
      (d != 0) ==> {
        Rational(n, d) - Rational(-n, d) == Rational(n + n, d)
      }
    }
  }

  test("""x*zero == zero""") {
    check((x: Rational) => x * ZERO == ZERO)
  }

  test("""x*0 == zero""") {
    check((x: Rational) => x * 0 == ZERO)
  }

  test("""x*one == x""") {
    check((x: Rational) => x * ONE == x)
  }

  test("""x*1 == x""") {
    check((x: Rational) => x * 1 == x)
  }

  test("""zero*x == zero""") {
    check((x: Rational) => ZERO * x == ZERO)
  }

  test("""0*x == zero""") {
    check((x: Rational) => 0 * x == ZERO)
  }

  test("""one*x == x""") {
    check((x: Rational) => ONE * x == x)
  }

  test("""1*x == x""") {
    check((x: Rational) => 1 * x == x)
  }

  test("""2*x == x+x""") {
    check((x: Rational) => 2 * x == x + x)
  }

  test("""x*2 == x+x""") {
    check((x: Rational) => x * 2 == x + x)
  }

  test("""one/zero lanza IllegalArgumentException""") {
    intercept[IllegalArgumentException] {
      ONE / ZERO
    }
  }

  test("""one/0 lanza IllegalArgumentException""") {
    intercept[IllegalArgumentException] {
      ONE / 0
    }
  }

  test("""x/one == x""") {
    check((x: Rational) => x / ONE == x)
  }

  test("""x/1 == x""") {
    check((x: Rational) => x / 1 == x)
  }

  test("""zero/x == zero""") {
    check((x: Rational) => (x != ZERO) ==> (ZERO / x == ZERO))
  }

  test("""0/x == zero""") {
    check((x: Rational) => (x != ZERO) ==> (0 / x == ZERO))
  }

  test("""one/(n/d) == (d/n)""") {
    check((x: Rational) => (x != ZERO) ==> (ONE / x == ratInverse(x)))
  }

  test("""1/(n/d) == (d/n)""") {
    check((x: Rational) => (x != ZERO) ==> (1 / x == ratInverse(x)))
  }

  test("""x/y == x*(1/y)""") {
    check((x: Rational, y: Rational) =>
      (y != ZERO) ==> (x / y == x * ratInverse(y))
    )
  }

  test("""x/x == one""") {
    check((x: Rational) => (x != ZERO) ==> (x / x == ONE))
  }

  test("""x*x/x == x""") {
    check((x: Rational) => (x != ZERO) ==> (x * x / x == x))
  }

  test("""-x.numer == -(x.numer) && -x.denom == x.denom""") {
    check((x: Rational) => (-x).numer == -(x.numer) && (-x).denom == x.denom)
  }

  test("""-x == zero - x""") {
    check((x: Rational) => -x == ZERO - x)
  }

  test("""-x + x == zero""") {
    check((x: Rational) => -x + x == ZERO)
  }

  test("""-(-x) == x""") {
    check((x: Rational) => -(-x) == x)
  }

  test(
    """Si x > zero, x.numer == x.inverse.denom && x.denom == x.inverse.numer"""
  ) {
    check((x: Rational) =>
      (x.numer > 0) ==> (x.numer == x.inverse.denom && x.denom == x.inverse.numer)
    )
  }

  test(
    """Si < zero, x.numer == -x.inverse.denom && x.denom == -x.inverse.numer"""
  ) {
    check((x: Rational) =>
      (x.numer < 0) ==> (x.numer == -x.inverse.denom && x.denom == -x.inverse.numer)
    )
  }

  test("""Si x!= zero, x*x.inverse.numer == one""") {
    check((x: Rational) => (x != ZERO) ==> (x * x.inverse == ONE))
  }

  test("""Si x!= zero, x.inverse.inverse == x""") {
    check((x: Rational) => (x != ZERO) ==> (x.inverse.inverse == x))
  }

  test("""Si x == zero, x.inverse lanza IllegalArgumentException""") {
    intercept[IllegalArgumentException] {
      ZERO.inverse
    }
  }

  test("""x.pow(0) == one""") {
    check((x: Rational) => x.pow(0) == ONE)
  }

  test("""x.pow(1) == x""") {
    check((x: Rational) => x.pow(1) == x)
  }

  test("""si x != zero, x.pow(-1) == x.inverse""") {
    check((x: Rational) => (x != ZERO) ==> (x.pow(-1) == x.inverse))
  }

  test("""Rational(n).pow(e) == Rational(math.pow(n,e).toInt)""") {
    check { (a: Byte, b: Byte) =>
      val e = b.abs % 4 + 1
      val rat = Rational(a)
      rat.pow(e) == Rational(math.pow(a, e).toInt)
    }
  }

  test("""Potencias de números conocidos""") {
    for n <- -10 to 10; d <- 1 to 10; e <- 1 to 6 do
      assert(
        Rational(n, d).pow(e) == Rational(
          math.pow(n, e).toInt,
          math.pow(d, e).toInt
        )
      )
  }

  test("""Potencias negativas de números conocidos""") {
    for n <- (-10 to -1) ++ (1 to 10); d <- 1 to 10; e <- 1 to 6 do
      assert(
        Rational(n, d).pow(-e) == Rational(
          math.pow(d, e).toInt,
          math.pow(n, e).toInt
        )
      )
  }

  test("""si x > 0, x.compare(-x) > 0""") {
    check((x: Rational) => (x.numer > 0) ==> (x.compare(-x) > 0))
  }

  test("""si x < 0, x.compare(-x) < 0""") {
    check((x: Rational) => (x.numer < 0) ==> (x.compare(-x) < 0))
  }

  test("""x.compare(x) == 0""") {
    check((x: Rational) => x.compare(x) == 0)
  }

  test("""x < x+1""") {
    check((x: Rational) => x < x + 1)
  }

  test("""x > x - 1""") {
    check((x: Rational) => x > x - 1)
  }
end TestRational
