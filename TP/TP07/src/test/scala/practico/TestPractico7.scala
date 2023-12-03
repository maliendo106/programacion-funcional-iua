package practico

import compiletime.asMatchable

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalactic.Equality
import Practico7.{length as len, *}

val smallBigInts = for n <- Gen.chooseNum(BigInt(-10000), BigInt(10000)) yield n
val rangeLengths = for n <- Gen.chooseNum(BigInt(-100), BigInt(100)) yield n
val exponents = for n <- Gen.chooseNum(0, 100) yield n

given Equality[(Double, Double)] with
  def areEqual(a: (Double, Double), b: Any): Boolean = b.asMatchable match
    case bb: (Double, Double)@unchecked =>
      val (x1, y1) = a
      val (x2, y2) = bb
      math.sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)) === 0.0 +- 1e-6
    case _ => false

class TestPractico7 extends AnyFunSuite with ScalaCheckPropertyChecks:
  test("Este práctico debe ser el 7"):
    assertResult("7")(Practico7.practico)

  test("max sobre lista vacía debe lanzar una excepción"):
    assertThrows[java.util.NoSuchElementException]:
      max(List.empty[Int])

  test("length"):
    forAll("list") { (list: List[Int]) =>
      len(list) `should` be(list.length)
    }

  test("sum"):
    forAll("list") { (list: List[Int]) =>
      sum(list) `should` be(list.sum)
    }

  test("max"):
    forAll("list") { (list: List[Int]) =>
      whenever(!list.isEmpty):
        max(list) `should` be(list.max)
    }

  test("factorial"):
    val results = Vector[BigInt](1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880,
      3628800, 39916800, 479001600, 6227020800L, 87178291200L, 1307674368000L)
    for (result, input) <- results.zipWithIndex do
      factorial(input) `should` be(result)

  test("factorial de números negativos debe lanzar IllegalArgumentException"):
    forAll("n") { (n: Int) =>
      whenever(n < 0):
        an[IllegalArgumentException] `should` be `thrownBy` factorial(n)
    }

  test("fibonacci"):
    val results = Vector(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377,
      610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025,
      121393, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578,
      5702887, 9227465, 14930352, 24157817, 39088169, 63245986, 102334155,
      165580141, 267914296, 433494437, 701408733, 1134903170, 1836311903,
      2971215073L, 4807526976L, 7778742049L, 12586269025L)
    for (result, input) <- results.zipWithIndex do
      fibo(input) `should` be(result)

  test("fibonacci de números negativos debe lanzar IllegalArgumentException"):
    forAll("n") { (n: Int) =>
      whenever(n < 0):
        an[IllegalArgumentException] `should` be `thrownBy` fibo(n)
    }

  test("pow con exponente 0 debe dar 1"):
    forAll("x") { (x: Int) =>
      pow(x, 0) `should` be(1)
    }

  test("pow con exponente 1 debe dar el mismo número"):
    forAll("x") { (x: Int) =>
      pow(x, 1) `should` be(x)
    }

  test("pow(x,2*n) debe ser igual a pow(x,n)*pow(x,n)"):
    def square(x: BigInt) = x * x
    forAll((smallBigInts, "x"), (exponents, "n")) { (x: BigInt, n: Int) =>
      whenever(n >= 0):
        pow(x, 2 * n) `should` be(square(pow(x, n)))
    }

  test("pow(x,n+1) debe ser igual a pow(x,n)*x"):
    forAll((smallBigInts, "x"), (exponents, "n")) { (x: BigInt, n: Int) =>
      whenever(n >= 0):
        pow(x, n + 1) `should` be(x * pow(x, n))
    }

  test("pow con exponente negativo"):
    forAll((smallBigInts, "x"), (exponents, "n")) { (x: BigInt, n: Int) =>
      whenever(n > 0):
        an[IllegalArgumentException] `should` be `thrownBy` pow(x, -n)
    }

  test("sumrange"):
    forAll((smallBigInts, "start"), (rangeLengths, "rangeLength")):
      (start: BigInt, rangeLength: BigInt) =>
        sumrange(start, start + rangeLength) `should` be(
          (start to start + rangeLength).sum
        )

  test("sumsquares"):
    def square(x: BigInt) = x * x
    forAll((smallBigInts, "start"), (rangeLengths, "rangeLength")):
      (start: BigInt, rangeLength: BigInt) =>
        sumsquares(start, start + rangeLength) `should` be(
          (start to start + rangeLength).map(square).sum
        )

  test("sumcubes"):
    def cube(x: BigInt) = x * x * x
    forAll((smallBigInts, "start"), (rangeLengths, "rangeLength")):
      (start: BigInt, rangeLength: BigInt) =>
        sumcubes(start, start + rangeLength) `should` be(
          (start to start + rangeLength).map(cube).sum
        )

  test("sumpowers"):
    forAll(
      (smallBigInts, "start"),
      (rangeLengths, "rangeLength"),
      (exponents, "n")
    ) { (start: BigInt, rangeLength: BigInt, n: Int) =>
      sumpowers(start, start + rangeLength, n) `should` be(
        (start to start + rangeLength).map(x => x.pow(n)).sum
      )
    }

  test("sumpowers con exponente negativo"):
    forAll(
      (smallBigInts, "start"),
      (rangeLengths, "rangeLength"),
      (exponents, "n")
    ) { (start: BigInt, rangeLength: BigInt, n: Int) =>
      whenever(n > 0 && rangeLength > 0):
        an[IllegalArgumentException] `should` be `thrownBy` sumpowers(
          start,
          start + rangeLength,
          -n
        )
    }

  test("quadraticRoots"):
    val square: ((Double, Double)) => (Double, Double) = { case (a, b) =>
      (a * a - b * b, 2 * a * b)
    }
    val mul: ((Double, Double), Double) => (Double, Double) =
      case ((a, b), x) => (a * x, b * x)
    val add: ((Double, Double), (Double, Double), (Double, Double)) => (
        Double,
        Double
    ) = { case ((a1, a2), (b1, b2), (c1, c2)) => (a1 + b1 + c1, a2 + b2 + c2) }

    def eval(a: Double, b: Double, c: Double, x: (Double, Double)) =
      add(mul(square(x), a), mul(x, b), (c, 0))

    val zero = (0.0, 0.0)
    forAll { (a: Short, b: Short, c: Short) =>
      whenever(a != 0):
        val (x1, x2) = quadraticRoots(a, b, c)
        eval(a, b, c, x1) `should` equal(zero)
        eval(a, b, c, x2) `should` equal(zero)
    }

  test("quadraticRoots con coeficiente a == 0"):
    forAll { (b: Short, c: Short) =>
      an[IllegalArgumentException] `should` be `thrownBy` quadraticRoots(
        0,
        b,
        c
      )
    }

  test("balance: '(if (zero? x) max (/ 1 x))' está equilibrado"):
    assert(balance("(if (zero? x) max (/ 1 x))"))

  test("balance: 'I told him ...' está equilibrado"):
    assert(
      balance(
        "I told him (that it's not (yet) done).\n(But he wasn't listening)"
      )
    )

  test("balance: ':-)' no está equilibrado"):
    assert(!balance(":-)"))

  test("balance: contar no es suficiente"):
    assert(!balance("())("))
