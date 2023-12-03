package rational
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.{propBoolean, BooleanOperators}
import scala.collection.immutable.{List as SList}
import obj.*
import scala.language.adhocExtensions

class TestRationalList extends AnyFunSuite with Checkers:
  test("Suma de racionales") {
    check((args: SList[Rational]) =>
      List(args*).sum == args.foldLeft(ZERO)(_ + _)
    )
  }
  test("Suma de enteros") {
    check((ints: SList[Int]) =>
      val expected = Rational(ints.sum)
      val was = List(ints.map(Rational(_))*).sum
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $ints"
    )
  }
  test("Producto de enteros") {
    check { (bytes: SList[Byte]) =>
      val ints = bytes.filter(_ != 0).map(_.toInt)
      val expected = Rational(ints.product)
      val was = List(ints.map(Rational(_))*).product
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $ints"
    }
  }
  test("Producto de racionales") {
    check { (nbytes: SList[Byte], dbytes: SList[Byte]) =>
      val tuples = nbytes.filter(_ != 0).map(_.toInt).zip(dbytes.filter(_ != 0).map(_.toInt).map(_.abs)).take(4)
      val rats = tuples.map{case (n, d) => Rational(n, d)}
      val tuplesProduct = tuples.foldLeft((1, 1))((a, b) => (a._1*b._1, a._2*b._2))
      val expected = Rational(tuplesProduct._1, tuplesProduct._2)
      val was = List(rats*).product
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $rats"
    }
  }
end TestRationalList
