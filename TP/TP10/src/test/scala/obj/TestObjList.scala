package obj
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.{propBoolean, BooleanOperators}
import org.scalacheck.{Gen, Arbitrary}
import Gen.{frequency, const}
import Arbitrary.arbitrary
import scala.collection.immutable.{List as SList}
import scala.language.adhocExtensions

given [T](using Arbitrary[T]): Arbitrary[List[T]] =
  Arbitrary {
    val genEmpty = const(Empty)
    def genCons: Gen[List[T]] = for
      head <- arbitrary[T]
      tail <- genList
    yield head ::: tail
    def genList: Gen[List[T]] = frequency((1, genEmpty), (4, genCons))
    genList
  }

extension [T](xs: List[T])
  def toScalaList: SList[T] = xs.foldRight(SList.empty[T])(_ :: _)


class TestObjList extends AnyFunSuite with Checkers:
  test("Empty.isEmpty debe ser verdadero") {
    assert(Empty.isEmpty)
  }
  test("List.empty.isEmpty debe ser verdadero") {
    assert(List.empty.isEmpty)
  }
  test("(1:::Empty).isEmpty debe ser falso") {
    assert(!(1 ::: Empty).isEmpty)
  }
  test("(1:::Empty).tail.isEmpty debe ser verdadero") {
    assert((1 ::: Empty).tail.isEmpty)
  }
  test("Empty.head debe lanzar NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      Empty.head
    }
  }
  test("Empty.tail debe lanzar UnsupportedOperationException") {
    assertThrows[UnsupportedOperationException] {
      Empty.tail
    }
  }
  test("apply() debe ser igual a Empty") {
    assert(List() == Empty)
  }
  test("apply(1,2,3) debe ser igual a 1:::2:::3:::Empty)") {
    assert(List(1, 2, 3) == 1 ::: 2 ::: 3 ::: Empty)
  }
  test("Empty.length debe ser 0") {
    assert(0 === Empty.length)
  }
  test("(x:::xs).length debe ser 1 + xs.length") {
    check{(x: Int, xs: List[Int]) =>
      val expected = 1 + xs.length
      val was = (x ::: xs).length
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: ${(x:::xs).toScalaList}"
    }
  }
  test("(0:::Empty).length debe ser 1") {
    assert(1 === (0 ::: Empty).length)
  }
  test("List(args*).length debe ser args.length") {
    check{(args: SList[Int]) =>
      val expected = args.length
      val was = (List(args*)).length
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    }
  }
  test("(x:::xs).head tiene que ser x") {
    check{(x: Int, xs: List[Int]) =>
      val expected = x
      val was = (x ::: xs).head
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: ${(x:::xs).toScalaList}"
    }
  }
  test("List(args*).head debe ser args.head si !args.isEmpty") {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.head
        val was = (List(args*)).head
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test("(x:::xs).tail tiene que ser xs") {
    check{(x: Int, xs: List[Int]) =>
      val expected = xs
      val was = (x ::: xs).tail
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: ${(x:::xs).toScalaList}"
    }
  }
  test("List(args*).tail debe ser List(args.tail*) si !args.isEmpty") {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = List(args.tail*)
        val was = (List(args*)).tail
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test("List(args*).foldLeft(0)(_-_) debe ser args.foldLeft(0)(_-_)") {
    check((args: SList[Int]) =>
      val expected = args.foldLeft(0)(_ - _)
      val was = List(args*).foldLeft(0)(_ - _)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    )
  }
  test("List(args*).foldRight(0)(_-_) debe ser args.foldRight(0)(_-_)") {
    check((args: SList[Int]) =>
      val expected = args.foldRight(0)(_ - _)
      val was = List(args*).foldRight(0)(_ - _)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    )
  }
  test("List(args*).foldRight(List())(_::_) debe ser igual a args") {
    check((args: SList[Int]) =>
      val expected = args
      val was = List(args*).foldRight(SList.empty[Int])(_ :: _)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    )
  }
  test(
    "List(args*).foldLeft(List())((z,x) => x::z) debe ser igual a args.reverse"
  ) {
    check((args: SList[Int]) =>
      val expected = args.reverse
      val was = List(args*).foldLeft(SList.empty[Int])((z, x) => x :: z)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    )
  }
  test("List(args*).reverse == List(args.reverse*)") {
    check{(args: SList[Int]) =>
      val expected = List(args.reverse*)
      val was = List(args*).reverse
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    }
  }
  test("xs.reverse.reverse == xs") {
    check{(xs: List[Char]) =>
      val expected = xs
      val was = xs.reverse.reverse
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: ${xs.toScalaList}"
    }
  }
  test(
    "List(args*).reduceLeft(math.max) debe ser igual a args.max si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.max
        val was = List(args*).reduceLeft(math.max)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List(args*).reduceRight(math.min) debe ser igual a args.min si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.max
        val was = List(args*).reduceRight(math.max)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List(args*).reduceLeft((z,x) => z) debe ser igual a args.head si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.head
        val was = List(args*).reduceLeft((z, x) => z)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List(args*).reduceRight((x,z) => z) debe ser igual a args.last si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.last
        val was = List(args*).reduceRight((x, z) => z)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List(args*).reduceLeft((z,x) => x) debe ser igual a args.last si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.last
        val was = List(args*).reduceLeft((z, x) => x)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List(args*).reduceRight((x,z) => x) debe ser igual a args.head si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.head
        val was = List(args*).reduceRight((x, z) => x)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List(args*).reduceLeft(_-_) debe ser args.tail.foldLeft(args.head)(_-_) si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.tail.foldLeft(args.head)(_ - _)
        val was = List(args*).reduceLeft(_ - _)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List(args*).reduceRight(_-_) debe ser args.init.foldRight(args.last)(_-_) si !args.isEmpty"
  ) {
    check((args: SList[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.init.foldRight(args.last)(_ - _)
        val was = List(args*).reduceRight(_ - _)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test(
    "List.empty[Int].reduceLeft(_+_) debe lanzar UnsupportedOperationException"
  ) {
    assertThrows[UnsupportedOperationException] {
      List.empty[Int].reduceLeft(_ + _)
    }
  }
  test(
    "List.empty[Int].reduceRight(_+_) debe lanzar UnsupportedOperationException"
  ) {
    assertThrows[UnsupportedOperationException] {
      List.empty[Int].reduceRight(_ + _)
    }
  }
  test("List.empty[Int].sum tiene que ser 0") {
    assert(List.empty[Int].sum == 0)
  }
  test("(x:::xs).sum tiene que ser x + xs.sum") {
    check{(x: Int, xs: List[Int]) =>
      val expected = x + xs.sum
      val was = (x ::: xs).sum
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: ${(x:::xs).toScalaList}"
    }
  }
  test("List(args*).sum debe ser equivalente a args.sum") {
    check{(args: SList[Int]) =>
      val expected = args.sum
      val was = List(args*).sum
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    }
  }
  test("List.empty[Int].product tiene que ser 1") {
    assert(List.empty[Int].product == 1)
  }
  test("(x:::xs).product tiene que ser x * xs.product") {
    check{(x: Int, xs: List[Int]) =>
      val expected = x * xs.product
      val was = (x ::: xs).product
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: ${(x:::xs).toScalaList}"
    }
  }
  test("List(args*).product debe ser equivalente a args.product") {
    check{(args: SList[Int]) =>
      val expected = args.product
      val was = List(args*).product
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    }
  }
  test("List(args*).max debe ser igual a args.max si !args.isEmpty") {
    check((args: SList[String]) =>
      (!args.isEmpty) ==> {
        val expected = args.max
        val was = List(args*).max
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test("List(args*).min debe ser igual a args.min si !args.isEmpty") {
    check((args: SList[String]) =>
      (!args.isEmpty) ==> {
        val expected = args.min
        val was = List(args*).min
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test("List.empty[Int].max debe lanzar UnsupportedOperationException") {
    assertThrows[UnsupportedOperationException] {
      List.empty[Int].max
    }
  }
  test("List.empty[Int].min debe lanzar UnsupportedOperationException") {
    assertThrows[UnsupportedOperationException] {
      List.empty[Int].min
    }
  }
  test("List(args*).find(p) debe ser equivalente a args.find(p)") {
    check((args: SList[Byte], x: Byte) =>
      val expected = args.find(_ == x)
      val was = List(args*).find(_ == x)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    )
  }
  test("List(args*).find(_ => false) debe ser None") {
    check{(args: SList[Byte]) =>
      val expected = None
      val was = List(args*).find(_ => false)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
    }
  }
  test(
    "List(args*).find(_ => true) debe ser Some(args.head) si !args.isEmpty"
  ) {
    check((args: SList[Byte]) =>
      (!args.isEmpty) ==> {
        val expected = Some(args.head)
        val was = List(args*).find(_ => true)
        (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $args"
      }
    )
  }
  test("List().find(p) debe ser None") {
    check{(x: Int) =>
      val expected = None
      val was = List.empty[Int].find(_ == x)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n  with input: $x"
    }
  }
  test("List(args*).mkString(s) debe ser equivalente a args.mkString(s)") {
    check((args: SList[Char], s: String) =>
      val expected = args.mkString(s)
      val was = List(args*).mkString(s)
      (expected == was) :| s"expected: '$expected'\n\t  was: '$was'\n  with input: $args and '$s'"
    )
  }
  test(
    "List(args*).mkString(s,sep,e) debe ser equivalente a args.mkString(s,sep,e)"
  ) {
    check((args: SList[Char], s: String, sep: String, e: String) =>
      val expected = args.mkString(s, sep, e)
      val was = List(args*).mkString(s, sep, e)
      (expected == was) :| s"expected: '$expected'\n\t  was: '$was'\n  with input: $args and ('$s', '$sep' and '$e')"
    )
  }
  test("List().mkString(s) debe ser equivalente a List().mkString(s)") {
    check((s: String) =>
      val expected = SList().mkString(s)
      val was = List().mkString(s)
      (expected == was) :| s"expected: '$expected'\n\t  was: '$was'\n  with input: '$s'"
    )
  }
  test(
    "List().mkString(s,sep,e) debe ser equivalente a List().mkString(s,sep,e)"
  ) {
    check((s: String, sep: String, e: String) =>
      val expected = SList().mkString(s, sep, e)
      val was = List().mkString(s, sep, e)
      (expected == was) :| s"expected: '$expected'\n\t  was: '$was'\n  with input: '$s', '$sep' and '$e'"
    )
  }
end TestObjList
