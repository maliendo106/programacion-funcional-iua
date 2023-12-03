package fun
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.{propBoolean, BooleanOperators}
import org.scalacheck.{Gen, Arbitrary}
import Gen.{frequency, const}
import Arbitrary.arbitrary
import IntList.*
import scala.language.adhocExtensions
import scala.language.experimental

given Arbitrary[IntList] = Arbitrary {
  val genEmpty = const(Empty)
  def genCons = for
    head <- arbitrary[Int]
    tail <- genList
  yield Cons(head, tail)
  def genList: Gen[IntList] = frequency((1, genEmpty), (4, genCons))
  genList
}

class TestIntList extends AnyFunSuite with Checkers:
  test("isEmpty(Empty) debe ser verdadero") {
    assert(isEmpty(Empty))
  }
  test("isEmpty(IntList.empty) debe ser verdadero") {
    assert(isEmpty(IntList.empty))
  }
  test("isEmpty(Cons(1,Empty)) debe ser falso") {
    assert(!isEmpty(Cons(1, Empty)))
  }
  test("isEmpty(Cons(1,Empty).tail) debe ser verdadero") {
    assert(isEmpty(Cons(1, Empty).tail))
  }
  test("head(Empty) debe lanzar NoSuchElementException") {
    assertThrows[NoSuchElementException] {
      head(Empty)
    }
  }
  test("tail(Empty) debe lanzar UnsupportedOperationException") {
    assertThrows[UnsupportedOperationException] {
      tail(Empty)
    }
  }
  test("head(Cons(x,_)) debe ser x") {
    check{(x: Int, xs: IntList) =>
      val expected = x
      val was = head(Cons(x, xs))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("tail(Cons(x,xs)) debe ser xs") {
    check{(x: Int, xs: IntList) => 
      val expected = xs
      val was = tail(Cons(x, xs))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("IntList(1,2,3) debe ser igual a Cons(1,Cons(2,Cons(3,Empty)))") {
    assert(Cons(1, Cons(2, Cons(3, Empty))) === IntList(1, 2, 3), "on input Cons(1, Cons(2, Cons(3, Empty))")
  }
  test("IntList() debe ser igual a Empty") {
    assert(Empty === IntList())
  }
  test("length(Empty) debe ser 0") {
    assert(0 === length(Empty))
  }
  test("length(Cons(0,Empty)) debe ser 1") {
    assert(1 === length(Cons(0, Empty)))
  }
  test("length(Cons(x,xs)) debe ser 1 + length(xs)") {
    check{(x: Int, xs: IntList) =>
      val expected = 1 + length(xs)
      val was = length(Cons(x, xs))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("length(IntList(args*)) debe ser args.length") {
    check{(args: List[Int]) => 
      val expected = args.length
      val was = length(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n\t  on input: IntList(${args.mkString(",")})"
    }
  }
  test("head(IntList(args*)) debe ser args.head si !args.isEmpty") {
    check{(args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.head
        val was = head(IntList(args*))
        (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"
      }
    }
  }
  test("tail(IntList(args*)) debe ser IntList(args.tail*) si !args.isEmpty") {
    check((args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = IntList(args.tail*)
        val was = tail(IntList(args*))
        (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"
      }
    )
  }
  test("foldl(0)(_-_)(IntList(args*)) debe ser args.foldLeft(0)(_-_)") {
    check{(args: List[Int]) =>
      val expected = args.foldLeft(0)(_ - _)
      val was = foldl(0)(_ - _)(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"
    }
  }
  test("foldr(0)(_-_)(IntList(args*)) debe ser args.foldRight(0)(_-_)") {
    check{(args: List[Int]) =>
      val expected = args.foldRight(0)(_ - _)
      val was = foldr(0)(_ - _)(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"
    }
  }
  test("foldr(List())(_::_)(IntList(args*)) debe ser igual a args") {
    check{(args: List[Int]) =>
      val expected = args
      val was = foldr(List.empty[Int])(_ :: _)(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"
    }
  }
  test(
    "foldl(Empty: IntList)((z, x) => Cons(x, z)) debe ser igual a IntList(args.reverse*)"
  ) {
    check{(args: List[Int]) =>
      val expected = IntList(args.reverse*)
      val was = foldl(Empty: IntList)((z, x) => Cons(x, z))(IntList(args*))
      (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
    }
  }
  test(
    "foldl1(math.max)(IntList(args*)) debe ser igual a args.max si !args.isEmpty"
  ) {
    check{(args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.max
        val was = foldl1(math.max)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    }
  }
  test(
    "foldr1(math.min)(IntList(args*)) debe ser igual a args.min si !args.isEmpty"
  ) {
    check((args: List[Int]) => 
      (!args.isEmpty) ==> {
        val expected = args.min
        val was = foldr1(math.min)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    )
  }
  test(
    "foldl1((z,x) => z)(IntList(args*)) debe ser igual a args.head si !args.isEmpty"
  ) {
    check((args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.head
        val was = foldl1((z, x) => z)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    )
  }
  test(
    "foldr1((x,z) => z)(IntList(args*)) debe ser igual a args.last si !args.isEmpty"
  ) {
    check((args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.last
        val was = foldr1((x, z) => z)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    )
  }
  test(
    "foldl1((z,x) => x)(IntList(args*)) debe ser igual a args.last si !args.isEmpty"
  ) {
    check((args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.last
        val was = foldl1((z, x) => x)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    )
  }
  test(
    "foldr1((x,z) => x)(IntList(args*)) debe ser igual a args.head si !args.isEmpty"
  ) {
    check((args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.head
        val was = foldr1((x, z) => x)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    )
  }
  test(
    "foldl1(_-_)(IntList(args*)) debe ser args.tail.foldLeft(args.head)(_-_) si !args.isEmpty"
  ) {
    check((args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.tail.foldLeft(args.head)(_ - _)
        val was = foldl1(_ - _)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    )
  }
  test(
    "foldr1(_-_)(IntList(args*)) debe ser args.init.foldRight(args.last)(_-_) si !args.isEmpty"
  ) {
    check((args: List[Int]) =>
      (!args.isEmpty) ==> {
        val expected = args.init.foldRight(args.last)(_ - _)
        val was = foldr1(_ - _)(IntList(args*))
        (expected == was) :| s"""expected: $expected\n\t  was: $was\n    on input: IntList(${args.mkString(",")})"""
      }
    )
  }
  test("foldl1(_+_)(Empty) debe lanzar UnsupportedOperationException") {
    assertThrows[UnsupportedOperationException] {
      foldl1(_ + _)(Empty)
    }
  }
  test("foldr1(_+_)(Empty) debe lanzar UnsupportedOperationException") {
    assertThrows[UnsupportedOperationException] {
      foldr1(_ + _)(Empty)
    }
  }
  test("range(a,b) debe ser equivalente a (a until b)") {
    check{(a: Byte, b: Byte) =>
      val expected = IntList(a.toInt until b*)
      val was = range(a, b)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("range(a,b,2) debe ser equivalente a (a until b by 2)") {
    check{(a: Byte, b: Byte) =>
      val expected = IntList(a.toInt until b by 2*)
      val was = range(a, b, 2)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("range(a,b,-1) debe ser equivalente a (a until b by -1)") {
    check{(a: Byte, b: Byte) =>
      val expected = IntList(a.toInt until b by -1*)
      val was = range(a, b, -1)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("range(n) debe ser equivalente a 0 until n") {
    check{(n: Byte) =>
      val expected = IntList(0 until n*)
      val was = range(n)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("sum(Empty) debe ser 0") {
    assert(sum(Empty) == 0)
  }
  test("sum(Cons(x,xs)) debe ser x + sum(xs)") {
    check{(x: Int, xs: IntList) => 
      val expected = x + sum(xs)
      val was = sum(Cons(x, xs))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("sum(IntList(args*) debe ser equivalente a args.sum") {
    check{(args: List[Int]) => 
      val expected = args.sum
      val was = sum(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("product(Empty) debe ser 1") {
    assert(product(Empty) == 1)
  }
  test("product(Cons(x,xs)) debe ser x * product(xs)") {
    check{(x: Int, xs: IntList) => 
      val expected = x * product(xs)
      val was = product(Cons(x, xs))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("product(IntList(args*) debe ser equivalente a args.product") {
    check{(args: List[Int]) => 
      val expected = args.product
      val was = product(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("mkString(s)(IntList(args*)) debe ser equivalente a args.mkString(s)") {
    check{(args: List[Int], s: String) =>
      val expected = args.mkString(s)
      val was = mkString(s)(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test(
    "mkString(s,sep,e)(IntList(args*)) debe ser equivalente a args.mkString(s,sep,e)"
  ) {
    check{(args: List[Int], s: String, sep: String, e: String) =>
      val expected = args.mkString(s, sep, e)
      val was = mkString(s, sep, e)(IntList(args*))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("mkString(s)(IntList()) debe ser equivalente a List().mkString(s)") {
    check{(s: String) => 
      val expected = List().mkString(s)
      val was = mkString(s)(IntList())
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test(
    "mkString(s,sep,e)(IntList()) debe ser equivalente a List().mkString(s,sep,e)"
  ) {
    check{(s: String, sep: String, e: String) =>
      val expected = List().mkString(s, sep, e)
      val was = mkString(s, sep, e)(IntList())
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
end TestIntList
