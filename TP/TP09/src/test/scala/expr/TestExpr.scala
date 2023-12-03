package expr

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import org.scalacheck.Prop.{BooleanOperators, propBoolean}
import scala.language.adhocExtensions
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import Gen.*
import Arbitrary.arbitrary
import rational.*
import BinaryOperator.*
import UnaryOperator.*
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import scala.language.implicitConversions
import scala.language.experimental

def genRational = for
  n <- Gen.chooseNum(-10, 10)
  d <- Gen.chooseNum(1, 10)
yield Rational(n, d)

def genNum = for q <- arbitrary[Rational] yield Num(q)
def genOp = oneOf(Plus, Minus, Times, Div, Pow)
def genUnOp = oneOf(Inv, Neg, Abs)

given Arbitrary[Rational] = Arbitrary(genRational)
given Arbitrary[BinaryOperator] = Arbitrary(genOp)
given Arbitrary[UnaryOperator] = Arbitrary(genUnOp)
given Arbitrary[Num] = Arbitrary(genNum)

case class TestableExpr(expr: Expr, value: Rational, repr: String, rpn: String)

val opSym =
  Map(Plus -> "+", Minus -> "-", Times -> "*", Div -> "÷", Pow -> "pow")

def testableBinOp(op: BinaryOperator, left: TestableExpr, right: TestableExpr) =
  def format(e: TestableExpr): String = e.expr match
    case BinOp(Plus, _, _) | BinOp(Minus, _, _) => s"(${e.repr})"
    case _                                      => e.repr
  def divformat(e: TestableExpr): String = e.expr match
    case BinOp(Plus | Minus | Times | Div, _, _) => s"(${e.repr})"
    case _                                       => e.repr
  val value = op match
    case Plus  => left.value + right.value
    case Minus => left.value - right.value
    case Times => left.value * right.value
    case Div   => left.value / right.value
    case Pow   => left.value.pow(right.value)
  val repr = op match
    case Plus  => s"${left.repr}+${right.repr}"
    case Minus => s"${left.repr}-${format(right)}"
    case Times => s"${format(left)}*${format(right)}"
    case Div   => s"${format(left)}÷${divformat(right)}"
    case Pow   => s"pow(${left.repr},${right.repr})"
  TestableExpr(
    BinOp(op, left.expr, right.expr),
    value,
    repr,
    s"${left.rpn} ${right.rpn} ${opSym(op)}"
  )

def unOp(op: UnaryOperator, q: Rational) = op match
  case Inv => q.inverse
  case Neg => -q
  case Abs => q.abs

def validPowArguments(q: Rational, e: Rational) =
  val limit = BigInt(1) << 31
  def overflow(n: Int) = BigInt(n).abs.pow(e.numer.abs) >= limit
  e.denom == 1 && !overflow(q.numer) && !overflow(
    q.denom
  ) && !(q.numer == 0 && e.numer < 0)

val genTestableNum =
  for q <- arbitrary[Rational]
  yield TestableExpr(Num(q), q, show(q), q.toString)

val genTestableUnOp = for
  op <- arbitrary[UnaryOperator]
  e <- genTestableExpr
  if !(op == Inv && e.value == Zero)
yield TestableExpr(
  UnOp(op, e.expr),
  unOp(op, e.value),
  s"${op.toString.toLowerCase}(${e.repr})",
  s"${e.rpn} ${op.toString.toLowerCase}"
)

val genTestableBinOp = for
  op <- arbitrary[BinaryOperator]
  left <- genTestableExpr
  right <- genTestableExpr
  if left.value.abs < MaxValue
  if right.value.abs < MaxValue
  if !(op == Div && right.value == Zero)
  if !(op == Pow && !validPowArguments(left.value, right.value))
yield testableBinOp(op, left, right)

def genTestableExpr: Gen[TestableExpr] =
  oneOf(genTestableNum, lzy(genTestableBinOp), lzy(genTestableUnOp))

given Arbitrary[TestableExpr] = Arbitrary(genTestableExpr)

val Zero = Rational(0)
val One = Rational(1)
val NumZero = Num(Zero)
val MaxValue = Rational(16535)

def show(q: Rational) = if q < Zero then s"($q)" else q.toString

def asString(q: Rational) = q match
  case Rational(n,1) => s"Rational($n)"
  case Rational(n,d) => s"Rational($n,$d)"

def asString(expr: Expr): String = expr match
  case Num(q) => s"Num(${asString(q)})"
  case BinOp(op,left,right) => s"BinOp($op,${asString(left)},${asString(right)})"
  case UnOp(op,expr) => s"UnOp($op,${asString(expr)})"

given Conversion[Rational, Expr] = Num(_)

def binop(op: BinaryOperator)(a: Expr, b: Expr) = BinOp(op, a, b)
val plus = binop(Plus)
val minus = binop(Minus)
val times = binop(Times)
val div = binop(Div)
val pow = binop(Pow)

def resultIsError(result: Either[String, Expr], message: String) =
  result match
    case Left(m) => m.toLowerCase.contains(message.toLowerCase)
    case _       => false

class TestExpr extends AnyFunSuite with Checkers:
  test("Debe mostrar correctamente expresiones Num") {
    check { (q: Rational) =>
      val expected = show(q)
      val was = Num(q).show
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente expresiones Num enteras") {
    check { (i: Int) =>
      val q = Rational(i)
      val expected = show(q)
      val was = Num(q).show
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe evaluar correctamente expresiones Num") {
    check { (q: Rational) =>
      val expected = q
      val was = Num(q).eval
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente sumas de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = s"${show(q)}+${show(r)}"
      val was = BinOp(Plus, Num(q), Num(r)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente restas de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = s"${show(q)}-${show(r)}"
      val was = BinOp(Minus, Num(q), Num(r)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente productos de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = s"${show(q)}*${show(r)}"
      val was = BinOp(Times, Num(q), Num(r)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente potencias de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = s"pow(${show(q)},${show(r)})"
      val was = BinOp(Pow, Num(q), Num(r)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente cocientes de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = s"${show(q)}÷${show(r)}"
      val was = BinOp(Div, Num(q), Num(r)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente productos de dos sumas") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"(${show(q)}+${show(r)})*(${show(s)}+${show(t)})"
      val was = times(plus(q, r), plus(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente productos de dos productos") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}*${show(r)}*${show(s)}*${show(t)}"
      val was = times(times(q, r), times(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente productos de dos cocientes") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}÷${show(r)}*${show(s)}÷${show(t)}"
      val was = times(div(q, r), div(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente productos de un producto y un cociente") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}*${show(r)}*${show(s)}÷${show(t)}"
      val was = times(times(q, r), div(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
     }
  }

  test("Debe mostrar correctamente productos de un cociente y un producto") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}÷${show(r)}*${show(s)}*${show(t)}"
      val was = times(div(q, r), times(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente productos de dos potencias") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"pow(${show(q)},${show(r)})*pow(${show(s)},${show(t)})"
      val was = times(pow(q, r), pow(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente exponenciación de dos productos") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"pow(${show(q)}*${show(r)},${show(s)}*${show(t)})"
      val was = pow(times(q, r), times(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente cocientes de dos sumas") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"(${show(q)}+${show(r)})÷(${show(s)}+${show(t)})"
      val was = div(plus(q, r), plus(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente restas de dos sumas") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}+${show(r)}-(${show(s)}+${show(t)})"
      val was = minus(plus(q, r), plus(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"    }
  }

  test("Debe mostrar correctamente la resta de una suma y una resta") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}+${show(r)}-(${show(s)}-${show(t)})"
      val was = minus(plus(q, r), minus(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente la resta de una suma y una multiplicación") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}+${show(r)}-${show(s)}*${show(t)}"
      val was = minus(plus(q, r), times(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente la resta de una suma y una división") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}+${show(r)}-${show(s)}÷${show(t)}"
      val was = minus(plus(q, r), div(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente la resta de una suma y una exponenciación") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}+${show(r)}-pow(${show(s)},${show(t)})"
      val was = minus(plus(q, r), pow(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente cocientes de dos productos") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}*${show(r)}÷(${show(s)}*${show(t)})"
      val was = div(times(q, r), times(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente cocientes de un producto y un cociente") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}*${show(r)}÷(${show(s)}÷${show(t)})"
      val was = div(times(q, r), div(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "Debe mostrar correctamente cocientes de un producto y una exponenciación"
  ) {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"${show(q)}*${show(r)}÷pow(${show(s)},${show(t)})"
      val was = div(times(q, r), pow(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Debe mostrar correctamente cocientes de dos potencias") {
    check { (q: Rational, r: Rational, s: Rational, t: Rational) =>
      val expected = s"pow(${show(q)},${show(r)})÷pow(${show(s)},${show(t)})"
      val was = div(pow(q, r), pow(s, t)).show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("eval debe evaluar correctamente sumas de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = q + r
      val was = BinOp(Plus, Num(q), Num(r)).eval
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("eval debe evaluar correctamente restas de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = q - r
      val was = BinOp(Minus, Num(q), Num(r)).eval
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("eval debe evaluar correctamente productos de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = q * r
      val was = BinOp(Times, Num(q), Num(r)).eval
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "eval debe evaluar correctamente cocientes de dos Num con divisor distinto de cero"
  ) {
    check { (q: Rational, r: Rational) =>
      (r != Zero) ==> {
        val expected = q / r
        val was = BinOp(Div, Num(q), Num(r)).eval
        (expected == was) :| s"expected: $expected\n\t  was: $was"
      }
    }
  }

  test(
    "eval debe lanzar IllegalArgumentException al evaluar un Div con divisor Num(Zero)"
  ) {
    assertThrows[IllegalArgumentException] {
      BinOp(Div, Num(One), Num(Zero)).eval
    }
  }

  test(
    "eval debe evaluar correctamente potencias de un Num con exponente entero"
  ) {
    check { (n: Byte, d: Byte, b: Byte) =>
      val e = b.abs % 4 + 1
      val q = Rational(n, d.abs % 32 + 1)
      val expected = q.pow(e)
      val was = BinOp(Pow, Num(q), Num(Rational(e))).eval
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "eval debe lanzar IllegalArgumentException al elevar un Num a una potencia no entera"
  ) {
    for n <- (-10 to -1) ++ (1 to 10); d <- 1 to 10; e <- 2 to 6 do
      assertThrows[IllegalArgumentException](
        BinOp(Pow, Num(Rational(n, d)), Num(Rational(1, e))).eval
      )
  }

  test("evalOption debe evaluar correctamente sumas de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = Some(q+r)
      val was = BinOp(Plus, Num(q), Num(r)).evalOption
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("evalOption debe evaluar correctamente restas de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = Some(q-r)
      val was = BinOp(Minus, Num(q), Num(r)).evalOption
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("evalOption debe evaluar correctamente productos de dos Num") {
    check { (q: Rational, r: Rational) =>
      val expected = Some(q*r)
      val was = BinOp(Times, Num(q), Num(r)).evalOption
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "evalOption debe evaluar correctamente cocientes de dos Num con divisor distinto de cero"
  ) {
    check { (q: Rational, r: Rational) =>
      (r != Zero) ==> {
        val expected = Some(q/r)
        val was = BinOp(Div, Num(q), Num(r)).evalOption
        (expected == was) :| s"expected: $expected\n\t  was: $was"
      }
    }
  }
  
  test(
    "evalOption debe devolver None al evaluar un Div con divisor Num(Zero)"
  ) {
    assert(BinOp(Div, Num(One), NumZero).evalOption == None)
  }

  test("Debe mostrar expresiones correctamente") {
    check { (e: TestableExpr) =>
      val expected = e.repr
      val was = e.expr.show.replace(" ", "")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("toString debe mostrar expresiones correctamente") {
    check { (e: TestableExpr) =>
      val expected = asString(e.expr)
      val was = e.expr.toString.replace(" ", "")
      (expected == was) :| s"expected: $expected, was $was"
    }
  }

  test("eval debe evaluar expresiones correctamente") {
    check { (e: TestableExpr) =>
      val expected = e.value
      val was = e.expr.eval
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("evalOption debe evaluar expresiones correctamente") {
    check { (e: TestableExpr) =>
      val expected = Some(e.value)
      val was = e.expr.evalOption
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "evalOption debe evaluar expresiones que dividen por cero correctamente"
  ) {
    check { (e1: TestableExpr, e2: TestableExpr, op: BinaryOperator) =>
      val was1 = BinOp(Div, e1.expr, NumZero).evalOption
      val was2 = BinOp(op, e1.expr, BinOp(Div, e2.expr, NumZero)).evalOption
      val was3 = BinOp(op, BinOp(Div, e1.expr, NumZero), e2.expr).evalOption
      val expected = (None, None, None)
      val was = (was1, was2, was3)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Expr.parseRPN(String) debe generar expresiones válidas") {
    check { (e: TestableExpr) =>
      val expected = Right(e.expr)
      val was = Expr.parseRPN(e.rpn)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "Expr.parseRPN(String) debe generar expresiones válidas en cadenas que comiencen o terminen con espacios en blanco"
  ) {
    check { (e: TestableExpr) =>
      val expected = Right(e.expr)
      val was = Expr.parseRPN(s" ${e.rpn} ")
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "Expr.parseRPN(String) debe generar expresiones válidas con secuencias de tokens separadas por más de un espacio"
  ) {
    check { (e: TestableExpr) =>
      val expected = Right(e.expr)
      val was = Expr.parseRPN(e.rpn.replace(" ", "  "))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "Expr.parseRPN(String) debe generar expresiones válidas con secuencias de tokens separadas por otros tipos de espacios en blanco"
  ) {
    check { (e: TestableExpr) =>
      val expected = Right(e.expr)
      val was = Expr.parseRPN(e.rpn.replace(" ", " \t\t"))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test(
    "Expr.parseRPN(List[String]) debe generar expresiones válidas en la presencia de string vacías"
  ) {
    check { (e: TestableExpr) =>
      val expected = Right(e.expr)
      val was = Expr.parseRPN("" :: e.rpn.split(" ").toList)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("Expr.parseRPN(String) debe rechazar expresiones con operandos de más") {
    check { (e: TestableExpr, q: Rational) =>
      val error = "faltan operadores"
      val result = Expr.parseRPN(s"${e.rpn} $q")
      resultIsError(result, error) :| s"""expected: Left("$error")\n\t  was: $result"""
    }
  }

  test(
    "Expr.parseRPN(List[String]) debe rechazar expresiones con operandos de más"
  ) {
    check { (e: TestableExpr, q: Rational) =>
      val error = "faltan operadores"
      val result = Expr.parseRPN(s"${e.rpn} $q".split(" ").toList)
      resultIsError(result, error) :| s"""expected: Left("$error")\n\t  was: $result"""
    }
  }

  test(
    "Expr.parseRPN(String) debe rechazar expresiones con operadores binarios de más"
  ) {
    check { (e: TestableExpr, op: BinaryOperator) =>
      val error = "argumentos insuficientes"
      val result = Expr.parseRPN(s"${e.rpn} ${opSym(op)}")
      resultIsError(result, error) :| s"""expected: Left("$error")\n\t  was: $result"""
    }
  }

  test(
    "Expr.parseRPN(List[String]) debe rechazar expresiones con operadores binarios de más"
  ) {
    check { (e: TestableExpr, op: BinaryOperator) =>
      val error = "argumentos insuficientes"
      val result = Expr.parseRPN(s"${e.rpn} ${opSym(op)}".split(" ").toList)
      resultIsError(result, error) :| s"""expected: Left("$error")\n\t  was: $result"""
    }
  }

  test(
    "Expr.parseRPN(String) debe rechazar expresiones con operadores unarios de más"
  ) {
    check { (e: TestableExpr, op: UnaryOperator) =>
      val error = "falta argumento"
      val result =
        Expr.parseRPN(s"${op.toString.toLowerCase} ${e.rpn}")
      resultIsError(result, error) :| s"""expected: Left("$error")\n\t  was: $result"""
    }
  }

  test(
    "Expr.parseRPN(List[String]) debe rechazar expresiones con operadores unarios de más"
  ) {
    check { (e: TestableExpr, op: UnaryOperator) =>
      val error = "falta argumento"
      val result =
        Expr.parseRPN(s"${op.toString.toLowerCase} ${e.rpn}".split(" ").toList)
      resultIsError(result, error) :| s"""expected: Left("$error")\n\t  was: $result"""
    }
  }

  test(
    "Expr.parseRPN(String) debe rechazar expresiones con elementos inválidos"
  ) {
    for
      rpn <- List(
        "1 2 `",
        "x 4",
        "1.5 2 +",
        "plus 3 4",
        "1e2 2 /",
        "1/ abs",
        "/1 inv",
        "1 2 **"
      )
    do
      val result = Expr.parseRPN(rpn)
      assert(resultIsError(result, "elemento desconocido"), s"with input '$rpn'")
  }

  test(
    "Expr.parseRPN(List[String]) debe rechazar expresiones con elementos inválidos"
  ) {
    for
      rpn <- List(
        "1 2 `",
        "x 4",
        "1.5 2 +",
        "plus 3 4",
        "1e2 2 /",
        "1/ abs",
        "/1 inv",
        "1 2 **"
      )
    do
      val input = rpn.split(" ").toList
      val result = Expr.parseRPN(input)
      assert(resultIsError(result, "elemento desconocido"), s"with input $input")
  }

  test("Expr.parseRPN(String) debe rechazar expresiones vacías") {
    for rpn <- List("", "  ", "\t", "\t \t") do
      assert(resultIsError(Expr.parseRPN(rpn), "nada que evaluar"), s"with input '$rpn'")
  }
  
  test("Expr.parseRPN(List[String]) debe rechazar expresiones vacías") {
    for rpn <- List(List(), List(""), List("", "", "")) do
      assert(resultIsError(Expr.parseRPN(rpn), "nada que evaluar"), "with input " + rpn.map(x => s"\"$x\"").toString)
  }

