package practico

import org.scalatest.funsuite.AnyFunSuite
import scala.language.adhocExtensions
import Practico10.*

class TestPractico10 extends AnyFunSuite:
  test("Este práctico debe ser el 10") {
    assert("10" === Practico10.practico)
  }

