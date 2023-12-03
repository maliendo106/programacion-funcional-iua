package practico

import org.scalatest.funsuite.AnyFunSuite
import scala.language.adhocExtensions

class TestPractico11 extends AnyFunSuite:
  test("Este práctico debe ser el 11") {
    assert("11" === Practico11.practico)
  }
