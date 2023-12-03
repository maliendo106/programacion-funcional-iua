package practico

import org.scalatest.funsuite.AnyFunSuite
import scala.language.adhocExtensions

class TestPractico9 extends AnyFunSuite:
  test("Este práctico debe ser el 9") {
    assert("9" === Practico9.practico)
  }
end TestPractico9

