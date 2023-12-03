package pouring
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.propBoolean
import scala.language.adhocExtensions
import scala.util.Random

def gcd(a: Int, b: Int): Int =
  if b == 0 then a else gcd(b, a % b)

def coprimes(ints: Seq[Int]): Boolean =
  require(ints.length > 1)
  ints.reduce(gcd) == 1

def distinct(ints: Seq[Int]): Boolean =
  ints.distinct == ints

def genPouring: Gen[Pouring] =
  for
    size <- Gen.choose(2, 4)
    ints <- Gen.containerOfN[List, Int](size, Gen.choose(2, 17)) `suchThat` (
      ints => coprimes(ints) && distinct(ints)
    )
    args = ints.sorted
  yield Pouring(args*)

given Arbitrary[Pouring] = Arbitrary(genPouring)

def randomState(capacities: Vector[Int]): Vector[Int] =
  capacities.map(capacity => Random.between(0, capacity + 1))

class TestPouring extends AnyFunSuite, Checkers:
  test("initialState debería tener el tamaño y valores adecuados") {
    check { (pouring: Pouring) =>
      (pouring.initialState.length == pouring.capacities.length) :| "initialState.length != capacities.length" &&
      pouring.initialState.forall(_ == 0) :| "initialState no está vacío"
    }
  }
  test(
    "change con Fill debería llenar el vaso correspondiente en el estado vacío"
  ) {
    check { (pouring: Pouring) =>
      import pouring.*
      capacities.zipWithIndex.forall((capacity, glass) =>
        val newState = change(initialState, Fill(glass))
        newState.sum == capacity && newState(glass) == capacity
      )
    }
  }
  test("change con Empty no debería cambiar el estado vacío") {
    check { (pouring: Pouring) =>
      import pouring.*
      (0 until capacities.length).forall(glass =>
        val newState = change(initialState, Empty(glass))
        newState == initialState
      )
    }
  }
  test("change con Fill no debería cambiar el estado lleno") {
    check { (pouring: Pouring) =>
      import pouring.*
      (0 until capacities.length).forall(glass =>
        val newState = change(capacities, Fill(glass))
        newState == capacities
      )
    }
  }
  test("change con Pour no debería cambiar el estado vacío") {
    check { (pouring: Pouring) =>
      import pouring.*
      (for
        from <- 0 until capacities.length
        to <- 0 until capacities.length
        if from != to
        newState = change(initialState, Pour(from, to))
      yield newState == initialState).reduce(_ && _)
    }
  }
  test("change con Pour no debería cambiar el estado lleno") {
    check { (pouring: Pouring) =>
      import pouring.*
      (for
        from <- 0 until capacities.length
        to <- 0 until capacities.length
        if from != to
        newState = change(capacities, Pour(from, to))
      yield newState == capacities).reduce(_ && _)
    }
  }

  test(
    "change con Fill debería llenar el vaso correspondiente"
  ) {
    check { (pouring: Pouring) =>
      import pouring.*
      val state = randomState(pouring.capacities)
      (0 until capacities.length).forall(glass =>
        val newState = change(state, Fill(glass))
        newState == state.updated(glass, capacities(glass))
      )
    }
  }
  test(
    "change con Empty debería vaciar el vaso correspondiente"
  ) {
    check { (pouring: Pouring) =>
      import pouring.*
      val state = randomState(pouring.capacities)
      (0 until capacities.length).forall(glass =>
        val newState = change(state, Empty(glass))
        newState == state.updated(glass, 0)
      )
    }
  }
  test("change con Pour debería producir el cambio adecuado") {
    check { (pouring: Pouring) =>
      import pouring.*
      val state = randomState(pouring.capacities)
      (for
        from <- 0 until capacities.length
        to <- 0 until capacities.length
        if from != to
        newState = change(state, Pour(from, to))
      yield newState.sum == state.sum && (newState(to) == capacities(
        to
      ) || newState(from) == 0) && newState(to) + newState(from) == state(
        to
      ) + state(from)).reduce(_ && _)
    }
  }
  test("La solución encontrada debería ser un camino válido") {
    check { (pouring: Pouring) =>
      import pouring.*
      val max = capacities.max
      val results = for
        target <- 0 to max
        found <- solution(target)
      yield found.path.foldLeft(initialState)(change).contains(target)
      results.length == max + 1 && results.reduce(_ && _)
    }
  }
  test("No debería encontrar soluciones para valores inválidos") {
    check { (pouring: Pouring, target: Int) =>
      import pouring.*
      (capacities.length <= 3  && (target < 0 || target > capacities.max)) ==> (solution(target) == None)
    }
  }
  test("La lista de caminos no debería tener destinos repetidos"){
      check{ (pouring: Pouring) => {
          val multiplePaths = pouring.pathsFromStart
            .take(5)
            .flatten
            .groupBy(_.endState)
            .filter(_._2.size > 1)
            .headOption
          val message = multiplePaths match
            case Some((endState, paths)) => s"endState: $endState\n       paths: \n\t${paths.mkString("\n\t")}"
            case None => ""
          multiplePaths.isEmpty :| message 
      }        
    }
  }
