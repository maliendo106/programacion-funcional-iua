package knightmoves
// scalastyle:off magic.number
import scala.util.Random
import scala.math.Ordering.Implicits.seqOrdering
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.propBoolean
import scala.language.adhocExtensions

object TestData:
  val fromTests: List[(Pos, Int, LazyList[Set[List[Pos]]])] = List(
    (
      Pos(0, 0),
      3,
      LazyList(
        Set(List(Pos(0, 0))),
        Set(List(Pos(2, 1), Pos(0, 0)), List(Pos(1, 2), Pos(0, 0))),
        Set(
          List(Pos(1, 3), Pos(2, 1), Pos(0, 0)),
          List(Pos(0, 4), Pos(1, 2), Pos(0, 0)),
          List(Pos(3, 3), Pos(2, 1), Pos(0, 0)),
          List(Pos(4, 0), Pos(2, 1), Pos(0, 0)),
          List(Pos(2, 0), Pos(1, 2), Pos(0, 0)),
          List(Pos(2, 4), Pos(1, 2), Pos(0, 0)),
          List(Pos(0, 2), Pos(2, 1), Pos(0, 0)),
          List(Pos(4, 2), Pos(2, 1), Pos(0, 0)),
          List(Pos(3, 1), Pos(1, 2), Pos(0, 0)),
          List(Pos(3, 3), Pos(1, 2), Pos(0, 0))
        )
      )
    ),
    (
      Pos(0, 3),
      3,
      LazyList(
        Set(List(Pos(0, 3))),
        Set(
          List(Pos(2, 2), Pos(0, 3)),
          List(Pos(2, 4), Pos(0, 3)),
          List(Pos(1, 1), Pos(0, 3)),
          List(Pos(1, 5), Pos(0, 3))
        ),
        Set(
          List(Pos(3, 6), Pos(2, 4), Pos(0, 3)),
          List(Pos(3, 2), Pos(1, 1), Pos(0, 3)),
          List(Pos(4, 1), Pos(2, 2), Pos(0, 3)),
          List(Pos(2, 3), Pos(1, 5), Pos(0, 3)),
          List(Pos(0, 5), Pos(2, 4), Pos(0, 3)),
          List(Pos(4, 5), Pos(2, 4), Pos(0, 3)),
          List(Pos(4, 3), Pos(2, 4), Pos(0, 3)),
          List(Pos(3, 4), Pos(1, 5), Pos(0, 3)),
          List(Pos(3, 6), Pos(1, 5), Pos(0, 3)),
          List(Pos(1, 0), Pos(2, 2), Pos(0, 3)),
          List(Pos(4, 3), Pos(2, 2), Pos(0, 3)),
          List(Pos(1, 2), Pos(2, 4), Pos(0, 3)),
          List(Pos(3, 2), Pos(2, 4), Pos(0, 3)),
          List(Pos(1, 6), Pos(2, 4), Pos(0, 3)),
          List(Pos(0, 1), Pos(2, 2), Pos(0, 3)),
          List(Pos(2, 7), Pos(1, 5), Pos(0, 3)),
          List(Pos(1, 4), Pos(2, 2), Pos(0, 3)),
          List(Pos(2, 3), Pos(1, 1), Pos(0, 3)),
          List(Pos(0, 7), Pos(1, 5), Pos(0, 3)),
          List(Pos(3, 0), Pos(1, 1), Pos(0, 3)),
          List(Pos(3, 0), Pos(2, 2), Pos(0, 3)),
          List(Pos(3, 4), Pos(2, 2), Pos(0, 3))
        )
      )
    ),
    (
      Pos(0, 7),
      3,
      LazyList(
        Set(List(Pos(0, 7))),
        Set(List(Pos(2, 6), Pos(0, 7)), List(Pos(1, 5), Pos(0, 7))),
        Set(
          List(Pos(2, 3), Pos(1, 5), Pos(0, 7)),
          List(Pos(3, 4), Pos(2, 6), Pos(0, 7)),
          List(Pos(4, 5), Pos(2, 6), Pos(0, 7)),
          List(Pos(1, 4), Pos(2, 6), Pos(0, 7)),
          List(Pos(3, 6), Pos(1, 5), Pos(0, 7)),
          List(Pos(0, 3), Pos(1, 5), Pos(0, 7)),
          List(Pos(3, 4), Pos(1, 5), Pos(0, 7)),
          List(Pos(4, 7), Pos(2, 6), Pos(0, 7)),
          List(Pos(2, 7), Pos(1, 5), Pos(0, 7)),
          List(Pos(0, 5), Pos(2, 6), Pos(0, 7))
        )
      )
    ),
    (
      Pos(3, 0),
      3,
      LazyList(
        Set(List(Pos(3, 0))),
        Set(
          List(Pos(1, 1), Pos(3, 0)),
          List(Pos(2, 2), Pos(3, 0)),
          List(Pos(5, 1), Pos(3, 0)),
          List(Pos(4, 2), Pos(3, 0))
        ),
        Set(
          List(Pos(0, 3), Pos(2, 2), Pos(3, 0)),
          List(Pos(3, 4), Pos(2, 2), Pos(3, 0)),
          List(Pos(4, 1), Pos(2, 2), Pos(3, 0)),
          List(Pos(4, 3), Pos(2, 2), Pos(3, 0)),
          List(Pos(0, 3), Pos(1, 1), Pos(3, 0)),
          List(Pos(3, 2), Pos(5, 1), Pos(3, 0)),
          List(Pos(7, 0), Pos(5, 1), Pos(3, 0)),
          List(Pos(3, 4), Pos(4, 2), Pos(3, 0)),
          List(Pos(2, 3), Pos(1, 1), Pos(3, 0)),
          List(Pos(1, 4), Pos(2, 2), Pos(3, 0)),
          List(Pos(6, 3), Pos(5, 1), Pos(3, 0)),
          List(Pos(3, 2), Pos(1, 1), Pos(3, 0)),
          List(Pos(2, 1), Pos(4, 2), Pos(3, 0)),
          List(Pos(7, 2), Pos(5, 1), Pos(3, 0)),
          List(Pos(0, 1), Pos(2, 2), Pos(3, 0)),
          List(Pos(4, 3), Pos(5, 1), Pos(3, 0)),
          List(Pos(6, 3), Pos(4, 2), Pos(3, 0)),
          List(Pos(1, 0), Pos(2, 2), Pos(3, 0)),
          List(Pos(6, 1), Pos(4, 2), Pos(3, 0)),
          List(Pos(2, 3), Pos(4, 2), Pos(3, 0)),
          List(Pos(5, 0), Pos(4, 2), Pos(3, 0)),
          List(Pos(5, 4), Pos(4, 2), Pos(3, 0))
        )
      )
    ),
    (
      Pos(3, 3),
      3,
      LazyList(
        Set(List(Pos(3, 3))),
        Set(
          List(Pos(2, 5), Pos(3, 3)),
          List(Pos(5, 2), Pos(3, 3)),
          List(Pos(1, 2), Pos(3, 3)),
          List(Pos(2, 1), Pos(3, 3)),
          List(Pos(5, 4), Pos(3, 3)),
          List(Pos(4, 5), Pos(3, 3)),
          List(Pos(1, 4), Pos(3, 3)),
          List(Pos(4, 1), Pos(3, 3))
        ),
        Set(
          List(Pos(3, 1), Pos(1, 2), Pos(3, 3)),
          List(Pos(2, 4), Pos(1, 2), Pos(3, 3)),
          List(Pos(1, 3), Pos(2, 1), Pos(3, 3)),
          List(Pos(2, 2), Pos(4, 1), Pos(3, 3)),
          List(Pos(3, 5), Pos(1, 4), Pos(3, 3)),
          List(Pos(1, 7), Pos(2, 5), Pos(3, 3)),
          List(Pos(4, 4), Pos(2, 5), Pos(3, 3)),
          List(Pos(0, 6), Pos(2, 5), Pos(3, 3)),
          List(Pos(0, 0), Pos(1, 2), Pos(3, 3)),
          List(Pos(7, 5), Pos(5, 4), Pos(3, 3)),
          List(Pos(3, 7), Pos(2, 5), Pos(3, 3)),
          List(Pos(4, 0), Pos(5, 2), Pos(3, 3)),
          List(Pos(7, 1), Pos(5, 2), Pos(3, 3)),
          List(Pos(0, 6), Pos(1, 4), Pos(3, 3)),
          List(Pos(5, 3), Pos(4, 5), Pos(3, 3)),
          List(Pos(4, 6), Pos(5, 4), Pos(3, 3)),
          List(Pos(4, 6), Pos(2, 5), Pos(3, 3)),
          List(Pos(6, 4), Pos(5, 2), Pos(3, 3)),
          List(Pos(4, 4), Pos(5, 2), Pos(3, 3)),
          List(Pos(6, 0), Pos(4, 1), Pos(3, 3)),
          List(Pos(2, 6), Pos(4, 5), Pos(3, 3)),
          List(Pos(7, 3), Pos(5, 4), Pos(3, 3)),
          List(Pos(6, 6), Pos(5, 4), Pos(3, 3)),
          List(Pos(0, 2), Pos(1, 4), Pos(3, 3)),
          List(Pos(6, 6), Pos(4, 5), Pos(3, 3)),
          List(Pos(1, 3), Pos(2, 5), Pos(3, 3)),
          List(Pos(3, 5), Pos(5, 4), Pos(3, 3)),
          List(Pos(0, 0), Pos(2, 1), Pos(3, 3)),
          List(Pos(6, 4), Pos(4, 5), Pos(3, 3)),
          List(Pos(2, 0), Pos(1, 2), Pos(3, 3)),
          List(Pos(2, 0), Pos(4, 1), Pos(3, 3)),
          List(Pos(6, 2), Pos(5, 4), Pos(3, 3)),
          List(Pos(2, 6), Pos(1, 4), Pos(3, 3)),
          List(Pos(0, 4), Pos(2, 5), Pos(3, 3)),
          List(Pos(3, 1), Pos(5, 2), Pos(3, 3)),
          List(Pos(6, 0), Pos(5, 2), Pos(3, 3)),
          List(Pos(6, 2), Pos(4, 1), Pos(3, 3)),
          List(Pos(3, 7), Pos(4, 5), Pos(3, 3)),
          List(Pos(2, 2), Pos(1, 4), Pos(3, 3)),
          List(Pos(4, 2), Pos(5, 4), Pos(3, 3)),
          List(Pos(4, 2), Pos(2, 1), Pos(3, 3)),
          List(Pos(7, 3), Pos(5, 2), Pos(3, 3)),
          List(Pos(5, 7), Pos(4, 5), Pos(3, 3)),
          List(Pos(0, 4), Pos(1, 2), Pos(3, 3)),
          List(Pos(0, 2), Pos(2, 1), Pos(3, 3)),
          List(Pos(4, 0), Pos(2, 1), Pos(3, 3)),
          List(Pos(2, 4), Pos(4, 5), Pos(3, 3)),
          List(Pos(5, 3), Pos(4, 1), Pos(3, 3))
        )
      )
    ),
    (
      Pos(3, 7),
      3,
      LazyList(
        Set(List(Pos(3, 7))),
        Set(
          List(Pos(1, 6), Pos(3, 7)),
          List(Pos(2, 5), Pos(3, 7)),
          List(Pos(5, 6), Pos(3, 7)),
          List(Pos(4, 5), Pos(3, 7))
        ),
        Set(
          List(Pos(6, 4), Pos(5, 6), Pos(3, 7)),
          List(Pos(0, 6), Pos(2, 5), Pos(3, 7)),
          List(Pos(2, 6), Pos(4, 5), Pos(3, 7)),
          List(Pos(3, 5), Pos(1, 6), Pos(3, 7)),
          List(Pos(4, 4), Pos(2, 5), Pos(3, 7)),
          List(Pos(0, 4), Pos(2, 5), Pos(3, 7)),
          List(Pos(5, 3), Pos(4, 5), Pos(3, 7)),
          List(Pos(3, 3), Pos(2, 5), Pos(3, 7)),
          List(Pos(4, 6), Pos(2, 5), Pos(3, 7)),
          List(Pos(7, 5), Pos(5, 6), Pos(3, 7)),
          List(Pos(1, 3), Pos(2, 5), Pos(3, 7)),
          List(Pos(2, 4), Pos(1, 6), Pos(3, 7)),
          List(Pos(3, 5), Pos(5, 6), Pos(3, 7)),
          List(Pos(2, 4), Pos(4, 5), Pos(3, 7)),
          List(Pos(1, 7), Pos(2, 5), Pos(3, 7)),
          List(Pos(6, 4), Pos(4, 5), Pos(3, 7)),
          List(Pos(3, 3), Pos(4, 5), Pos(3, 7)),
          List(Pos(4, 4), Pos(5, 6), Pos(3, 7)),
          List(Pos(0, 4), Pos(1, 6), Pos(3, 7)),
          List(Pos(6, 6), Pos(4, 5), Pos(3, 7)),
          List(Pos(7, 7), Pos(5, 6), Pos(3, 7)),
          List(Pos(5, 7), Pos(4, 5), Pos(3, 7))
        )
      )
    ),
    (
      Pos(7, 0),
      3,
      LazyList(
        Set(List(Pos(7, 0))),
        Set(List(Pos(5, 1), Pos(7, 0)), List(Pos(6, 2), Pos(7, 0))),
        Set(
          List(Pos(6, 3), Pos(5, 1), Pos(7, 0)),
          List(Pos(3, 0), Pos(5, 1), Pos(7, 0)),
          List(Pos(7, 2), Pos(5, 1), Pos(7, 0)),
          List(Pos(4, 3), Pos(6, 2), Pos(7, 0)),
          List(Pos(4, 1), Pos(6, 2), Pos(7, 0)),
          List(Pos(3, 2), Pos(5, 1), Pos(7, 0)),
          List(Pos(5, 4), Pos(6, 2), Pos(7, 0)),
          List(Pos(4, 3), Pos(5, 1), Pos(7, 0)),
          List(Pos(5, 0), Pos(6, 2), Pos(7, 0)),
          List(Pos(7, 4), Pos(6, 2), Pos(7, 0))
        )
      )
    ),
    (
      Pos(7, 3),
      3,
      LazyList(
        Set(List(Pos(7, 3))),
        Set(
          List(Pos(5, 2), Pos(7, 3)),
          List(Pos(5, 4), Pos(7, 3)),
          List(Pos(6, 1), Pos(7, 3)),
          List(Pos(6, 5), Pos(7, 3))
        ),
        Set(
          List(Pos(7, 5), Pos(5, 4), Pos(7, 3)),
          List(Pos(5, 3), Pos(6, 1), Pos(7, 3)),
          List(Pos(4, 4), Pos(6, 5), Pos(7, 3)),
          List(Pos(4, 0), Pos(6, 1), Pos(7, 3)),
          List(Pos(6, 2), Pos(5, 4), Pos(7, 3)),
          List(Pos(7, 7), Pos(6, 5), Pos(7, 3)),
          List(Pos(3, 1), Pos(5, 2), Pos(7, 3)),
          List(Pos(3, 3), Pos(5, 2), Pos(7, 3)),
          List(Pos(5, 7), Pos(6, 5), Pos(7, 3)),
          List(Pos(4, 0), Pos(5, 2), Pos(7, 3)),
          List(Pos(5, 3), Pos(6, 5), Pos(7, 3)),
          List(Pos(3, 5), Pos(5, 4), Pos(7, 3)),
          List(Pos(4, 2), Pos(6, 1), Pos(7, 3)),
          List(Pos(3, 3), Pos(5, 4), Pos(7, 3)),
          List(Pos(6, 4), Pos(5, 2), Pos(7, 3)),
          List(Pos(7, 1), Pos(5, 2), Pos(7, 3)),
          List(Pos(6, 0), Pos(5, 2), Pos(7, 3)),
          List(Pos(6, 6), Pos(5, 4), Pos(7, 3)),
          List(Pos(4, 4), Pos(5, 2), Pos(7, 3)),
          List(Pos(4, 6), Pos(5, 4), Pos(7, 3)),
          List(Pos(4, 2), Pos(5, 4), Pos(7, 3)),
          List(Pos(4, 6), Pos(6, 5), Pos(7, 3))
        )
      )
    ),
    (
      Pos(7, 7),
      3,
      LazyList(
        Set(List(Pos(7, 7))),
        Set(List(Pos(5, 6), Pos(7, 7)), List(Pos(6, 5), Pos(7, 7))),
        Set(
          List(Pos(4, 4), Pos(5, 6), Pos(7, 7)),
          List(Pos(7, 5), Pos(5, 6), Pos(7, 7)),
          List(Pos(3, 7), Pos(5, 6), Pos(7, 7)),
          List(Pos(7, 3), Pos(6, 5), Pos(7, 7)),
          List(Pos(5, 3), Pos(6, 5), Pos(7, 7)),
          List(Pos(5, 7), Pos(6, 5), Pos(7, 7)),
          List(Pos(3, 5), Pos(5, 6), Pos(7, 7)),
          List(Pos(4, 6), Pos(6, 5), Pos(7, 7)),
          List(Pos(6, 4), Pos(5, 6), Pos(7, 7)),
          List(Pos(4, 4), Pos(6, 5), Pos(7, 7))
        )
      )
    )
  )
end TestData

class TestGoalGame extends AnyFunSuite with Checkers:
  test("GoalGame.done()") {
    check { (pos: Pos) =>
      val board = new GoalGame with ChessBoard with NoPieces:
        val startPos = Pos(0, 0)
        val goal: Pos = pos
      val expected = true
      val was = board.done(pos)
      (expected == was) :| s"expected: done($pos) = $expected\n\t  was: $was\n    on input: goal = ${board.goal}"
    }
 }
  test("!GoalGame.done()") {
    check { (pos: Pos) =>
      val board = new GoalGame with ChessBoard with NoPieces:
        val startPos = Pos(0, 0)
        val goal: Pos = startPos
      pos != board.goal ==> {
        val expected = false
        val was = board.done(pos)
        (expected == was) :| s"expected: done($pos) = $expected\n\t  was: $was\n    on input: goal = ${board.goal}"
      }
    }
 }
  test("from desde distintas posiciones en tablero vacío") {
    val pos = Pos(0, 0)
    val board = new GoalGame with ChessBoard with NoPieces:
      val startPos: Pos = pos
      val goal: Pos = pos
    for (p, len, expected) <- TestData.fromTests do
      val found = board.from(Set(List(p)), Set(p))
      for (f, e) <- found.zip(expected) do
        val fheads = f.map(_.head).toList.sorted
        val eheads = e.map(_.head).toList.sorted
        assert(fheads == eheads, s"\n\nexpected: ${eheads.mkString("\n")}\n\nwas: ${fheads.mkString("\n")}\n\ninput: from(Set(List($p)), Set($p))")
    end for 
  }
  test("pathsToGoal de longitud cero en tablero vacío") {
    check { (pos: Pos) =>
      val board = new GoalGame with ChessBoard with NoPieces:
        val startPos: Pos = pos
        val goal: Pos = pos
      val expected = LazyList(List(pos))
      val was = board.pathsToGoal.take(3)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: startPos = ${board.startPos}, goal = ${board.goal}"
    }
  }
  test("pathsToGoal de longitud uno en tablero vacío") {
    def validMove(pos: Pos): Pos =
      Random.shuffle(moves.map(move => move(pos)).filter(EmptyChessBoard.isLegal)).head

    check { (pos: Pos) =>
      val board = new GoalGame with ChessBoard with NoPieces:
        val startPos: Pos = pos
        val goal: Pos = validMove(pos)
      val expected = LazyList(List(board.goal, pos))
      val paths = board.pathsToGoal.take(4)
      (expected == paths) :| s"expected: $expected\n\t  was: $paths\n    on input: startPos = ${board.startPos}, goal = ${board.goal}"
    }
  }
  test("solution de longitud cero en tablero vacío") {
    check { (pos: Pos) =>
      val board = new GoalGame with ChessBoard with NoPieces:
        val startPos: Pos = pos
        val goal: Pos = pos
      val expected = Some(List(pos))
      val was = board.solution
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: startPos = ${board.startPos}, goal = ${board.goal}"
    }
  }
  test("solution de longitud uno en tablero vacío") {
    def validMove(pos: Pos): Pos =
      Random.shuffle(moves.map(move => move(pos)).filter(EmptyChessBoard.isLegal)).head
    check { (pos: Pos) =>
      val board = new GoalGame with ChessBoard with NoPieces:
        val startPos: Pos = pos
        val goal: Pos = validMove(pos)
      val expected = Some(List(pos, board.goal))
      val was = board.solution
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: startPos = ${board.startPos}, goal = ${board.goal}"
    }
  }
  test("no solution") {
    check { (pos: Pos, target: Pos) =>
      val board = new GoalGame with ChessBoard:
        val startPos = pos
        val goal = target
        val pieces = EmptyChessBoard.legalNeighbors(pos).toSet
      (target != pos && !board.pieces(target)) ==> {
        val expected = None
        val was = board.solution
        (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: startPos = ${board.startPos}, goal = ${board.goal}, pieces = ${board.pieces}"
      }
    }
  }
  test("no debería generar caminos distintos a una misma posición") {
    val board = new GoalGame with InfiniteBoard with NoPieces:
      val startPos = Pos(0, 0)
      val goal = startPos
    val multiplePaths = board.pathsFromStart
      .take(5)
      .flatten
      .groupBy(_.head)
      .filter(_._2.size > 1)
      .headOption
    val message = multiplePaths match
      case Some((pos, paths)) =>
        s"\nmultiple paths from ${board.startPos} to $pos: $paths"
      case None => ""
    assert(
      multiplePaths.isEmpty, message
    )
  }
  test("debería encontrar caminos válidos") {
    check { (start: Pos, target: Pos, somePieces: Set[Pos]) =>
      val game = new GoalGame with ChessBoard:
        val pieces = somePieces - start - target
        val startPos = start
        val goal = target
      (!game.solution.isEmpty) ==> {
        game.solution.get match
          case solution @ (_ :: _ :: _) =>
            solution.forall(pos =>
              inChessBoard(pos) && !game.pieces(pos)
            ) && solution
              .zip(solution.tail)
              .forall((a, b) => game.legalNeighbors(a) contains b)
          case _ => true
      }
    }
  }
end TestGoalGame
