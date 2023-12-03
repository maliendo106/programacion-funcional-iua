package knightmoves

// scalastyle:off magic.number

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.language.adhocExtensions
import Move.*

val moves: List[Move] = List(
  UpUpLeft,
  UpUpRight,
  UpLeftLeft,
  UpRightRight,
  DownDownLeft,
  DownDownRight,
  DownLeftLeft,
  DownRightRight
)

def inChessBoard(pos: Pos): Boolean =
  pos.x >= 0 && pos.y >= 0 && pos.x < 8 && pos.y < 8

def posGen: Gen[Pos] = for
  x <- Gen.choose(0, 7)
  y <- Gen.choose(0, 7)
yield Pos(x, y)

given Arbitrary[Pos] = Arbitrary(posGen)

given Ordering[Pos] = Ordering.by(pos => (pos.x, pos.y))

trait AllPieces extends BaseGame:
  val pieces: Pos => Boolean = (_: Pos) => true

trait ChessBoard extends Board:
  val size: Int = 8

object InfiniteEmptyBoard extends InfiniteBoard, NoPieces

object InfiniteFullBoard extends InfiniteBoard, AllPieces

object EmptyChessBoard extends ChessBoard, NoPieces

object FullChessBoard extends ChessBoard, AllPieces

object HalfFullChessBoard extends ChessBoard:
  val pieces: Pos => Boolean = { case Pos(x, y) =>
    (x + y) % 2 == 0
  }
end HalfFullChessBoard

object NullBoard extends BaseGame, NoPieces:
  val board: Pos => Boolean = (_: Pos) => false

class TestBaseGame extends AnyFunSuite, Checkers:

  test("Pos.move(0,0)") {
    check { (x: Short, y: Short) => 
      val expected = Pos(x, y)
      val was = Pos(x, y).move(0, 0)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("Pos.move(-x,-y)") {
    check { (x: Short, y: Short) => Pos(x, y).move(-x, -y) == Pos(0, 0) 
      val expected = Pos(0, 0)
      val was = Pos(x, y).move(-x, -y)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("Pos.move(dx,dy)") {
    check { (x: Short, y: Short, dx: Short, dy: Short) =>
      val expected = Pos(x + dx, y + dy)
      val was = Pos(x, y).move(dx, dy)
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("UpUpLeft") {
    check((x: Short, y: Short) => 
      val expected = Pos(x - 2, y - 1)
      val was = UpUpLeft(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    )
  }
  test("UpUpRight") {
    check{(x: Short, y: Short) => 
      val expected = Pos(x - 2, y + 1)
      val was = UpUpRight(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("UpLeftLeft") {
    check{(x: Short, y: Short) => 
      val expected = Pos(x - 1, y - 2)
      val was = UpLeftLeft(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("UpRightRight") {
    check{(x: Short, y: Short) => 
      val expected = Pos(x - 1, y + 2)
      val was = UpRightRight(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("DownDownLeft") {
    check{(x: Short, y: Short) => 
      val expected = Pos(x + 2, y - 1)
      val was = DownDownLeft(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("DownDownRight") {
    check{(x: Short, y: Short) => 
      val expected = Pos(x + 2, y + 1)
      val was = DownDownRight(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("DownLeftLeft") {
    check{(x: Short, y: Short) => 
      val expected = Pos(x + 1, y - 2)
      val was = DownLeftLeft(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("DownRightRight") {
    check{(x: Short, y: Short) =>
      val expected = Pos(x + 1, y + 2)
      val was = DownRightRight(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }

  test("UpUpLeft + DownDownRight") {
    check{(x: Short, y: Short) =>
      val expected = Pos(x, y)
      val was = DownDownRight(UpUpLeft(Pos(x, y)))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("UpUpRight + DownDownLeft") {
    check{(x: Short, y: Short) =>
      val expected = Pos(x, y)
      val was = DownDownLeft(UpUpRight(Pos(x, y)))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("UpLeftLeft + DownRightRight") {
    check{(x: Short, y: Short) =>
      val expected = Pos(x, y)
      val was = DownRightRight(UpLeftLeft(Pos(x, y)))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    }
  }
  test("UpRightRight + DownLeftLeft") {
    check((x: Short, y: Short) =>
      val expected = Pos(x, y)
      val was = DownLeftLeft(UpRightRight(Pos(x, y)))
      (expected == was) :| s"expected: $expected\n\t  was: $was"
    )
  }
  test("Infinite Empty Board") {
    check((x: Short, y: Short) => 
      val expected = true
      val was = InfiniteEmptyBoard.isLegal(Pos(x, y)) 
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: InfiniteEmptyBoard.isLegal(Pos($x, $y))"
    )
  }
  test("Infinite Full Board") {
    check((x: Short, y: Short) => 
      val expected = false
      val was = InfiniteFullBoard.isLegal(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: InfiniteFullBoard.isLegal(Pos($x, $y))"
    )
  }
  test("Empty Chess Board (en el tablero)") {
    check((pos: Pos) =>
      val expected = true
      val was = EmptyChessBoard.isLegal(pos)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: EmptyChessBoard.isLegal($pos)"
    )
  }
  test("Empty Chess Board") {
    check((x: Short, y: Short) =>
      val expected = inChessBoard(Pos(x, y))
      val was = EmptyChessBoard.isLegal(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: EmptyChessBoard.isLegal(Pos($x, $y))"
    )
  }
  test("Full Chess Board") {
    check((x: Short, y: Short) => 
      val expected = false
      val was = FullChessBoard.isLegal(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: FullChessBoard.isLegal(Pos($x, $y))"
    )
  }
  test("Full Chess Board (en el tablero)") {
    check((pos: Pos) => 
      val expected = false
      val was = FullChessBoard.isLegal(pos)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: FullChessBoard.isLegal($pos)"
    )
  }
  test("Half Full Chess Board (en el tablero)") {
    check((pos: Pos) =>
      val expected = (pos.x + pos.y) % 2 == 1
      val was = HalfFullChessBoard.isLegal(pos)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: HalfFullChessBoard.isLegal($pos)"
    )
  }

  test("Algunas piezas en ChessBoard") {
    check { (somePos: List[Pos], somePieces: Set[Pos]) =>
      val board: ChessBoard = new ChessBoard:
        val pieces: Set[Pos] = somePieces
      val expected = somePos.map(!somePieces(_))
      val was = somePos.map(board.isLegal)
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: positions = $somePos pieces = $somePieces"
    }
  }
  test("Null Board") {
    check((x: Short, y: Short) => 
      val expected = false
      val was = NullBoard.isLegal(Pos(x, y))
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: NullBoard.isLegal(Pos($x, $y))"
    )
  }

  test("Neighbors") {
    check { (x: Byte, y: Byte) =>
      val pos = Pos(x, y)
      val expected = moves.map(move => move(pos)).sorted
      val was = InfiniteEmptyBoard.neighbors(pos).sorted
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: InfiniteEmptyBoard.neighbors($pos)"
    }
  }
  test("Legal Neighbors (tablero vacío)") {
    val board = EmptyChessBoard
    check { (pos: Pos) =>
      val expected = moves.map(move => move(pos)).filter(inChessBoard).sorted
      val was = board.legalNeighbors(pos).sorted
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: EmptyChessBoard.legalNeighbors($pos)"
    }

  }
  test("Legal Neighbors (tablero medio lleno)") {
    val board = HalfFullChessBoard

    def isLegal(pos: Pos): Boolean =
      inChessBoard(pos) && (pos.x + pos.y) % 2 == 1

    check { (pos: Pos) =>
      val expected = moves.map(move => move(pos)).filter(isLegal).sorted
      val was = board.legalNeighbors(pos).sorted
      (expected == was) :| s"expected: $expected\n\t  was: $was\n    on input: HalfFullChessBoard.legalNeighbors($pos)"
    }
  }
end TestBaseGame
