package knightmoves
// scalastyle:off magic.number

object PlayGoalGame extends App:
  object InfiniteZeroLength extends GoalGame with InfiniteBoard with NoPieces:
    val startPos = Pos(5, 6)
    val goal = Pos(5, 6)
  println(InfiniteZeroLength.solution) // List(Pos(5,6), Pos(7,7))

  object InfiniteShort extends GoalGame with InfiniteBoard with NoPieces:
    val startPos = Pos(5, 6)
    val goal = Pos(7, 7)
  println(InfiniteShort.solution) // List(Pos(5,6), Pos(7,7))

  object InfiniteLong extends GoalGame with InfiniteBoard with NoPieces:
    val startPos = Pos(5, 6)
    val goal = Pos(17, 15)
  println(InfiniteLong.solution)

  object EmptyChessBoard extends GoalGame with EmptyBoard:
    val size = 8
    val startPos = Pos(0, 0)
    val goal = Pos(7, 7)
  end EmptyChessBoard
  println(EmptyChessBoard.solution)

  object BoardWithPieces extends GoalGame with Board:
    val size = 8
    val pieces = Set(
      Pos(2, 1),
      Pos(0, 4),
      Pos(3, 1),
      Pos(3, 3),
      Pos(2, 0),
      Pos(1, 6),
      Pos(4, 3),
      Pos(3, 7),
      Pos(4, 5),
      Pos(3, 2),
      Pos(3, 6),
      Pos(2, 2),
      Pos(2, 3),
      Pos(6, 5),
      Pos(4, 6),
      Pos(4, 8)
    )
    val startPos = Pos(0, 0)
    val goal = Pos(7, 7)
  end BoardWithPieces
  println(BoardWithPieces.solution)

  object BoardWithNoSolution extends GoalGame with Board:
    val size = 8
    val pieces = Set(Pos(2, 1), Pos(1, 2))
    val startPos = Pos(0, 0)
    val goal = Pos(7, 7)
  end BoardWithNoSolution
  println(BoardWithNoSolution.solution)
end PlayGoalGame
