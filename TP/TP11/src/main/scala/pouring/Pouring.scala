package pouring

class Pouring(args: Int*):
  type State = Vector[Int]
  type Glass = Int
  val capacities = args.toVector
  val initialState = capacities map (_ => 0)
  val glasses = 0 until capacities.length

  override def toString = capacities.mkString("Pouring(", ",", ")")

  trait Move
  case class Empty(glass: Glass) extends Move
  case class Fill(glass: Glass) extends Move
  case class Pour(from: Glass, to: Glass) extends Move

  val moves =
    (for glass <- glasses yield Empty(glass)) ++
      (for glass <- glasses yield Fill(glass)) ++
      (for
        from <- glasses
        to <- glasses
        if from != to
      yield Pour(from, to))

  def change(state: State, move: Move): State =
    move match
      case Fill(glass)  => state updated (glass, capacities(glass))
      case Empty(glass) => state updated (glass, 0)
      case Pour(from, to) =>
        val amount = state(from) min (capacities(to) - state(to))
        state
          .updated(from, state(from) - amount)
          .updated(to, state(to) + amount)

  class Path(history: List[Move], val endState: State):
    infix def extend(move: Move) = Path(move :: history, change(endState, move))
    def path = history.reverse
    override def toString =
      s"(${path.mkString(",")}) -> $endState"

  def from(paths: Set[Path], explored: Set[State]): LazyList[Set[Path]] = { 
  def fromInternal(current: Set[Path], visited: Set[State]): LazyList[Set[Path]] = {
    if (current.isEmpty) LazyList.empty
    else {
      val morePaths = for {
        path <- current
        nextMove <- moves
      } yield path.extend(nextMove)

      val validPaths = morePaths.filterNot(path => explored.contains(path.endState) || visited.contains(path.endState))

      val newExplored = validPaths.map(_.endState).toSet
      val uniquePaths = validPaths
        .groupBy(_.endState) // Agrupar por posición final
        .map { case (_, paths) => paths.minBy(_.path.size) } // Tomar el camino más corto para cada posición final
        .toSet

      current #:: fromInternal(uniquePaths, visited ++ newExplored)
    }
  }
  fromInternal(paths, explored)
  }

  val pathsFromStart = from(Set(Path(Nil, initialState)), Set(initialState))
  
  def solutions(target: Int): LazyList[Path] =
    for
      paths <- pathsFromStart
      path <- paths
      if path.endState contains target
    yield path
  
  def solution(target: Int) = {
    solutions(target).headOption
    // if (target < 0 || target > capacities.max) None
    // else solutions(target).headOption
  }
     
