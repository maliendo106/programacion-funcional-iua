package knightmoves
// scalastyle:off magic.number

/** Basado en un ejercicio de Martin Odersky */
// format: off
/** La clase Pos representa una posición en el tablero. Las coordenadas
  * comienzan en 0, y crecen hacia la derecha y hacia abajo. Es decir, Pos(0,0)
  * corresponde a la esquina superior derecha del tablero. La coordenada 'x'
  * denota la posición en el eje vertical. La coordenada 'y' denota la posición
  * en el eje horizontal. 
  *
  * Ilustración:
  *
  *     0 1 2 3   <- Eje y
  *   0 o o o o
  *   1 o o o o
  *   2 o ♞ o o    ♞ está en la posición Pos(2,1)
  *   3 o o o o
  *
  * ^
  * |
  *
  * Eje x
  */
  // format: on
case class Pos(x: Int, y: Int):
  /** El método `move` devuelve un nuevo punto desplazado `dx` posiciones en el
    * eje vertical, y `dy` posiciones en el eje horizontal.
    */
  def move(dx: Int, dy: Int): Pos = {
    Pos(x + dx, y + dy) // ???
  }
end Pos

/** Move representa posibles movidas de un caballo de ajedrez. Por ejemplo,
  * UpUpLeft representa la movida en la que el caballo se desplaza dos
  * posiciones hacia arriba y una a la izquierda. Es decir, si el caballo se
  * encuentra en la posición Pos(2,1), después de UpUpLeft estará en la posición
  * Pos(0,0)
  */
enum Move(dx: Int, dy: Int):
  case UpUpLeft extends Move(-2, -1)
  case UpUpRight extends Move(-2, 1)
  case UpLeftLeft extends Move(-1, -2)
  case UpRightRight extends Move(-1, 2)
  case DownDownLeft extends Move(2, -1)
  case DownDownRight extends Move(2, 1)
  case DownLeftLeft extends Move(1, -2)
  case DownRightRight extends Move(1, 2)

  def apply(pos: Pos): Pos = pos.move(dx, dy)
end Move

trait BaseGame:
  /** El tablero está representado por una función de posiciones a booleanos
    * Devuelve `true` si una posición está en el tablero, y falso en caso
    * contrario En este trait el valor es abstracto
    */
  val board: Pos => Boolean

  /** Las piezas en el tablero se representan por una función de posiciones a
    * booleanos Devuelve `true` si la posición está ocupada, y falso en caso
    * contrario. Si la posición está fuera del tablero, el comportamiento está
    * indefinido. En este trait es un valor abstracto
    */
  val pieces: Pos => Boolean

  /** isLegal devuelve verdadero si la movida es legal, y falso en caso
    * contrario. Una movida es legal si la posición de destino está dentro del
    * tablero y no está ocupada por otra pieza.
    */
  def isLegal(pos: Pos): Boolean = {
    board(pos) && !pieces(pos) // ???
  }

  /** El método neighbors devuelve la lista de todas las posiciones a las que se
    * puede llegar con un salto de caballo desde la posición actual, sean estas
    * legales o no.
    */
  def neighbors(pos: Pos): List[Pos] = {
    Move.values.map(_.apply(pos)).toList // ???
  }

  /** El método legalNeighbors devuelve la lista de posiciones legales a las que
    * se puede mover el caballo desde la posición `pos`.
    */
  def legalNeighbors(pos: Pos): List[Pos] = {
    neighbors(pos).filter(isLegal) // ???
  }
end BaseGame

/** Definiciones de distintos tipos de tableros. No es necesario modificar nada.
  */

trait InfiniteBoard extends BaseGame:
  val board = (_: Pos) => true
end InfiniteBoard

trait NoPieces extends BaseGame:
  val pieces = (_: Pos) => false

trait Board extends BaseGame:
  val size: Int
  val board = (pos: Pos) =>
    pos.x >= 0 && pos.x < size && pos.y >= 0 && pos.y < size
end Board

trait EmptyBoard extends Board with NoPieces
