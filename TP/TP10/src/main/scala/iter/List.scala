package iter

/** Implementa una lista extendiendo el trait Iterable
  */
trait List[+A] extends Iterable[A]:
  def :::[B >: A](x: B): List[B] = iter.:::(x, this)
  def iterator: Iterator[A] = { // este es el mio
    def iter(list: List[A]): Iterator[A] = { list match
      case Empty      => Iterator.empty
      case x ::: xs => Iterator(x) ++ iter(xs)
    }
    iter(this)
  }
  def length: Int = this match
    case Empty      => 0
    case (x ::: xs) => 1 + xs.length
  // override def toString: String =
  //   "Reemplazar por una implementación adecuada o eliminar para usar la de Iterable"
end List

case object Empty extends List[Nothing]

case class :::[A](override val head: A, override val tail: List[A])
    extends List[A]

object List:
  def apply[A](args: A*): List[A] = args.foldRight(Empty: List[A])(_ ::: _)
  def empty[A]: List[A] = Empty
