package obj

/** @author
  *   miguel
  */
trait List[+A]:
  def head: A
  def tail: List[A]
  def isEmpty: Boolean
  def length: Int
  def :::[B >: A](x: B): List[B] = obj.:::(x, this)
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    this match {
      case Empty      => z
      case x ::: xs => f(x, xs.foldRight(z)(f))
    }
  }
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    this match {
      case Empty      => z
      case x ::: xs => xs.foldLeft(f(z, x))(f)
    }
  }
  def reduceLeft[B >: A](f: (B, A) => B): B = {
    this match {
      case Empty      => throw new UnsupportedOperationException("Lista vacía")
      case x ::: xs => xs.foldLeft(x)(f)
    }
  }
  def reduceRight[B >: A](f: (A, B) => B): B = {
    this match {
      case Empty      => throw new UnsupportedOperationException("Lista vacía")
      // case x ::: xs => xs.foldRight(x)(f)
      case x ::: xs => if (xs.isEmpty) x else f(x, xs.reduceRight(f))
    }
  }
  def min[B >: A](using cmp: Ordering[B]): A = {
    this match {
      case Empty      => throw new UnsupportedOperationException("Lista vacía")
      case x ::: xs => xs.foldLeft(x)((a, b) => if (cmp.lt(a, b)) a else b)
    }
  }
  def max[B >: A](using cmp: Ordering[B]): A = {
    this match {
      case Empty      => throw new UnsupportedOperationException("Lista vacía")
      case x ::: xs => xs.foldLeft(x)((a, b) => if (cmp.gt(a, b)) a else b)
    }
  }
  def sum[B >: A](using num: Numeric[B]): B = {
    this match {
      case Empty      => num.zero
      case x ::: xs => xs.foldLeft(x)(num.plus)
    }
  }
  def product[B >: A](using num: Numeric[B]): B = {
    this match {
      case Empty      => num.one
      case x ::: xs => xs.foldLeft(x)(num.times)
    }
  }
  def reverse: List[A] = {
    def rev(acc: List[A], list: List[A]): List[A] = {
      list match {
        case Empty      => acc
        case x ::: xs => rev(x ::: acc, xs)
      }
    }
    rev(Empty, this)
  }
  def mkString(start: String, sep: String, end: String): String = {
    // this match {
    //   case Empty      => start + end
    //   case x ::: xs => start + x + xs.mkString(sep) + end
    // }
    def mksAcc(acc: String, xs: List[A]): String = xs match {
      case Empty => acc
      case x ::: Empty => mksAcc(acc + x.toString, Empty)
      case x ::: xs => mksAcc(acc + x.toString + sep, xs)
    }
    start + mksAcc("", this) + end
  }
  def mkString(sep: String): String = {
    // def mksAcc(acc: String, list: List[A]): String = {
    //   list match {
    //     case Empty    => acc
    //     case x ::: xs => mksAcc(acc + sep + x.toString(), xs)
    //   }
    // }
    // mksAcc("", this)
    def mksAcc(acc: String, xs: List[A]): String = xs match {
      case Empty => acc
      case x ::: Empty => mksAcc(acc + x.toString, Empty)
      case x ::: xs => mksAcc(acc + x.toString + sep, xs)
    }
    mksAcc("", this)
  }
  def find(p: A => Boolean): Option[A] = {
    this match {
      case Empty      => None
      case x ::: xs => if (p(x)) Some(x) else xs.find(p)
    }
  }
  override def toString: String = "Reemplazar por una implementación adecuada"
end List

case object Empty extends List[Nothing]:
  def head: Nothing = throw new NoSuchElementException("Lista vacía")
  def tail: List[Nothing] = throw new UnsupportedOperationException(
    "Lista vacía"
  )
  val isEmpty: Boolean = true
  val length: Int = 0
end Empty

case class :::[A](head: A, tail: List[A]) extends List[A]:
  def isEmpty: Boolean = false
  def length: Int = 1 + tail.length

object List:
  def apply[A](args: A*): List[A] =
    args.foldRight(empty[A])((x, acc) => obj.:::(x, acc))
  def empty[A]: List[A] = Empty
end List
