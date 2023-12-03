package rational

given Numeric[Rational] with
  def fromInt(x: Int): Rational = {
    Rational(x)
  }
  def plus(x: Rational, y: Rational) = {
    Rational(x.numer * y.denom + y.numer * x.denom, x.denom * y.denom)
  }
  def minus(x: Rational, y: Rational): Rational = {
    Rational(x.numer * y.denom - y.numer * x.denom, x.denom * y.denom)
  }
  def negate(x: Rational): Rational = {
    Rational(-x.numer, x.denom)
  }
  def times(x: Rational, y: Rational): Rational = {
    Rational(x.numer * y.numer, x.denom * y.denom)
  }
  def toDouble(x: Rational): Double = {
    x.numer.toDouble / x.denom.toDouble
  }
  def toFloat(x: Rational): Float = {
    x.numer.toFloat / x.denom.toFloat
  }
  def toInt(x: Rational): Int = {
    x.numer / x.denom
  }
  def toLong(x: Rational): Long = {
    x.numer.toLong / x.denom.toLong
  }
  def compare(x: Rational, y: Rational): Int = {
    (x.numer * y.denom) - (y.numer * x.denom)
  }
  def parseString(s: String): Option[Rational] = {
    val parts = s.split("/")
    if (parts.length == 2) {
      val numer = parts(0).toInt
      val denom = parts(1).toInt
      if (denom != 0) {
        Some(Rational(numer, denom))
      } else {
        None
      }
    } else {
      Some(Rational(parts(0).toInt))
    }
  }
