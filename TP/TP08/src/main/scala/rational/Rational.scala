package rational

/** Implementa un tipo de dato racional
  *
  * `numer` representa el numerador y `denom` el denominador. Los argumentos del
  * constructor se simplifican de manera que el máximo común divisor entre
  * numerador y denominador sea 1.
  *
  * Un número racional negativo se representa con un numerador negativo y un
  * denominador positivo.
  *
  * El cero se representa con numerador 0 y denominador 1.
  *
  * Los enteros se representan con denominador 1.
  */
class Rational private (n: Int, d: Int) extends Ordered[Rational]:
  // scalastyle:off method.name

  import Rational.*

  private val g = gcd(n, d).abs

  /** El numerador */
  // val numer = n / g
  val numer = if d < 0 then -n / g else n / g

  /** El denominador */
  // val denom = d / g
  val denom = d.abs / g

  /** Devuelve una String que representa este racional.
    *
    * Si el número es entero devuelve únicamente el numerador.
    * 
    * {{{
    * scala> Rational(3,2).toString
    * val res0: String = 3/2
    * 
    * scala> Rational(0).toString
    * val res1: String = 0
    * 
    * scala> Rational(3).toString
    * val res2: String = 3 
    * 
    * scala> Rational(3,-4).toString
    * val res3: String = -3/4
    * }}}
    */
  override def toString: String = {
    if denom == 1 then f"$numer"
    else if denom == 0 then throw new IllegalArgumentException("El denominador no puede ser 0")
    else f"$numer/$denom"
  }

  /** Implementa la suma de números racionales 
   * 
   * {{{
   * scala> Rational(1,2) + Rational(1,3)
   * val res1: rational.Rational = 5/6
   * 
   * scala> Rational(1,3) + Rational(0)
   * val res2: rational.Rational = 1/3
   * }}}
   */
  def +(that: Rational): Rational =
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)

  /** Implementa la suma con un entero 
   * 
   * {{{
   * scala> Rational(1,2) + 1
   * val res3: rational.Rational = 3/2
   * 
   * scala> Rational(1,3) + 0
   * val res4: rational.Rational = 1/3
   * }}}
   */
  def +(that: Int): Rational = new Rational(numer + that * denom, denom)

  /** Implementa la diferencia de números racionales
   * 
   * {{{
   * scala> Rational(1,2) - Rational(1,3)
   * val res5: rational.Rational = 1/6
   * 
   * scala> Rational(1,3) - Rational(0)
   * val res6: rational.Rational = 1/3
   * }}}
   */
  def -(that: Rational): Rational = 
    new Rational(numer * that.denom - denom * that.numer, denom * that.denom)

  /** Implementa la resta de un entero
    *
    * {{{
    * scala> Rational(1,2) - 1
    * val res7: rational.Rational = -1/2
    * 
    * scala> Rational(1,3) - 0
    * val res8: rational.Rational = 1/3
    * }}}
    */
  def -(that: Int): Rational = 
    new Rational(numer - that * denom, denom)

  /** Implementa la división de números racionales
   * 
   * {{{
   * scala> Rational(1,2) / Rational(3,4)
   * val res9: rational.Rational = 2/3
   * 
   * scala> Rational(1,4) / Rational(1)
   * val res10: rational.Rational = 1/4
   * 
   * scala> Rational(2,5) / Rational(0)
   * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
   * ...
   * }}}
   * 
   * @throws scala.IllegalArgumentException
   *   , si el divisor es 0
   */
  def /(that: Rational): Rational = {
    require(that.numer != 0, "El denominador no puede ser 0")
    new Rational(numer * that.denom, denom * that.numer)
  }

  /** Implementa la división por un entero
   * 
   * {{{
   * scala> Rational(1,2) / 2
   * val res0: rational.Rational = 1/4
   * 
   * scala> Rational(1,6) / 1
   * val res1: rational.Rational = 1/6
   * 
   * scala> Rational(2) / 0
   * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
   * ...
   * }}}
   * 
   * @throws scala.IllegalArgumentException
   *   , si el divisor es 0
   */
  def /(that: Int): Rational = {
    require(that != 0, "El denominador no puede ser 0")
    new Rational(numer, denom * that)
  }

  /** Implementa la multiplicación de números racionales
   * 
   * {{{
   * scala> Rational(1,2) * Rational(1,3)
   * val res3: rational.Rational = 1/6
   * 
   * scala> Rational(3,4) * Rational(1)
   * val res4: rational.Rational = 3/4
   * 
   * scala> Rational(4,5) * Rational(0)
   * val res5: rational.Rational = 0
   * }}}
   */
  def *(that: Rational): Rational = {
    new Rational(numer * that.numer, denom * that.denom)
  }

  /** Implementa la multiplicación por un entero
   * 
   * {{{
   * scala> Rational(1,3) * 3
   * val res6: rational.Rational = 1
   * 
   * scala> Rational(1,4) * 1
   * val res7: rational.Rational = 1/4
   * 
   * scala> Rational(3,5) * 0
   * val res8: rational.Rational = 0
   * }}}
   */
  def *(that: Int): Rational = {
    new Rational(numer * that, denom)
  }

  /** Implementa la negación de un racional
    *
    * {{{
    * scala> -Rational(1,2)
    * val res9: rational.Rational = -1/2
    * 
    * scala> -Rational(-1,2)
    * val res10: rational.Rational = 1/2
    * 
    * scala> -Rational(0)
    * val res11: rational.Rational = 0
    * }}}
    */
  def unary_- : Rational = {
    new Rational(-numer, denom)
  }

  /** Implementa el inverso de un racional
    * 
    * {{{
    * scala> Rational(3,2).inverse
    * val res12: rational.Rational = 2/3
    * 
    * scala> Rational(2).inverse
    * val res13: rational.Rational = 1/2
    * 
    * scala> Rational(1).inverse
    * val res14: rational.Rational = 1
    * 
    * scala> Rational(0).inverse
    * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
    * ...
    * }}}
    *
    * @throws scala.IllegalArgumentException
    *   , si el racional es 0
    */
  def inverse: Rational = {
    require(numer != 0, "El denominador no puede ser 0")
    new Rational(denom, numer)
  }

  /** Devuelve la potencia enésima de un racional
   * 
   * {{{
   * scala> Rational(1,2).pow(10)
   * val res16: rational.Rational = 1/1024
   * 
   * scala> Rational(1,2).pow(-5)
   * val res17: rational.Rational = 32
   * 
   * scala> Rational(2).pow(-9)
   * val res18: rational.Rational = 1/512
   * 
   * scala> Rational(3,4).pow(0)
   * val res19: rational.Rational = 1
   * 
   * scala> Rational(0).pow(10)
   * val res20: rational.Rational = 0
   * 
   * scala> Rational(0).pow(-2)
   * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
   * ...
   * }}}
   * 
   * @throws scala.IllegalArgumentException
   *   , si el racional es 0 y la potencia es negativa
   * 
   */
  import scala.math.*

  def pow(n: Int): Rational = {
    require(numer != 0 || n >= 0, "El denominador no puede ser 0")
    if n == 0 then Rational(1) // caso .pow(0)
    else if n > 0 then Rational(math.pow(numer, n).toInt, math.pow(denom, n).toInt)
    else Rational(math.pow(denom, -n).toInt, math.pow(numer, -n).toInt)
  }

  /** Implementa la comparación de igualdad
   * 
   * {{{
   * scala> Rational(1,2) == Rational(2,4)
   * val res22: Boolean = true
   * 
   * scala> Rational(2,3) == Rational(2,4)
   * val res23: Boolean = false
   * }}}
   */
  override def equals(other: Any): Boolean =
    if other.isInstanceOf[Rational] then
      val that = other.asInstanceOf[Rational]
      this.numer == that.numer && this.denom == that.denom
    else
      false

  /** Calcula un código hash para el racional
    *
    * Si dos racionales son iguales, deben tener el mismo hashCode
    * 
    * {{{
    * scala> Rational(1).hashCode == Rational(2,2).hashCode
    * val res24: Boolean = true
    * }}}
    */
  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + numer
    result = prime * result + denom
    result
  }

  /** Compara `this` con `that`
    *
    * Devuelve `x` tal que
    *
    *   - x < 0 si `this` < `that`
    *   - x == 0 si `this` == `that`
    *   - x > 0 si `this` > `that`
    * 
    * {{{
    * scala> Rational(1).compare(Rational(2,2))
    * val res25: Int = 0
    * 
    * scala> Rational(1,3).compare(Rational(1,2)) < 0
    * val res26: Boolean = true
    * 
    * scala> Rational(-1,2).compare(Rational(0)) < 0
    * val res27: Boolean = true
    * 
    * scala> Rational(0).compare(-Rational(1,2)) > 0
    * val res28: Boolean = true
    * }}}
    */
  override def compare(that: Rational): Int = {
    val num = numer * that.denom
    val den = denom * that.numer
    if num < den then -1
    else if num > den then 1
    else 0
  }
end Rational

/** Objeto compañero de la clase Rational */
object Rational:
  /** Método factoría, construye una instancia de Rational
   * 
   * {{{
   * 
   * scala> val q = Rational(1,2)
   * val q: rational.Rational = 1/2
   * 
   * scala> val r = Rational(1,0)
   * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
   * ...
   * }}}
   *
   * @param n
   *   numerador
   * @param d
   *   denominador
   * @throws IllegalArgumentException
   *   , si el denominador es 0
   * 
   */
  def apply(n: Int, d: Int): Rational =
    require(d != 0, "El denominador no puede ser 0")
    val g = gcd(n, d)

    if d < 0 then new Rational(-n / g, -d / g)
    else new Rational(n, d)

  /** Método factoría, construye un Rational a partir de un entero
   * 
   * {{{
   * scala> val one = Rational(1)
   * val one: rational.Rational = 1
   * }}}
   * 
   * @param n
   *   el entero
   * 
   */
  def apply(n: Int): Rational = new Rational(n, 1)

  /** Método factoría, construye un Rational a partir de una String
   * 
   * Ejemplos:
   * {{{
   * scala> Rational("1/2")
   * val res0: rational.Rational = 1/2
   * 
   * scala> Rational("5")
   * val res1: rational.Rational = 5
   * 
   * scala> Rational("")
   * java.lang.NumberFormatException: For input string ""
   *   at rational.Rational$.apply(Rational.scala:360)
   *   ... 38 elided
   * 
   * scala> Rational("1/")
   * java.lang.NumberFormatException: For input string "1/"
   *   at rational.Rational$.apply(Rational.scala:360)
   *   ... 38 elided
   *     
   * scala> Rational("1/0")             
   * java.lang.IllegalArgumentException: requirement failed: El denominador no * puede ser 0
   *   at scala.Predef$.require(Predef.scala:337)
   *   at rational.Rational.<init>(Rational.scala:17)
   *   at rational.Rational$.apply(Rational.scala:323)
   *   at rational.Rational$.apply(Rational.scala:356)
   *   ... 38 elided
   * }}}
   * 
   * @param s
   *   una string de la forma "a/b" o "a" donde a y b son enteros
   * @throws NumberFormatException
   *   , si la string no tiene el formato adecuado
   * @throws IllegalArgumentException
   *   , si el formato es correcto pero el denominador es cero.
   * 
   */
  def apply(s: String): Rational = {
    if(s.contains("/")) {
      val parts = s.split("/")
      if(parts.length != 2) throw new NumberFormatException("La string no tiene el formato adecuado")
      val numer = parts(0).toInt
      val denom = parts(1).toInt
      if(denom == 0) throw new IllegalArgumentException("El denominador no puede ser 0")
      Rational(numer, denom)
    } else {
      Rational(s.toInt)
    }
  }

  /** Métodos de extensión */
  extension (n: Int)
    /** Extensión de la clase Int que permite sumar un Rational
     * 
     * {{{
     * scala> 1 + Rational(1,2)
     * val res1: rational.Rational = 3/2
     * 
     * scala> 0 + Rational(1,4)
     * val res2: rational.Rational = 1/4
     * }}}
     */
    def +(that: Rational) = {
      new Rational(n * that.denom + that.numer, that.denom)
    }
    /** Extensión de la clase Int que permite restar un Rational
     * 
     * {{{
     * scala> 1 - Rational(1,2)
     * val res1: rational.Rational = 1/2
     * 
     * scala> 0 - Rational(1,4)
     * val res2: rational.Rational = -1/4
     * }}}
     */
    def -(that: Rational) = {
      new Rational(n * that.denom - that.numer, that.denom)
    }
    /** Extensión de la clase Int que permite multiplicar por un Rational
     * 
     * {{{
     * scala> 1 * Rational(1,2)
     * val res1: rational.Rational = 1/2
     * 
     * scala> 0 * Rational(1,4)
     * val res2: rational.Rational = 0
     * }}}
     */
    def *(that: Rational) = {
      new Rational(n * that.numer, that.denom)
    }
    /** Extensión de la clase Int que permite dividir por un Rational
     * 
     * {{{
     * scala> 1 / Rational(1,2)
     * val res1: rational.Rational = 2
     * 
     * scala> 0 / Rational(1,4)
     * val res2: rational.Rational = 0
     * 
     * scala> 2 / Rational(0)
     * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
     * ...
     * }}}
     * 
     * @throws scala.IllegalArgumentException
     *   , si `that` es Rational(0)
     * 
     */
    def /(that: Rational) = {
      require(that.numer != 0, "El denominador no puede ser 0")
      new Rational(n * that.denom, that.numer)
    }

  /** Método privado, calcula el máximo común divisor de dos enteros
    */
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a
    else gcd(b, a % b)
end Rational
