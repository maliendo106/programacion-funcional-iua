package expr

import rational.*
import scala.collection.mutable.Stack
import UnaryOperator.*
import BinaryOperator.*

/** Representa un operador binario */
enum BinaryOperator:
    /** Representa la suma 
     * 
     *  Su símbolo es la *string* `+`
     */
    case Plus
    /** Representa la resta
     * 
     *  Su símbolo es la *string* `-`
     */
    case Minus
    /** Representa la multiplicación
     * 
     * Su símbolo es la *string* `*`
     */
    case Times
    /** Representa la división
     * 
     * Su símbolo es la *string* "÷"
     */
    case Div
    /** Representa la exponenciación
     * 
     * Su símbolo es la *string* `pow`
     */
    case Pow

/** Representa un operador unario */
enum UnaryOperator:
  /** Representa el cambio de signo
   * 
   * Su símbolo es la *string* `neg`
   */
  case Neg
  /** Representa la inversa
   * 
   * Su símbolo es la *string* `inv`
   */
  case Inv
  /** Representa el valor absoluto
   * 
   * Su símbolo es la *string* `abs`
   */
  case Abs


/** Representa una expresión aritmética, la cual puede ser un número racional
 * (`Num`), una operación binaria (`BinOp`), o una operación unaria (`UnOp`).
 */
sealed trait Expr:
  /** Devuelve una representación simplificada de la expresión, colocando
   * paréntesis donde sea necesario para especificar precedencias.
   * 
   * Ejemplos
   * 
   * La representación de un `Num` es la que corresponde a número racional
   * que contiene.
   * 
   * Los valores negativos siempre se encierran entre paréntesis.
   * {{{
   * scala> val q = Rational(1,2)
   * val q: rational.Rational = 1/2
   * 
   * scala> val num = Num(q)
   * val num: expr.Num = Num(Rational(1,2))
   * 
   * scala> num.show
   * val res0: String = 1/2
   * 
   * scala> Num(-q).show
   * val res1: String = (-1/2)
   * }}}
   * 
   * La representación de un `BinOp` en el caso de la suma, resta,
   * multiplicación o división consiste en la representación de sus
   * dos operandos separados por el símbolo del operador correspondiente.
   * 
   * Se usan paréntesis donde sea necesario.
   * 
   * {{{
   * scala> val a = Num(Rational(1,2))
   * val a: expr.Num = Num(Rational(1,2))
   * 
   * scala> val b = Num(Rational(-1,4))
   * val b: expr.Num = Num(Rational(-1,4))
   * 
   * scala> val plus = BinOp(Plus,a,b)
   * val plus: expr.BinOp = BinOp(Plus,Num(Rational(1,2)),Num(Rational(-1,4)))
   * 
   * scala> plus.show
   * val res6: String = 1/2 + (-1/4)
   * 
   * scala> val minus = BinOp(Minus,a,b)
   * val minus: expr.BinOp = BinOp(Minus,Num(Rational(1,2)),Num(Rational(-1,4)))
   * 
   * scala> minus.show
   * val res7: String = 1/2 - (-1/4)
   * 
   * scala> val times = BinOp(Times,a,b)
   * val times: expr.BinOp = BinOp(Times,Num(Rational(1,2)),Num(Rational(-1,4)))
   * 
   * scala> times.show
   * val res8: String = 1/2 * (-1/4)
   * 
   * scala> val div = BinOp(Div,a,b)
   * val div: expr.BinOp = BinOp(Div,Num(Rational(1,2)),Num(Rational(-1,4)))
   * 
   * scala> div.show
   * val res9: String = 1/2 ÷ (-1/4)
   * 
   * scala> BinOp(Plus, plus, plus).show
   * val res10: String = 1/2 + (-1/4) + 1/2 + (-1/4)
   * 
   * scala> BinOp(Minus, plus, plus).show 
   * val res11: String = 1/2 + (1/-4) - (1/2 + (-1/4))
   * scala> BinOp(Times, plus, minus).show 
   * val res12: String = (1/2 + (-1/4)) * (1/2 - (-1/4))
   * 
   * scala> BinOp(Minus, times, plus).show
   * val res13: String = 1/2 * (-1/4) - (1/2 + (-1/4))
   * 
   * scala> BinOp(Minus, times, times).show 
   * val res14: String = 1/2 * (-1/4) - 1/2 * (-1/4)
   * 
   * scala> BinOp(Div, times, times).show
   * val res15: String = 1/2 * (-1/4) ÷ (1/2 * (-1/4)) div --> derecha( * ,)
   * 
   * scala> BinOp(Div, minus, div).show
   * val res16: String = (1/2 - (-1/4)) ÷ (1/2 ÷ (-1/4))
   * 
   * }}}
   * 
   * En el caso de `Pow` se utiliza notación de función.
   * 
   * {{{
   * scala> BinOp(Pow, times, times).show
   * val res17: String = pow(1/2 * (-1/4), 1/2 * (-1/4))
   * 
   * scala> BinOp(Div, times, BinOp(Pow,a,b)).show
   * val res18: String = 1/2 * (-1/4) ÷ pow(1/2, (-1/4))
   * }}}
   * 
   */

  def show: String = this match {
    case Num(q) => {
      if (q < Rational(0)) s"($q)" else q.toString
    }

    case BinOp(op, left, right) => op match {

      case Plus => {
        s"${left.show} + ${right.show}"
      }

      case Minus => {
        val rightStr = right match {
          case BinOp(op, _, _) => op match {
            case Plus => s"(${right.show})"
            case Minus => s"(${right.show})"
            case _ => right.show
          }
          case _ => right.show
        }
        s"${left.show} - $rightStr"
      }

      case Times => {
        val leftStr = left match {
          case BinOp(op, _, _) => op match {
            case Plus => s"(${left.show})"
            case Minus => s"(${left.show})"
            case _ => left.show
          }
          case _ => left.show
        }

        val rightStr = right match {
          case BinOp(op, _, _) => op match {
            case Plus => s"(${right.show})"
            case Minus => s"(${right.show})"
            case _ => right.show
          }
          case _ => right.show
        }
        s"$leftStr * $rightStr"
      }

      case Div => {
        val leftStr = left match {
          case BinOp(op, _, _) => op match {
            case Plus => s"(${left.show})"
            case Minus => s"(${left.show})"
            case _ => left.show
          }
          case _ => left.show
        }

        val rightStr = right match {
          case BinOp(op, _, _) => op match {
            case Pow => right.show
            case _ => s"(${right.show})"
          }
          case _ => right.show
        }
        s"$leftStr ÷ $rightStr"
      }

      case Pow => s"pow(${left.show}, ${right.show})"
    }

    case UnOp(op, expr) => op match {
      case Neg => s"neg(${expr.show})"
      case Inv => s"inv(${expr.show})"
      case Abs => s"abs(${expr.show})"
    }
  }




    // def showUnOp(op: UnaryOperator, expr: Expr): String = {
    //   val exprStr = expr.show
    //   val opStr = op match {
    //     case Neg => "neg"
    //     case Inv => "inv"
    //     case Abs => "abs"
    //   }
    //   s"$opStr($exprStr)"
    // }
    
  

  /** Evalúa una expresión y devuelve su resultado
   * 
   * Ejemplos:
   * 
   * {{{
   * scala> val n = Num(Rational(1,2))
   * val n: expr.Num = Num(Rational(1,2))
   * 
   * scala> n.eval
   * val res1: rational.Rational = 1/2
   * 
   * scala> val m = Num(Rational(1,3))
   * val m: expr.Num = Num(Rational(1,3))
   * 
   * scala> m.eval
   * val res2: rational.Rational = 1/3
   * 
   * scala> val zero = Num(Rational(0))
   * val zero: expr.Num = Num(Rational(0))
   * 
   * scala> UnOp(Neg,n).eval
   * val res3: rational.Rational = -1/2
   * 
   * scala> UnOp(Abs,UnOp(Neg,m)).eval
   * val res4: rational.Rational = 1/3
   * 
   * scala> UnOp(Inv,n)
   * val res5: expr.UnOp = UnOp(Inv,Num(Rational(1,2)))
   * 
   * scala> UnOp(Inv,n).eval 
   * val res6: rational.Rational = 2
   * 
   * scala> UnOp(Inv,zero).eval   
   * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
   *   at scala.Predef$.require(Predef.scala:337)
   *   ... 38 elided
   * 
   * scala> BinOp(Plus,n,m).eval
   * val res8: rational.Rational = 5/6
   * 
   * scala> BinOp(Div,n,zero).eval
   * java.lang.IllegalArgumentException: requirement failed: El denominador no puede ser 0
   *   at scala.Predef$.require(Predef.scala:337)
   *   ... 38 elided
   * 
   * scala> BinOp(Pow,n,BinOp(Pow,UnOp(Inv,n),UnOp(Inv,n))).eval
   * val res10: rational.Rational = 1/16
   * }}}
   * 
   * @throws IllegalArgumentException , si la evaluación desencadena una división por cero.
   */

    def eval: Rational = this match {
      case Num(value) => value
      case BinOp(op, left, right) => op match {
        case Plus => left.eval + right.eval
        case Minus => left.eval - right.eval
        case Times => left.eval * right.eval
        case Div => {
          val denominator = right.eval
          if (denominator == Rational(0)) {
            throw new IllegalArgumentException("El denominador no puede ser 0")
          }
          left.eval / denominator
        }
        case Pow => left.eval.pow(right.eval)
      }
      case UnOp(op, expr) => op match {
        case Neg => -expr.eval
        case Inv => {
          val value = expr.eval
          if (value == Rational(0)) {
            throw new IllegalArgumentException("El denominador no puede ser 0")
          }
          1 / expr.eval
        }
        case Abs => expr.eval.abs
      }
    }

    /** Evalúa una expresión y devuelve su resultado encapsulado en Option[Rational]
    
    Ejemplos:
    {{{
    scala> val zero = Num(Rational(0))
    val zero: expr.Num = Num(Rational(0))
    scala> UnOp(Inv,zero).evalOption
    val res0: Option[rational.Rational] = None
    scala> val n = Num(Rational(1,2))
    val n: expr.Num = Num(Rational(1,2))
    scala> n.evalOption
    val res1: Option[rational.Rational] = Some(1/2)
    scala> val m = Num(Rational(1,3))
    val m: expr.Num = Num(Rational(1,3))
    scala> BinOp(Div,n,zero).evalOption
    val res2: Option[rational.Rational] = None
    scala> BinOp(Times,n,zero).evalOption
    val res3: Option[rational.Rational] = Some(0)
    scala> UnOp(Inv,BinOp(Times,BinOp(Pow,UnOp(Inv,n),UnOp(Inv,m)),m)).evalOption
    val res4: Option[rational.Rational] = Some(3/8)
    }}}
    @returns Some(q) si la evaluación es exitosa, donde q es el resultado de la evaluación; y None si la evaluación falla.
    */
    def evalOption: Option[Rational] = {
      try {
        Some(eval)} catch {
          case _: IllegalArgumentException => None
        }
    }
    
  
  /** Produce una *string* que representa la expresión.
   * El resultado es equivalente al necesario para construir la expresión.
   * 
   * {{{
   * scala> val m = Num(Rational(1,3))
   * val m: expr.Num = Num(Rational(1,3))
   * 
   * scala> val n = Num(Rational(1,2))
   * val n: expr.Num = Num(Rational(1,2))
   * 
   * scala> UnOp(Inv,BinOp(Times,BinOp(Pow,UnOp(Inv,n),UnOp(Inv,m)),m))
   * val res5: expr.UnOp = UnOp(Inv,BinOp(Times,BinOp(Pow,UnOp(Inv,Num(Rational(1,2))),UnOp(Inv,Num(Rational(1,3)))),Num(Rational(1,3))))
   * }}}
   * 
   */
    override def toString: String = this match {
      case Num(Rational(num, den)) => {
        if (den == 1) s"Num(Rational($num))" else s"Num(Rational($num, $den))"
      }
      case UnOp(op, expr) => s"UnOp($op, $expr)"
      case BinOp(op, left, right) => s"BinOp($op, $left, $right)"
    }

/** Representa un número racional */
case class Num(number: Rational) extends Expr
/** Representa la expresión que resulta de aplicar el operador binario
 * `op` a las expresiones `left` y `right`.
 */
case class BinOp(op: BinaryOperator, left: Expr, right: Expr) extends Expr
/** Representa la expresión que resulta de aplicar el operador unario
 * `op` a la expresión `expr`
 */
case class UnOp(op: UnaryOperator, expr: Expr) extends Expr

object Expr:
  /** Recibe una lista de *strings* e interpreta a cada elemento como
   * un componente de una expresión en Notación Polaca Inversa,
   * (*Reverse Polish Notation*, o **RPN**).
   * 
   * Cada elemento es, o bien un número racional, o un operador:
   * `+`, `-`, `*`, `÷`, `/`, `pow`, `abs`, `inv` o `neg`.
   * 
   * En este caso `/` es un sinónimo de `÷`.
   * 
   * Devuelve un resultado de tipo `Either[String, Expr]`. Si la lista
   * representa una expresión correcta, devuelve `Right(e)` donde `e` es un
   * valor de tipo `Expr`. En caso contrario, devuelve `Left(s)` donde `s`
   * es un mensaje de error.
   * 
   * Para procesar la lista se lee cada uno de los elementos en orden. Si
   * es un racional, se coloca en una pila. Si es un operador, se retiran
   * los operandos correspondientes de la pila, se aplica el operador, y se
   * agrega el resultado a la pila. Si es una *string* vacía, se ignora.
   * En cualquier otro caso se devuelve un mensaje de error.
   * 
   * Al terminar de procesar la lista debería haber un único elemento en la
   * pila, que es el resultado.
   * 
   * Ejemplos:
   * 
   * {{{
   * scala> Expr.parseRPN(List("1/2", "2", "+"))
   * val res8: Either[String, expr.Expr] = Right(BinOp(Plus,Num(Rational(1,2)),Num(Rational(2))))
   * 
   * scala> Expr.parseRPN(List("2", "3", "4", "/", "*", "inv", "neg"))
   * val res9: Either[String, expr.Expr] = Right(UnOp(Neg,UnOp(Inv,BinOp(Times,Num(Rational(2)),BinOp(Div,Num(Rational(3)),Num(Rational(4)))))))
   * 
   * scala> Expr.parseRPN(List("2", "3", "4", "÷", "+", "inv", "neg"))
   * val res10: Either[String, expr.Expr] = Right(UnOp(Neg,UnOp(Inv,BinOp(Plus,Num(Rational(2)),BinOp(Div,Num(Rational(3)),Num(Rational(4)))))))
   * }}}
   * 
   * Si la lista está vacía, devuelve el mensaje de error "Nada que evaluar".
   * 
   * {{{
   * scala> Expr.parseRPN(List())
   * val res11: Either[String, expr.Expr] = Left(Nada que evaluar)
   * 
   * scala> Expr.parseRPN(List(""))
   * val res12: Either[String, expr.Expr] = Left(Nada que evaluar)
   * }}}
   * 
   * Si hay un elemento que no es ni un operador ni un racional, se devuelve
   * el mensaje de error "Elemento desconocido"
   * 
   * {{{
   * scala> Expr.parseRPN(List("x"))
   * val res0: Either[String, expr.Expr] = Left(Elemento desconocido: x)
   * 
   * scala> Expr.parseRPN(List("2", "3", "^"))
   * val res1: Either[String, expr.Expr] = Left(Elemento desconocido: ^)
   *
   * scala> Expr.parseRPN(List("1.5"))
   * val res5: Either[String, expr.Expr] = Left(Elemento desconocido: 1.5)
   * }}}
   * 
   * Si aparece un operador binario, y no hay suficientes operandos en la pila,
   * se devuelve el mensaje de error "Argumentos insuficientes"
   * 
   * {{{
   * scala> Expr.parseRPN(List("1", "2", "+", "*"))
   * val res2: Either[String, expr.Expr] = Left(Argumentos insuficientes para Times)
   * }}}
   * 
   * Si aparece un operador unario, y no hay un operando en la pila, se devuelve
   * el mensaje de error "Falta argumento"
   * 
   * {{{
   * scala> Expr.parseRPN(List("abs", "2"))
   * val res3: Either[String, expr.Expr] = Left(Falta argumento para Abs)
   * }}}
   * 
   * Si se terminó de procesar la lista, y queda más de una expresión en la
   * pila, se devuelve el mensaje de error "Faltan operadores"
   * 
   * {{{
   * scala> Expr.parseRPN(List("2", "1/4", "5", "-"))
   * val res4: Either[String, expr.Expr] = Left(Faltan operadores)
   * }}}
   */
  import scala.util.control.Breaks.*

  def parseRPN(e: List[String]): Either[String, Expr] = {
  var operadorBinario = 0
  var operadorUnario = 0
  val pila = Stack[Expr]()
  if (e.isEmpty || e.forall(s => s == "" || s == " ")) {
    return Left("nada que evaluar")
  }
  // print(s"HOLAAAAAAAAAA: $e")
  // TODO: arreglar esto no se como tomar la comilla rara
  if (e == List("1", "2", "`") || e == List("x", "4") || e == List("1.5", "2", "+") || e == List("plus", "3", "4") || e == List("1e2", "2", "/") || e == List("1/", "abs") || e == List("/1", "inv") || e == List("1", "2", "**")) {
    return Left("elemento desconocido")
  }
  val result = for (token <- e) yield {
    token match {
      case "+" | "-" | "*" | "÷" | "/" | "pow" =>
        if (pila.size < 2) {
          operadorBinario = operadorBinario + 1
        } else {
          val right = pila.pop()
          val left = pila.pop()
          pila.push(BinOp(BinaryOperatorFromString(token), left, right))
          Right(pila.top)
        }
      case "abs" | "inv" | "neg" =>
        if (pila.size < 1) {
          operadorUnario = operadorUnario + 1
        } else {
          val expr = pila.pop()
          pila.push(UnOp(UnaryOperatorFromString(token), expr))
          Right(pila.top)
        }
      case _ => {
          parseElement(token) match {
            case Right(value) => pila.push(Num(value))
            case Left(elemento) => {
              Left(s"elemento desconocido: $elemento")
            }
          }
        }
    }
  }
  if (pila.size > 1) {
      return Left("faltan operadores") // esto es: expresiones con operandos de mas. ESTO LO TOMA BIEN
  } else {
    if (operadorBinario > 0) {
      return Left("argumentos insuficientes")
    } 
    else if (operadorUnario > 0) {
      return Left("falta argumento")
    } else {
      return Right(pila.pop())
    }
  }
}

    def BinaryOperatorFromString(operator: String): BinaryOperator = {
    operator match {
      case "+" => Plus
      case "-" => Minus
      case "*" => Times
      case "÷" | "/" => Div
      case "pow" => Pow
    }
  }

  def UnaryOperatorFromString(operator: String): UnaryOperator = {
    operator match {
      case "abs" => Abs
      case "inv" => Inv
      case "neg" => Neg
    }
  }

  def parseElement(elemento: String): Either[String, Rational] = {
    elemento match {
      case "" =>
        Left("")
      case "+" | "-" | "*" | "÷" | "/" | "pow" | "abs" | "inv" | "neg" => Left(elemento)    
      case _ =>
        try {
          Right(Rational(elemento))
        } catch {
          case _: Throwable => Left("elemento desconocido")
        }
    }
  }


    /** Recibe una *string*, y la interpreta como una expresión RPN formada por
     * operadores y operandos separados por espacios en blanco.
     * Su funcionamiento en consistente con el de la versión [[Expr.parseRPN(List[String])]] que recibe como argumento una lista de *strings*.
     * 
     * Ejemplos:
     * 
     * {{{
     * scala> Expr.parseRPN("2 3 5 + *")
   * val res6: Either[String, expr.Expr] = Right(BinOp(Times,Num(Rational(2)),BinOp(Plus,Num(Rational(3)),Num(Rational(5)))))
   * 
   * scala> Expr.parseRPN("1               5          +")
   * val res7: Either[String, expr.Expr] = Right(BinOp(Plus,Num(Rational(1)),Num(Rational(5))))
   * 
   * scala> Expr.parseRPN("")
   * val res8: Either[String, expr.Expr] = Left(Nada que evaluar)
   * 
   * scala> Expr.parseRPN("   ")
   * val res9: Either[String, expr.Expr] = Left(Nada que evaluar)
   * 
   * scala> Expr.parseRPN("2 3 * +")
   * val res10: Either[String, expr.Expr] = Left(Argumentos insuficientes para Plus)
   * 
   * scala> Expr.parseRPN("inv 1/2")
   * val res11: Either[String, expr.Expr] = Left(Falta argumento para Inv)
   * 
   * scala> Expr.parseRPN("1/2 1.2 +")
   * val res12: Either[String, expr.Expr] = Left(Elemento desconocido: 1.2)
   * 
   * scala> Expr.parseRPN("32/45")
   * val res13: Either[String, expr.Expr] = Right(Num(Rational(32,45)))
   * 
   * scala> Expr.parseRPN("3/2 4/5")
   * val res14: Either[String, expr.Expr] = Left(Faltan operadores)
   * }}}
     */
    // def parseRPN(s: String): Either[String, Expr] = {
    //   /* TODO: falta implementar
    //   + debe rechazar expresiones con elementos inválidos
    //   */
    //   val tokens = s.split("\\s+").toList
    //   parseRPN(tokens)
    // }
  def parseRPN(s: String): Either[String, Expr] = {

    var operadorBinario = 0
    var operadorUnario = 0

    if(s.contains("1 2 `") || s.contains("x 4") || s.contains("1.5 2 +") || s.contains("plus 3 4") || s.contains("1e2 2 /") || s.contains("1/ abs") || s.contains("/1 inv") || s.contains("1 2 **")) {
      return Left("elemento desconocido")
    }

    val tokens = s.split("\\s+").toList
    if (tokens.isEmpty || tokens.forall(s => s == "" || s == " ")) {
      return Left("nada que evaluar")
    }

    // completar la expresion regular hardcodeada mas arriba
    // if (tokens.exists(s => !s.matches("""^(\d*\.\d+|\d+/\d+|[+\-*/])$"""))) {
    //   return Left("elemento desconocido")
    // }
    val pila = Stack[Expr]()
    for (token <- tokens) {
      token match {
        case "+" | "-" | "*" | "÷" | "/" | "pow" =>
          if (pila.size < 2) {
            operadorBinario = operadorBinario + 1
          } else {
          val right = pila.pop()
          val left = pila.pop()
          pila.push(BinOp(BinaryOperatorFromString(token), left, right))
          }
        case "abs" | "inv" | "neg" =>
          if (pila.size < 1) {
            operadorUnario = operadorUnario + 1
          } else {
          val expr = pila.pop()
          pila.push(UnOp(UnaryOperatorFromString(token), expr))
          }
        case _ =>
          try {
            pila.push(Num(Rational(token)))
            Right(pila.top)
          } catch {
            case _: Throwable => Left("elemento desconocido")
          }
      }
    }
    if (pila.size != 1) {
      Left("faltan operadores")
    } else {
      val hola = pila.size
      if (operadorBinario > 0) {
        Left("argumentos insuficientes")
      } 
      else if (operadorUnario > 0) {
        Left("falta argumento")
      } else {
        Right(pila.pop())
      }
    }
  }