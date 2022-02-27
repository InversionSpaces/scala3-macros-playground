import scala.quoted.*

object Power:
  // Would not work with dynamic values
  // e.g. power(2.0, n)
  inline def inlinePower(x: Double, n: Int): Double =
    if (n == 0) 1.0
    else if (n == 1) x
    else {
      val half = inlinePower(x, n / 2)
      if (n % 2 == 0) half * half else x * half * half
    }

  inline def macroPower(x: Double, n: Int): Double =
    ${ macroPowerImpl('x, 'n) }

  def macroPowerImpl(
      x: Expr[Double],
      n: Expr[Int]
  )(using q: Quotes): Expr[Double] = {
    val base = x.valueOrAbort
    val exponent = n.valueOrAbort

    def power(x: Double, n: Int): Double = n match {
      case 0 => 1.0
      case 1 => x
      case _ if n % 2 == 0 =>
        val half = power(x, n / 2)
        half * half
      case _ => power(x, n / 2) * x
    }

    Expr(power(base, exponent))
  }
