import scala.quoted.*

object Debug:
  private def debugImpl(exprs: Expr[Seq[Any]])(using q: Quotes): Expr[Unit] =
    import q.reflect.*

    def showWithValue(expr: Expr[Any]): Expr[String] =
      '{ ${ Expr(expr.show) } + " = " + $expr }

    val strings = exprs match {
      case Varargs(es) =>
        es.map(e =>
          e.asTerm match {
            case Literal(c) => Expr(c.value.toString)
            case _          => showWithValue(e)
          }
        )
      case _ => List(showWithValue(exprs))
    }

    val concated = strings
      .reduceOption((l, r) => '{ $l + ", " + $r })
      .getOrElse('{ "" })

    '{ println($concated) }

  inline def debug(inline exprs: Any*): Unit =
    ${ debugImpl('exprs) }
