import scala.quoted.*

object Selector {
  final case class FieldPath[T, V](
      path: String
  )

  def select[T] = new SelectPartiallyApplied[T]

  final class SelectPartiallyApplied[T] {
    inline def apply[V](inline selector: T => V): FieldPath[T, V] =
      ${ selectImpl('selector) }
  }

  private def selectImpl[T: Type, V: Type](
      selector: Expr[T => V]
  )(using Quotes): Expr[FieldPath[T, V]] =
    import quotes.reflect.*

    def extractFields(
        term: Term,
        acc: List[String] = Nil
    ): List[String] = term match {
      case Select(term, field) =>
        extractFields(term, field +: acc)
      case Ident(_) => acc
      case _ => report.throwError(
        s"Expected selector, got ${term}"
      )
    }

    selector.asTerm match {
      case Inlined(_, _, Lambda(_, method)) =>
        '{
          FieldPath(${
            Expr(
              extractFields(method).mkString(".")
            )
          })
        }
      case _ => report.throwError(
        s"Expected inlined lambda, got ${selector.show}"
      )
    }
}
