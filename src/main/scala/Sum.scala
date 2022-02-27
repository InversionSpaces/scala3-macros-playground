import scala.quoted.*
import scala.compiletime.*

object Sum:
  inline def sumNow(inline nums: Int*): Int =
    ${ sumNowImpl('nums) }

  def sumNowImpl(nums: Expr[Seq[Int]])(using q: Quotes): Expr[Int] =
    nums match {
      case Varargs(nums) =>
        Expr(nums.map(_.valueOrAbort).sum)
      case _ =>
        import q.reflect.*
        report.throwError("Varargs expected")
    }
