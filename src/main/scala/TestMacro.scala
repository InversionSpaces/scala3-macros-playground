import scala.quoted.*

object TestMacro:
    private def testImpl(using q: Quotes) = 
        import q.reflect.*
        val x = 5
        '{${Expr.apply(x)} + ${Expr.apply(x)}}

    inline def test = ${ testImpl }