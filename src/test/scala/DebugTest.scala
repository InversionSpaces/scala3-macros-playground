import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DebugTest extends AnyFlatSpec with Matchers:
  import Debug._

  "debug macro" should "work" in {
    val x = 5
    debug(1, x * x, x)
  }
