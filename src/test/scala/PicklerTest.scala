import io.github.netvl.picopickle.backends.collections.CollectionsPickler._
import org.scalatest.Matchers
import org.scalatest.WordSpec

class PicklerTest extends WordSpec with Matchers {
  import CaseClasses._

  "simple case class" in {
    write(Simple(10, "hi")) shouldBe Map("x" -> 10, "y" -> "hi")
    read[Simple](Map("x" -> 10, "y" -> "hi")) shouldBe Simple(10, "hi")
  }

  "unwraps Some and None" in {
    write(WithOption(Some("hi"))) shouldBe Map("so" -> "hi")
    write(WithOption(None)) shouldBe Map.empty

    read[WithOption](Map("so" -> "hi")) shouldBe WithOption(Some("hi"))
    read[WithOption](Map.empty) shouldBe WithOption(None)
  }

  "unwraps AnyVal" in {
    write(WithValueClass(10, MyValueClass("hi"))) shouldBe Map("i" -> 10, "vc" -> "hi")
    read[WithValueClass](Map("i" -> 10, "vc" -> "hi")) shouldBe WithValueClass(10, MyValueClass("hi"))
  }

}

object CaseClasses {
  case class Simple(x: Int, y: String)

  case class WithOption(so: Option[String])

  case class MyValueClass(s: String) extends AnyVal
  case class WithValueClass(i: Int, vc: MyValueClass)
}
