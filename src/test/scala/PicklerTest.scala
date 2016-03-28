import org.scalatest.Matchers
import org.scalatest.WordSpec
import io.github.netvl.picopickle.{BackendComponent, TypesComponent}
import io.github.netvl.picopickle.backends.collections.CollectionsPickler

class PicklerTest extends WordSpec with Matchers {
  import CaseClasses._

  "simple case class" in {
    import CollectionsPickler._
    write(Simple(10, "hi")) shouldBe Map("x" → 10, "y" → "hi")
    read[Simple](Map("x" → 10, "y" → "hi")) shouldBe Simple(10, "hi")
  }

  "unwraps Some and None" in {
    import CollectionsPickler._
    write(WithOption(Some("hi"))) shouldBe Map("so" → "hi")
    write(WithOption(None)) shouldBe Map.empty

    read[WithOption](Map("so" → "hi")) shouldBe WithOption(Some("hi"))
    read[WithOption](Map.empty) shouldBe WithOption(None)
  }

  "custom pickler" in {
    object DefinedByIntPickler extends CollectionsPickler {
      implicit val definedByIntWriter: Writer[Simple] = Writer {
        case Simple(x, _) ⇒ backend.makeNumber(x)
      }

      implicit val definedByIntReader: Reader[Simple] = Reader {
        case backend.Extract.Number(x) ⇒ Simple(x.intValue(), x.intValue().toString)
      }
    }
    import DefinedByIntPickler._

    write(Simple(10, "hi")) shouldBe 10
    read[Simple](10) shouldBe Simple(10, "10")
  }

  "unwraps AnyVal" in {
    import CollectionsPickler._
    write(WithValueClass(10, MyValueClass("hi"))) shouldBe Map("i" → 10, "vc" → "hi")
    read[WithValueClass](Map("i" → 10, "vc" → "hi")) shouldBe WithValueClass(10, MyValueClass("hi"))
  }

}

object CaseClasses {
  case class Simple(x: Int, y: String)

  case class WithOption(so: Option[String])

  case class MyValueClass(s: String) extends AnyVal
  case class WithValueClass(i: Int, vc: MyValueClass)
}
