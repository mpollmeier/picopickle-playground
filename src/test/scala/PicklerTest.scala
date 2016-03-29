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

  "unwraps specific value class" in {
    object ValueClassPickler extends CollectionsPickler {
      implicit def valueClassWriter: Writer[MyValueClass] = Writer {
        case MyValueClass(value) ⇒ backend.makeString(value)
      }

      // implicit val valueClassReader: Reader[MyValueClass] = Reader {
      //   case backend.Extract.String(s) ⇒ MyValueClass(s)
      // }
      // TODO: compiler doesn't hook this up correctly... it falls back to the generic case class pickler which creates a Map
      // implicit val valueClassWriter: Writer[AnyVal] = Writer {
      //   case MyValueClass(value) ⇒ backend.makeString(value)
      // }

      // implicit val valueClassReader: Reader[MyValueClass] = Reader {
      //   case backend.Extract.String(s) ⇒ MyValueClass(s)
      // }
    }
    import ValueClassPickler._

    val a = implicitly[Writer[MyValueClass]]
    // val b = implicitly[Writer[MyValueClass]]
    // println(a)
    // println(a.getClass)
    println(write(WithValueClass(MyValueClass("hi"))))

    // write(WithValueClass(MyValueClass("hi"))) shouldBe Map("vc" → "hi")
    // read[WithValueClass](Map("vc" → "hi")) shouldBe WithValueClass(MyValueClass("hi"))

    // TODO: ensure still works
    // write(WithValueClass(10, MyValueClass("hi"))) shouldBe Map("i" → 10, "vc" → "hi")
    // read[WithValueClass](Map("i" → 10, "vc" → "hi")) shouldBe WithValueClass(10, MyValueClass("hi"))
  }

}

object CaseClasses {
  case class Simple(x: Int, y: String)

  case class WithOption(so: Option[String])

  case class MyValueClass(value: String) extends AnyVal
  case class WithValueClass(vc: MyValueClass)
  // TODO:
  // case class WithValueClass(i: Int, vc: MyValueClass)
}
