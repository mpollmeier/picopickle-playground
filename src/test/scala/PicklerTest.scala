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

      implicit val valueClassReader: Reader[MyValueClass] = Reader {
        case backend.Extract.String(s) ⇒ MyValueClass(s)
      }
    }
    import ValueClassPickler._

    write(WithValueClass(MyValueClass("hi"))) shouldBe Map("vc" → "hi")
    read[WithValueClass](Map("vc" → "hi")) shouldBe WithValueClass(MyValueClass("hi"))

    // TODO: ensure still works
    // write(WithValueClass(10, MyValueClass("hi"))) shouldBe Map("i" → 10, "vc" → "hi")
    // read[WithValueClass](Map("i" → 10, "vc" → "hi")) shouldBe WithValueClass(10, MyValueClass("hi"))
  }

  "unwraps generically all value classes" in {
    trait ValueClassWriters { this: TypesComponent ⇒
      import shapeless._
      implicit def valueClassWriter[ValueClass <: AnyVal, Value](implicit
        gen: Generic.Aux[ValueClass, Value :: HNil],
        rw: Writer[Value]): Writer[ValueClass] =
        Writer(t ⇒ gen.to(t) match {
                 case r :: HNil ⇒ rw.write(r)
               })
    }
    trait ValueClassReaders { this: TypesComponent ⇒
      import shapeless._
      implicit def valueClassReader[ValueClass <: AnyVal, Value](implicit
                                                      gen: Generic.Aux[ValueClass, Value :: HNil],
                                                    rr: Reader[Value]): Reader[ValueClass] =
        rr.andThen(r ⇒ gen.from(r :: HNil))
    }
    object ValueClassPickler extends CollectionsPickler with ValueClassWriters// with ValueClassReaders
    import ValueClassPickler._

    println(write(WithValueClass(MyValueClass("hi"))))
    println(read[WithValueClass](Map("vc" → Map("value" → "hi")))) // TODO: get rid of nested Map

    write(WithValueClass(MyValueClass("hi"))) shouldBe Map("vc" → "hi")
    read[WithValueClass](Map("vc" → "hi")) shouldBe WithValueClass(MyValueClass("hi"))

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
