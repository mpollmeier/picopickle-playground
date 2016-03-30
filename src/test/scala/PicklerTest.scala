import org.scalatest.Matchers
import org.scalatest.WordSpec
import io.github.netvl.picopickle.{BackendComponent, TypesComponent}
import io.github.netvl.picopickle.backends.collections.CollectionsPickler
import shapeless._
import shapeless.ops.hlist.IsHCons

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
  }

  "unwraps generically all value classes" in {
    trait ValueClassReaders { this: TypesComponent ⇒
      implicit def valueClassReader[
        ValueClass <: AnyVal,
        VCAsHList <: HList,
        Value
      ](implicit
        gen: Generic.Aux[ValueClass, VCAsHList],
        isHCons: IsHCons.Aux[VCAsHList, Value, HNil],
        valueReader: Reader[Value],
        eqv: (Value :: HNil) =:= VCAsHList): Reader[ValueClass] =
        valueReader.andThen { value ⇒
          gen.from(value :: HNil)
        }
    }

    trait ValueClassWriters { this: TypesComponent ⇒
      implicit def valueClassWriter[
        ValueClass <: AnyVal,
        VCAsHList <: HList,
        Value](implicit
               gen: Generic.Aux[ValueClass, VCAsHList],
               isHCons: IsHCons.Aux[VCAsHList, Value, HNil],
               valueWriter: Writer[Value]): Writer[ValueClass] =
        Writer(t ⇒ valueWriter.write(gen.to(t).head))
    }
    object ValueClassPickler extends CollectionsPickler with ValueClassWriters with ValueClassReaders
    import ValueClassPickler._

    write(WithValueClass(MyValueClass("hi"))) shouldBe Map("vc" → "hi")
    read[WithValueClass](Map("vc" → "hi")) shouldBe WithValueClass(MyValueClass("hi"))
  }

}

object CaseClasses {
  case class Simple(x: Int, y: String)

  case class WithOption(so: Option[String])

  case class MyValueClass(value: String) extends AnyVal
  case class WithValueClass(vc: MyValueClass)
}
