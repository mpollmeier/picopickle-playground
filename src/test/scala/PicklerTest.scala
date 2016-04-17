import org.scalatest.Matchers
import org.scalatest.WordSpec
import io.github.netvl.picopickle.key
import io.github.netvl.picopickle.{BackendComponent, TypesComponent, ValueClassReaderWritersComponent}
import io.github.netvl.picopickle.backends.collections.CollectionsPickler
import shapeless._
import shapeless.ops.hlist.IsHCons
import java.lang.{Long ⇒ JLong, Double ⇒ JDouble}

object MyPickler extends CollectionsPickler with ValueClassReaderWritersComponent

class PicklerTest extends WordSpec with Matchers {
  import CaseClasses._

  "simple case class" in {
    import MyPickler._
    write(Simple(10, "hi")) shouldBe Map("x" → 10, "y" → "hi")
    read[Simple](Map("x" → 10, "y" → "hi")) shouldBe Simple(10, "hi")
  }

  "handles java types" in {
    import MyPickler._
    // TODO: implement
    // println(write(new Integer(12)))

    // val cc = WithJavaTypes(new Integer(12), new JLong(22l), new JDouble(3.3d))
    // write(cc) shouldBe Map("i" → 12, "l" → 22, "d" -> 3.3d)
    // read[Simple](Map("i" → 12, "l" → 22, "d" -> 3.3d)) shouldBe cc
  }

  "unwraps Some and None" in {
    import MyPickler._
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

  "unwraps value classes" in {
    import MyPickler._

    write(WithValueClass(MyValueClass("hi"))) shouldBe Map("vc" → "hi")
    read[WithValueClass](Map("vc" → "hi")) shouldBe WithValueClass(MyValueClass("hi"))
  }

  "gets result type from Writer" ignore {
    // import CollectionsPickler._

    // def writeWithLabel[Value](value: Value)(implicit w: Writer[Value]) = {
    //   val writtenValue = write(value)
    //   val label: String = value match {
    //     case a: WithLabel ⇒ a.label
    //     case _        ⇒ value.getClass.getSimpleName
    //   }
    //   (writtenValue, label)
    // }

    // writeWithLabel(CCWithLabel(10)) shouldBe ((Map("i" -> 10), "my custom label"))
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

  "handles labels" in {
    import CollectionsPickler._

    def writeWithLabel[Value](value: Value)(implicit w: Writer[Value]) = {
      val writtenValue = write(value)
      val label: String = value match {
        case a: WithLabel ⇒ a.label
        case _        ⇒ value.getClass.getSimpleName
      }
      (writtenValue, label)
    }

    writeWithLabel(CCWithLabel(10)) shouldBe ((Map("i" -> 10), "my custom label"))
  }

  "converts to java classes" in {
    import CollectionsPickler._
    // TODO: custom backend implementation needed?
    // idea: only support a given set of base types: Integer, Long, java.util.Map etc.

    val a: backend.BValue = write(WithMap(5, Map("key" -> "value")))
    val b = a.asInstanceOf[Map[String, Any]]
    println(b)
    println(b("x").getClass) //java.lang.Integer -> fine
    println(b("m").getClass) //scala.Map -> not fine
  }

  "allows renaming of keys" in {
    import MyPickler._

    write(Renamed(10, "hi")) shouldBe Map("x" → 10, "renamed" → "hi")
    read[Renamed](Map("x" → 10, "renamed" → "hi")) shouldBe Renamed(10, "hi")
  }

  "handles Ids" in {
    trait IdPicklers { this: BackendComponent with TypesComponent ⇒
      implicit def idWriter[IdType](implicit w: Writer[IdType]): Writer[Id[IdType]] = Writer {
        case Id(value) =>
          // println("XXXXXXXXXXXXXXX yay, writer getting invoked")
          // TODO: get rid of cast
          Map("__id" -> w.write(value)).asInstanceOf[backend.BValue]
      }

      implicit def idReader[IdType](implicit r: Reader[IdType]): Reader[Id[IdType]] = Reader {
        case idValueMap: Map[_,_] =>
            // println("YYYYYYYYYYYYYYY yay, reader getting invoked")
          // TODO: get rid of cast
          Id[IdType](idValueMap.asInstanceOf[Map[String, IdType]]("__id"))
      }
    }

    object MyPickler extends CollectionsPickler with IdPicklers
    import MyPickler._

    write(Id("id value")) shouldBe Map("__id" -> "id value")
    read[Id[String]](Map("__id" -> "id value")) shouldBe Id("id value")

    write(WrappedId(42, Id(Long.MaxValue))) shouldBe Map("i" -> 42, "id" -> Map("__id" -> Long.MaxValue))
    read[WrappedId](Map("i" -> 42, "id" -> Map("__id" -> Long.MaxValue))) shouldBe WrappedId(42, Id(Long.MaxValue))
  }

}

object CaseClasses {
  case class Simple(x: Int, y: String)

  case class Renamed(x: Int, @key("renamed") y: String)

  case class WithOption(so: Option[String])

  case class WithJavaTypes(i: Integer, l: JLong, d: JDouble)

  case class MyValueClass(value: String) extends AnyVal
  case class WithValueClass(vc: MyValueClass)

  case class Nested(x: Int, wo: WithOption)

  case class WithMap(x: Int, m: Map[String, String])

  trait WithLabel {
    def label(): String
  }
  case class CCWithLabel(i: Int) extends WithLabel {
    def label = "my custom label"
  }

  case class Id[IdType](value: IdType)
  case class WrappedId(i: Int, id: Id[Long])
}
