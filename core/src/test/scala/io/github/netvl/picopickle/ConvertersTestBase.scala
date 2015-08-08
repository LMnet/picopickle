package io.github.netvl.picopickle

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable
import shapeless._

import org.scalatest.{MustMatchers, FreeSpec}

object ConvertersTestBase {
  object ComplexObjects {
    case class A(x: Int, y: String, z: B)
    case class B(a: Boolean, b: Double)
  }
}

trait ConvertersTestBase extends FreeSpec with MustMatchers with DefaultPickler {
  this: BackendComponent =>

  import backend._
  import conversionImplicits._
  import converters._
  import ConvertersTestBase.ComplexObjects._

  def backendName: String = "backend"

  "Converters" - {
    s"should convert to and from the $backendName representation" - {
      "null" in {
        `null`.isDefinedAt(makeNull) mustBe true
        (`null`.fromBackend(makeNull): Any) mustEqual (null: Any)

        `null`.isDefinedAt("something".toBackend) mustBe false

        `null`.toBackend(null) mustEqual (makeNull: Any)
      }

      "booleans" in {
        bool.isDefinedAt(true.toBackend) mustBe true
        bool.fromBackend(true.toBackend) mustBe true

        bool.isDefinedAt(false.toBackend) mustBe true
        bool.fromBackend(false.toBackend) mustBe false

        bool.isDefinedAt("something".toBackend) mustBe false

        bool.toBackend(true) mustBe true.toBackend
        bool.toBackend(false) mustBe false.toBackend
      }

      "numbers" in {
        num.isDefinedAt((1: Int).toBackend) mustBe true
        num.fromBackend((1: Int).toBackend) mustEqual 1

        num.isDefinedAt((10.1: Double).toBackend) mustBe true
        num.fromBackend((10.1: Double).toBackend) mustEqual 10.1

        num.isDefinedAt("something".toBackend) mustBe false

        num.toBackend(133: Long) mustEqual 133L.toBackend
        num.toBackend(42.2f) mustEqual 42.2f.toBackend
      }

      "strings" in {
        str.isDefinedAt("abcde".toBackend) mustBe true
        str.fromBackend("abcde".toBackend) mustEqual "abcde"

        str.isDefinedAt(12345.toBackend) mustBe false

        str.toBackend("hello") mustEqual "hello".toBackend
      }

      "objects" in {
        val m = obj {
          ("a" -> num.int) ::
          ("b" -> str) ::
          HNil
        }

        val t1 = Map("a" -> 123.toBackend, "b" -> "hello".toBackend).toBackend
        val t2 = Map("a" -> false.toBackend, "b" -> "hello".toBackend).toBackend
        val t3 = Map("b" -> "hello".toBackend).toBackend
        val t4 = Map("a" -> 342.toBackend, "b" -> "goodbye".toBackend, "c" -> false.toBackend).toBackend

        m.isDefinedAt(t1) mustBe true
        m.fromBackend(t1) mustEqual (123 :: "hello" :: HNil)

        m.isDefinedAt(t2) mustBe false
        m.isDefinedAt(t3) mustBe false

        m.isDefinedAt(t4) mustBe true
        m.fromBackend(t4) mustEqual (342 :: "goodbye" :: HNil)

        m.toBackend(234 :: "blabla" :: HNil) mustEqual Map("a" -> 234.toBackend, "b" -> "blabla".toBackend).toBackend
      }

      "complex classes" in {

        val bc: Converter.Id[B] = unlift(B.unapply) >>> obj {
          "a" -> bool ::
          "b" -> num.double ::
          HNil
        } >>> B.apply _

        val ac: Converter.Id[A] = unlift(A.unapply) >>> obj {
          "x" -> num.int ::
          "y" -> str ::
          "z" -> bc ::
          HNil
        } >>> A.apply _

        val s = A(
          10,
          "hello",
          B(true, 42.4)
        )

        val t = Map(
          "x" -> 10.toBackend,
          "y" -> "hello".toBackend,
          "z" -> Map(
            "a" -> true.toBackend,
            "b" -> 42.4.toBackend
          ).toBackend
        ).toBackend

        ac.isDefinedAt(t) mustBe true
        ac.fromBackend(t) mustEqual s

        ac.toBackend(s) mustEqual t
      }

      "homogeneous arrays" in {
        val cv = arr.as[Vector] of num.int
        val cs = arr.as[Set] of num.int

        val c1 = Vector(1.toBackend, 2.toBackend, 3.toBackend).toBackend
        cv.isDefinedAt(c1) mustBe true
        cv.fromBackend(c1) mustEqual Vector(1, 2, 3)
        cv.toBackend(Vector(1, 2, 3)) mustEqual c1
        cs.isDefinedAt(c1) mustBe true
        cs.fromBackend(c1) mustEqual Set(1, 2, 3)
        cs.toBackend(TreeSet(1, 2, 3)) mustEqual c1

        val c2 = Vector("a".toBackend, "e".toBackend).toBackend
        cv.isDefinedAt(c2) mustBe false
        cs.isDefinedAt(c2) mustBe false
      }

      "heterogenous arrays" in {
        val ma = arr(str :: num :: arr.as[Set].of(bool) :: HNil)
        val me = arr(HNil: HNil)

        val c1 = Vector("a".toBackend, 1.toBackend, Vector(false.toBackend, true.toBackend).toBackend).toBackend
        val r1 = "a" :: (1: Number) :: Set(true, false) :: HNil
        ma.isDefinedAt(c1) mustBe true
        ma.fromBackend(c1) mustEqual r1
        ma.toBackend("a" :: (1: Number) :: TreeSet(false, true) :: HNil) mustEqual c1

        val c2 = Vector("too small".toBackend).toBackend
        ma.isDefinedAt(c2) mustBe false

        val c3 = Vector(
          "too large".toBackend, 1.toBackend, Vector(true.toBackend).toBackend, "a".toBackend,
          34.toBackend, 22.9.toBackend, "zzz".toBackend
        ).toBackend
        val r3 = "too large" :: (1: Number) :: Set(true) :: HNil
        ma.isDefinedAt(c3) mustBe true
        ma.fromBackend(c3) mustEqual r3
        ma.toBackend(r3) mustEqual Vector("too large".toBackend, 1.toBackend, Vector(true.toBackend).toBackend).toBackend

        val c4 = Vector("incorrect types".toBackend, true.toBackend, Vector(false.toBackend).toBackend).toBackend
        ma.isDefinedAt(c4) mustBe false

        val c5 = Vector().toBackend  // empty
        me.isDefinedAt(c1) mustBe true
        me.fromBackend(c1) mustEqual HNil
        me.isDefinedAt(c5) mustBe true
        me.fromBackend(c5) mustEqual HNil
        me.toBackend(HNil) mustEqual c5
      }

      "object as map" in {
        val mm = obj.as[Map] to num.double
        val mt = obj.as[TreeMap] to num.double

        val t1 = Map.empty[String, BValue].toBackend
        mm.isDefinedAt(t1) mustBe true
        mm.fromBackend(t1) mustBe 'empty
        mm.toBackend(Map.empty) mustEqual t1
        mt.isDefinedAt(t1) mustBe true
        mt.fromBackend(t1) mustBe 'empty
        mt.toBackend(TreeMap.empty) mustEqual t1

        val t2 = Map[String, BValue]("a" -> 12.3.toBackend, "b" -> 13.4.toBackend).toBackend
        val s2m = Map("a" -> 12.3, "b" -> 13.4)
        val s2t = TreeMap("a" -> 12.3, "b" -> 13.4)
        mm.isDefinedAt(t2) mustBe true
        mm.fromBackend(t2) mustEqual s2m
        mm.toBackend(s2m) mustEqual t2
        mt.isDefinedAt(t2) mustBe true
        mt.fromBackend(t2) mustEqual s2t
        mt.toBackend(s2t) mustEqual t2

        val t3 = Map[String, BValue]("a" -> true.toBackend, "b" -> Vector(1.toBackend).toBackend).toBackend
        mm.isDefinedAt(t3) mustBe false
        mt.isDefinedAt(t3) mustBe false
      }

      "autoconverted classes" in {

        val m =
          {
            (k: String, vs: mutable.LinkedHashSet[A]) => k :: vs :: HNil
          }.tupled >> obj(
            "k" -> str ::
            "vs" -> arr.as[mutable.LinkedHashSet].of(converters.value[A]) ::
            HNil
          ) >> {
            case k :: vs :: HNil => (k, vs)
          }

        val t1 = Map(
          "k" -> "hello".toBackend,
          "vs" -> Vector(
            Map(
              "x" -> 10.toBackend,
              "y" -> "hello".toBackend,
              "z" -> Map(
                "a" -> true.toBackend,
                "b" -> 42.4.toBackend
              ).toBackend
            ).toBackend,
            Map(
              "x" -> 11.toBackend,
              "y" -> "bye".toBackend,
              "z" -> Map(
                "a" -> false.toBackend,
                "b" -> (-42.4).toBackend
              ).toBackend
            ).toBackend
          ).toBackend
        ).toBackend
        val r1 = ("hello", mutable.LinkedHashSet(A(10, "hello", B(true, 42.4)), A(11, "bye", B(false, -42.4))))
        m.isDefinedAt(t1) mustBe true
        m.fromBackend(t1) mustEqual r1
        m.toBackend(r1) mustEqual t1
      }
    }
  }
}
