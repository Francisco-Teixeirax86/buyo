package buoy.core

import scala.collection.immutable.HashMap

import zio.test.*

object FingerprintSpec extends ZIOSpecDefault:

  import Hash.value

  private val shapeA: Map[String, FieldType] = Map(
    "a" -> FieldType.Str,
    "b" -> FieldType.Num
  )

  def spec: Spec[Any, Nothing] =
    suite("Fingerprint")(
      test("the same shape map always produces the same Hash") {
        val h1 = Fingerprint.compute(shapeA)
        val h2 = Fingerprint.compute(shapeA)
        assertTrue(h1.value == h2.value)
      },
      test("different shapes produce different Hashes") {
        val other = Map("a" -> FieldType.Bool)
        val h1    = Fingerprint.compute(shapeA)
        val h2    = Fingerprint.compute(other)
        assertTrue(h1.value != h2.value)
      },
      test("key ordering in the input map does not affect the Hash") {
        val m1 = HashMap("z" -> FieldType.Str, "a" -> FieldType.Num)
        val m2 = HashMap("a" -> FieldType.Num, "z" -> FieldType.Str)
        val h1 = Fingerprint.compute(m1)
        val h2 = Fingerprint.compute(m2)
        assertTrue(h1.value == h2.value)
      },
      test("Union serialization is stable regardless of Set construction order") {
        val u1 = FieldType.Union(Set(FieldType.Str, FieldType.Num))
        val u2 = FieldType.Union(Set(FieldType.Num, FieldType.Str))
        val m1 = Map("p" -> u1)
        val m2 = Map("p" -> u2)
        assertTrue(
          Fingerprint.normalize(m1) == Fingerprint.normalize(m2),
          Fingerprint.compute(m1).value == Fingerprint.compute(m2).value
        )
      }
    )
