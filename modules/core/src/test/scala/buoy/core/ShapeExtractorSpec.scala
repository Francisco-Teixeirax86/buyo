package buoy.core

import io.circe.Json
import zio.test.*

object ShapeExtractorSpec extends ZIOSpecDefault:

  def spec: Spec[Any, Nothing] =
    suite("ShapeExtractor")(
      test("maps primitive JSON values to the correct FieldType") {
        val json = Json.obj(
          "s" -> Json.fromString("hello"),
          "n" -> Json.fromInt(42),
          "b" -> Json.True,
          "z" -> Json.Null
        )
        val got = ShapeExtractor.extract(json)
        assertTrue(
          got.get("s").contains(FieldType.Str),
          got.get("n").contains(FieldType.Num),
          got.get("b").contains(FieldType.Bool),
          got.get("z").contains(FieldType.Null)
        )
      },
      test("uses dot notation for nested object paths") {
        val json = Json.obj(
          "outer" -> Json.obj(
            "inner" -> Json.fromBoolean(false)
          )
        )
        val got = ShapeExtractor.extract(json)
        assertTrue(
          got.get("outer").contains(FieldType.Obj),
          got.get("outer.inner").contains(FieldType.Bool)
        )
      },
      test("for an array of objects, unions keys across all elements") {
        val json = Json.obj(
          "items" -> Json.arr(
            Json.obj("a" -> Json.fromInt(1)),
            Json.obj("b" -> Json.fromInt(2))
          )
        )
        val got = ShapeExtractor.extract(json)
        assertTrue(
          got.get("items").contains(FieldType.Arr(FieldType.Obj)),
          got.get("items[].a").contains(FieldType.Num),
          got.get("items[].b").contains(FieldType.Num)
        )
      },
      test("empty JSON array extracts as Arr(Null)") {
        val json = Json.obj("x" -> Json.arr())
        val got  = ShapeExtractor.extract(json)
        assertTrue(got.get("x").contains(FieldType.Arr(FieldType.Null)))
      },
      test("mixed-type JSON array elements extract as Union inside Arr") {
        val json = Json.obj(
          "x" -> Json.arr(Json.fromInt(1), Json.fromString("a"))
        )
        val got = ShapeExtractor.extract(json)
        assertTrue(
          got
            .get("x")
            .contains(
              FieldType.Arr(FieldType.Union(Set(FieldType.Num, FieldType.Str)))
            )
        )
      }
    )
