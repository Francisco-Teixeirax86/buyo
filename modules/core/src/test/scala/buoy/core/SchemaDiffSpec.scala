package buoy.core

import zio.test.*

object SchemaDiffSpec extends ZIOSpecDefault:

  private def pathOf(ch: Change): String =
    ch match
      case Change.Added(p, _)          => p
      case Change.Removed(p, _)        => p
      case Change.TypeChanged(p, _, _) => p

  def spec: Spec[Any, Nothing] =
    suite("SchemaDiff")(
      test("identical shapes yield no changes and Additive severity") {
        val shape = Map("x" -> FieldType.Str, "y" -> FieldType.Num)
        val got   = SchemaDiff.diff(shape, shape)
        assertTrue(got.changes.isEmpty, got.severity == Severity.Additive)
      },
      test("a key only in candidate yields Added and Additive severity") {
        val baseline  = Map("x" -> FieldType.Str)
        val candidate = baseline + ("y" -> FieldType.Bool)
        val got       = SchemaDiff.diff(baseline, candidate)
        assertTrue(
          got.changes == List(Change.Added("y", FieldType.Bool)),
          got.severity == Severity.Additive
        )
      },
      test("a key only in baseline yields Removed and Breaking severity") {
        val baseline  = Map("x" -> FieldType.Str, "y" -> FieldType.Bool)
        val candidate = Map("x" -> FieldType.Str)
        val got       = SchemaDiff.diff(baseline, candidate)
        assertTrue(
          got.changes == List(Change.Removed("y", FieldType.Bool)),
          got.severity == Severity.Breaking
        )
      },
      test("a shared key with different types yields TypeChanged and Breaking severity") {
        val baseline  = Map("k" -> FieldType.Str)
        val candidate = Map("k" -> FieldType.Num)
        val got       = SchemaDiff.diff(baseline, candidate)
        assertTrue(
          got.changes == List(Change.TypeChanged("k", FieldType.Str, FieldType.Num)),
          got.severity == Severity.Breaking
        )
      },
      test("mixed Added, Removed, and TypeChanged are all present, Breaking, sorted by path") {
        val baseline = Map(
          "a" -> FieldType.Str,
          "b" -> FieldType.Str,
          "c" -> FieldType.Str
        )
        val candidate = Map(
          "b" -> FieldType.Num,
          "c" -> FieldType.Str,
          "d" -> FieldType.Str
        )
        val got          = SchemaDiff.diff(baseline, candidate)
        val pathsInOrder = got.changes.map(pathOf)
        assertTrue(
          got.severity == Severity.Breaking,
          got.changes.exists { case Change.Removed("a", FieldType.Str) => true; case _ => false },
          got.changes.exists {
            case Change.TypeChanged("b", FieldType.Str, FieldType.Num) => true; case _ => false
          },
          got.changes.exists { case Change.Added("d", FieldType.Str) => true; case _ => false },
          pathsInOrder == pathsInOrder.sorted,
          pathsInOrder == List("a", "b", "d")
        )
      },
      test("empty baseline against non-empty candidate yields only Added and Additive severity") {
        val baseline  = Map.empty[String, FieldType]
        val candidate = Map("only" -> FieldType.Null)
        val got       = SchemaDiff.diff(baseline, candidate)
        assertTrue(
          got.changes == List(Change.Added("only", FieldType.Null)),
          got.severity == Severity.Additive
        )
      },
      test("non-empty baseline against empty candidate yields only Removed and Breaking severity") {
        val baseline  = Map("only" -> FieldType.Obj)
        val candidate = Map.empty[String, FieldType]
        val got       = SchemaDiff.diff(baseline, candidate)
        assertTrue(
          got.changes == List(Change.Removed("only", FieldType.Obj)),
          got.severity == Severity.Breaking
        )
      }
    )
