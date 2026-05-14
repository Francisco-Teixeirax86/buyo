package buoy.core

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat

/** Canonical string form and SHA-256 fingerprint of a [[FieldType]] shape map. */
object Fingerprint:

  /** Returns a deterministic, newline-separated encoding of [[shape]].
    *
    * Keys are sorted lexicographically so two maps that differ only in iteration order (e.g. two
    * `HashMap` instances with the same entries) still produce the same string. Each line is `path:`
    * followed by the serialized type at that path.
    */
  def normalize(shape: Map[String, FieldType]): String =
    shape.keys.toList.sorted
      .map(path => s"$path:${serialize(shape(path))}")
      .mkString("\n")

  /** Computes the SHA-256 fingerprint of the canonical [[normalize]]d shape.
    *
    * Two equal shape maps always yield the same [[Hash]], even if the original JSON object key
    * order differed, because [[normalize]] sorts paths and [[serialize]] sorts [[FieldType.Union]]
    * members (see [[serialize]] for unions).
    */
  def compute(shape: Map[String, FieldType]): Hash =
    val normalized = normalize(shape)
    val digest     =
      MessageDigest.getInstance("SHA-256").digest(normalized.getBytes(StandardCharsets.UTF_8))
    Hash(HexFormat.of().withLowerCase().formatHex(digest))

  private def serialize(tpe: FieldType): String =
    tpe match
      case FieldType.Str          => "String"
      case FieldType.Num          => "Number"
      case FieldType.Bool         => "Boolean"
      case FieldType.Null         => "Null"
      case FieldType.Obj          => "Object"
      case FieldType.Arr(element) => s"Array<${serialize(element)}>"
      case FieldType.Union(types) =>
        // Set iteration order is undefined; sort serialized members so the same union always prints the same way.
        val members = types.map(serialize).toList.sorted
        s"Union<${members.mkString("|")}>"
