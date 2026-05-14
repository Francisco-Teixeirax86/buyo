package buoy.core

import io.circe.{Json, JsonObject}

/** Pure JSON shape extraction: maps dot-paths to `FieldType`; payload values are not retained. */
object ShapeExtractor:

  /** Walks a JSON value and returns a flat path → `FieldType` map.
    *
    * The top-level JSON record is not emitted as its own path; only its children (and deeper paths)
    * are present in the result. Top-level arrays or scalars yield an empty map.
    */
  def extract(json: Json): Map[String, FieldType] =
    json.asObject match
      case Some(fields) => extractObjectChildren(fields, prefix = "")
      case None         => Map.empty

  private def extractObjectChildren(fields: JsonObject, prefix: String): Map[String, FieldType] =
    fields.toList.foldLeft(Map.empty[String, FieldType]) { case (acc, (key, child)) =>
      val path =
        if prefix.isEmpty then key
        else s"$prefix.$key"
      acc ++ mergeValuesAt(path, List(child))
    }

  /** Merges one or more JSON values observed under the same dot-path (e.g. same field across array
    * elements).
    */
  private def mergeValuesAt(path: String, values: List[Json]): Map[String, FieldType] =
    values match
      case Nil                            => Map.empty
      case _ if values.forall(_.isObject) =>
        val objs = values.flatMap(_.asObject)
        Map(path -> FieldType.Obj) ++ mergeObjectChildren(path, objs)
      case _ if values.forall(_.isArray) =>
        val combined = values.flatMap(_.asArray.fold(List.empty[Json])(_.toList)).toVector
        arrayShape(path, combined)
      case _ if values.forall(j => primitiveFieldType(j).isDefined) =>
        val prims    = values.flatMap(primitiveFieldType)
        val distinct = prims.distinct
        if distinct.length == 1 then Map(path -> distinct.head)
        else Map(path                         -> FieldType.Union(prims.toSet))
      case _ =>
        Map(path -> FieldType.Union(values.map(directFieldType).toSet))

  /** Unions keys from every object in `objs` under `parentPath` (dot-separated); used for object
    * arrays.
    */
  private def mergeObjectChildren(
      parentPath: String,
      objs: List[JsonObject]
  ): Map[String, FieldType] =
    // For arrays of objects we call this with parentPath like "items[]". Keys must be unioned across *all*
    // elements so optional fields omitted as null in one element but present in another still surface.
    val keys = objs.iterator.flatMap(_.keys).toSet
    keys.foldLeft(Map.empty[String, FieldType]) { (acc, key) =>
      val childPath =
        if parentPath.isEmpty then key
        else s"$parentPath.$key"
      val childValues = objs.flatMap(_.apply(key))
      acc ++ mergeValuesAt(childPath, childValues)
    }

  private def arrayShape(path: String, elems: Vector[Json]): Map[String, FieldType] =
    if elems.isEmpty then Map(path -> FieldType.Arr(FieldType.Null))
    else if elems.forall(_.isObject) then
      val objs = elems.flatMap(_.asObject).toList
      Map(path -> FieldType.Arr(FieldType.Obj)) ++ mergeObjectChildren(
        parentPath = s"$path[]",
        objs
      )
    else Map(path -> FieldType.Arr(fieldTypeOfArrayElements(elems)))

  private def primitiveFieldType(json: Json): Option[FieldType] =
    if json.isNull then Some(FieldType.Null)
    else if json.isBoolean then Some(FieldType.Bool)
    else if json.isNumber then Some(FieldType.Num)
    else if json.isString then Some(FieldType.Str)
    else None

  /** Classifies a single JSON value for drift (object/array structure, or a leaf primitive). */
  private def directFieldType(json: Json): FieldType =
    primitiveFieldType(json).getOrElse:
      if json.isObject then FieldType.Obj
      else if json.isArray then
        val elems = json.asArray.fold(Vector.empty[Json])(_.toVector)
        // Delegate array element unification to `fieldTypeOfArrayElements` so we do not duplicate
        // the “all objects / all primitives / mixed” matrix that also applies to sibling elements.
        FieldType.Arr(fieldTypeOfArrayElements(elems))
      else FieldType.Null

  /** Unifies several JSON values that occupy the same structural role (e.g. array elements) into
    * one `FieldType`.
    *
    * This is the single place that decides how heterogeneous sibling values collapse (all-objects →
    * `FieldType.Obj`, all-primitives → one primitive or `FieldType.Union`, otherwise
    * `FieldType.Union` of `directFieldType`). Nested arrays recurse through `directFieldType`
    * without re-stating that matrix.
    */
  private def fieldTypeOfArrayElements(elems: Vector[Json]): FieldType =
    if elems.isEmpty then FieldType.Null
    else if elems.forall(_.isObject) then FieldType.Obj
    else if elems.forall(j => primitiveFieldType(j).isDefined) then
      val ts = elems.flatMap(primitiveFieldType).toSet
      if ts.size == 1 then ts.head
      else FieldType.Union(ts)
    else
      val ts = elems.map(directFieldType).toSet
      if ts.size == 1 then ts.head
      else FieldType.Union(ts)
