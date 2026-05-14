package buoy.core

/** Result of comparing a baseline shape map to a candidate shape map.
  *
  * `severity` is `Severity.Breaking` when `changes` contains any `Change.Removed` or
  * `Change.TypeChanged`, because removals or type changes can break consumers. It is
  * `Severity.Additive` when every change is a `Change.Added` only, or when `changes` is empty
  * (including identical baseline and candidate).
  */
final case class DiffResult(
    /** All structural differences between the two maps; empty when paths and types agree. */
    changes: List[Change],
    /** Drift impact derived from `changes`; see the `DiffResult` class comment for rules. */
    severity: Severity
)

/** Pure comparison of two flattened JSON shape maps (`path` to `FieldType`). */
object SchemaDiff:

  /** Computes structural differences between `baseline` and `candidate`.
    *
    * Keys only in `candidate` become `Change.Added`, keys only in `baseline` become
    * `Change.Removed`, and keys in both with differing `FieldType` values become
    * `Change.TypeChanged`.
    *
    * The returned `changes` list is sorted first by path (lexicographically), then by `Change`
    * variant ordinal, which matches declaration order: `Added` before `Removed` before
    * `TypeChanged`. Deterministic ordering keeps alerts, logs, and tests stable and avoids spurious
    * differences when comparing two diff results.
    */
  def diff(baseline: Map[String, FieldType], candidate: Map[String, FieldType]): DiffResult =
    val bKeys = baseline.keySet
    val cKeys = candidate.keySet

    val added =
      (cKeys -- bKeys).toList.sorted.map(path => Change.Added(path, candidate(path)))

    val removed =
      (bKeys -- cKeys).toList.sorted.map(path => Change.Removed(path, baseline(path)))

    val typeChanged =
      (bKeys & cKeys).toList.sorted.flatMap { path =>
        val from = baseline(path)
        val to   = candidate(path)
        if from == to then None
        else Some(Change.TypeChanged(path, from, to))
      }

    val unsorted = added ++ removed ++ typeChanged
    // Stable ordering: path is the primary key; ordinal tie-breaks if multiple rows per path were ever emitted.
    val changes  = unsorted.sortBy(ch => (changePath(ch), ch.ordinal))
    val severity =
      if changes.exists(isBreaking) then Severity.Breaking
      else Severity.Additive
    DiffResult(changes, severity)

  private def changePath(ch: Change): String =
    ch match
      case Change.Added(path, _)          => path
      case Change.Removed(path, _)        => path
      case Change.TypeChanged(path, _, _) => path

  private def isBreaking(ch: Change): Boolean =
    ch match
      case _: Change.Removed | _: Change.TypeChanged => true
      case _: Change.Added                           => false
