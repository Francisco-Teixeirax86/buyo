package buoy.core

import java.time.Instant
import java.util.UUID

import io.circe.Json

/** Identifies a tenant for routing; carried in the URL path and is not a secret. */
opaque type AccountId = String

/** Constructs and unwraps [[AccountId]] values. */
object AccountId:

  /** Wraps the raw string routing key as an [[AccountId]]. */
  def apply(value: String): AccountId = value

  /** The underlying string routing key. */
  extension (id: AccountId) def value: String = id

/** Uniquely identifies a single ingested webhook event. */
opaque type EventId = UUID

/** Constructs and unwraps [[EventId]] values. */
object EventId:

  /** Wraps a [[UUID]] as an [[EventId]]. */
  def apply(id: UUID): EventId = id

  /** The underlying unique identifier. */
  extension (id: EventId) def value: UUID = id

/** Identifies a retry-queue job for one consumer delivery attempt chain. */
opaque type JobId = UUID

/** Constructs and unwraps [[JobId]] values. */
object JobId:

  /** Wraps a [[UUID]] as a [[JobId]]. */
  def apply(id: UUID): JobId = id

  /** The underlying unique identifier. */
  extension (id: JobId) def value: UUID = id

/** Hex or other string form of a content hash (e.g. SHA-256 of a shape or idempotency material). */
opaque type Hash = String

/** Constructs and unwraps [[Hash]] values. */
object Hash:

  /** Wraps the raw hash string as a [[Hash]]. */
  def apply(value: String): Hash = value

  /** The underlying hash string. */
  extension (h: Hash) def value: String = h

/** Describes the type of a single field in a flattened JSON path → type shape map. */
enum FieldType:
  /** JSON string primitive at this path. */
  case Str

  /** JSON number primitive at this path. */
  case Num

  /** JSON boolean primitive at this path. */
  case Bool

  /** JSON null at this path. */
  case Null

  /** JSON object at this path (structure of children is represented by child paths). */
  case Obj

  /** JSON array whose elements have the given [[elementType]]. */
  case Arr(elementType: FieldType)

  /** One of several possible shapes observed at this path (e.g. union of object layouts). */
  case Union(types: Set[FieldType])

/** An incoming webhook as captured on ingest, before async processing (shape, fan-out, retries). */
final case class RawEvent(
    /** Stable identifier for this event row. */
    id: EventId,
    /** Tenant routing key from the URL path. */
    accountId: AccountId,
    /** Provider slug, e.g. `"stripe"` or `"github"`; identifies which integration produced the
      * payload.
      */
    provider: String,
    /** Logical event name used for fingerprinting and routing (Stripe: from body; GitHub: from
      * header).
      */
    // Extraction of this value from headers or body is the proxy module's responsibility, not core.
    eventType: String,
    /** Parsed JSON body as a [[Json]] value tree (structure used by shape extraction; values may be
      * traversed for typing only).
      */
    payload: Json,
    /** Exact request body bytes as received; must feed the idempotency key and any later crypto or
      * replay checks.
      */
    rawBody: Array[Byte],
    /** Idempotency key: `sha256(accountId + ":" + provider + ":" + rawBody)` (computed by proxy
      * before async work).
      */
    // All three parts are required so keys never collide across tenants, providers, or distinct byte payloads.
    idempotencyKey: String,
    /** Wall-clock time when the proxy accepted the request. */
    receivedAt: Instant
)

/** Key for looking up known fingerprints in memory or in the fingerprint store. */
final case class CacheKey(
    /** Tenant scope; fingerprints are never shared across accounts. */
    accountId: AccountId,
    /** Provider slug, same convention as [[RawEvent.provider]]. */
    provider: String,
    /** Same logical event classification as [[RawEvent.eventType]]. */
    eventType: String
)

/** A recorded shape fingerprint at a point in time (baseline or drift candidate). */
final case class FingerprintSnapshot(
    /** SHA-256 (or project-standard) hash of the canonical shape encoding. */
    hash: Hash,
    /** Flat map from JSON path (e.g. `data.object.id`) to [[FieldType]]. */
    shape: Map[String, FieldType],
    /** Whether this snapshot is the accepted baseline for drift policy comparisons. */
    isBaseline: Boolean,
    /** When this shape was first observed or promoted. */
    detectedAt: Instant
)

/** One structural difference between two shape maps (typically baseline vs candidate). */
enum Change:
  /** A path present in the new shape but absent in the old. */
  case Added(path: String, tpe: FieldType)

  /** A path present in the old shape but absent in the new. */
  case Removed(path: String, tpe: FieldType)

  /** A path exists in both but [[FieldType]] differs. */
  case TypeChanged(path: String, from: FieldType, to: FieldType)

/** Coarse impact class for a set of [[Change]] values when evaluating drift. */
enum Severity:
  /** Chosen when any change is [[Change.Removed]] or [[Change.TypeChanged]] (consumers may break).
    */
  case Breaking

  /** Chosen when every change is [[Change.Added]] only (backward compatible). */
  case Additive

/** Policy for how to treat unknown or drifted schema fingerprints after WAL append. */
enum OnDrift:
  /** Deliver to consumers and emit a drift alert. */
  case Warn

  /** When drift is detected: skip fan-out, enqueue dead letter, emit alert.
    *
    * This policy never changes the HTTP response the provider already received: after a successful
    * WAL append the proxy responds `200` regardless of [[Block]]; blocking applies only to
    * downstream delivery.
    */
  case Block

  /** Deliver to consumers and suppress drift alerts. */
  case Ignore

/** Backoff strategy between retry attempts for a single consumer delivery. */
enum Backoff:
  /** Increase delay multiplicatively between attempts. */
  case Exponential

  /** Increase delay by a fixed increment between attempts. */
  case Linear

  /** Use the same delay before every retry after the first failure. */
  case Fixed

/** Configuration for delivering webhook payloads to one downstream HTTP consumer. */
final case class Consumer(
    /** Human-readable label for logs and metrics. */
    name: String,
    /** Target URL for POST fan-out. */
    url: String,
    /** Provider slugs this consumer accepts; empty list can mean "all" depending on proxy wiring.
      */
    providers: List[String],
    /** Per-request timeout in milliseconds applied inside the fan-out loop for this consumer only.
      */
    timeoutMs: Int,
    /** Total delivery attempts including the first try; not "retries after first". */
    maxAttempts: Int,
    /** How long to wait before attempt n+1 after attempt n fails. */
    backoff: Backoff
)

/** Lifecycle state of a row in the retry queue. */
enum RetryStatus:
  /** Waiting to be claimed or scheduled for the next delivery try. */
  case Pending

  /** Successfully delivered or explicitly completed; no further dequeue. */
  case Done

  /** Exhausted permitted delivery attempts or otherwise terminal; kept for inspection. */
  case Dead

/** One retryable unit of work: redeliver one event to one consumer URL after a failure. */
final case class RetryJob(
    /** Primary key for this retry row. */
    id: JobId,
    /** Which WAL event to redeliver. */
    eventId: EventId,
    /** The failed consumer's URL; retries are never merged across consumers. */
    consumerUrl: String,
    /** 1-based attempt index for the next delivery try. */
    attempt: Int,
    /** Earliest instant the worker may dequeue this job. */
    nextAttemptAt: Instant,
    /** Last failure reason if any (timeout, 5xx, circuit open, etc.). */
    lastError: Option[String],
    /** Queue lifecycle; workers transition [[Pending]] → [[Done]] or [[Dead]]. */
    status: RetryStatus
)
