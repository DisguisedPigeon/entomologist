import entomologist/internal/sql
import gleam/dynamic/decode
import gleam/list

import gleam/int
import gleam/json
import gleam/option.{type Option}
import gleam/result
import gleam/string
import pog

/// The logged level of an [error](#Error).
pub type Level {
  /// Action must be taken immediately.
  Alert
  /// Critical for fuctionality, but does not require immediate action.
  Critical
  /// Useful information for debugging.
  Debug
  /// System is unusable. Fix NOW.
  Emergency
  /// Severe issues, but core functionality is stable.
  ErrorLevel
  /// Informational messages.
  Info
  /// Normal but noticeable conditions.
  Notice
  /// Important issues. Does not alter most functionality.
  Warning
}

/// A kind of error.
///
/// Error [occurrences](#Occurrence) are grouped into errors, that share location and message.
/// This same structure is replicated on the database.
pub type ErrorLog {
  ErrorLog(
    /// The unique identifier of an error kind.
    id: Int,
    /// The message logged.
    message: String,
    /// The [error level](#Level) associated with the error.
    level: Level,
    /// The module that raised the error.
    module: String,
    /// The function that raised the error.
    function: String,
    /// The arity of the function.
    arity: Int,
    /// The file from which the error was raised.
    file: String,
    /// The line where the error occurred.
    line: Int,
    /// Whether the error was tagged as resolved.
    resolved: Bool,
    /// Timestamp of the last occurrence associated with this error
    last_occurrence: Int,
    /// Whether the error is snoozed.
    ///
    /// A snoozed error is hidden from the user, but it can be brought back and, as soon as it gets risen again, the error appears highlighted.
    ///
    /// See also:
    /// - [snooze](#snooze)
    /// - [wakeup](#wakeup)
    snoozed: Bool,
  )
}

/// A singular raise instance of an [error](#ErrorLog).
///
/// This type holds all non-general information relative to its error, like the timestamp or the specific data it returned.
pub type Occurrence {
  Occurrence(
    /// Unique identifier of this occurrence.
    ///
    /// This is shared between all error types, there is only one error for a given occurrence identifier.
    id: Int,
    /// Unique identifier of the associated error type.
    log_id: Option(Int),
    /// OS-relative timestamp of the occurrence in microseconds.
    ///
    /// > Since this is provided by the OS, It may be warped forwards or backwards by the user, so it may not be consistent.
    timestamp: Int,
    /// The body of the occurrence.
    ///
    /// Usually in json format.
    full_contents: Option(String),
  )
}

/// Configuring function for the erlang logger.
///
/// It registers the database as a logging handler and saves the DB connection
/// for later usage.
/// ## Example
///
/// ```gleam
/// let connection = pog.connect(config)
/// logging.configure(connection)
/// ```
@external(erlang, "entomologist_logger_ffi", "configure")
pub fn configure(connection: pog.Connection) -> Result(Nil, Nil)

/// Snoozes a log. It hides it until it happens again.
///
/// This is different than resolving because it is reversible and it highlights the error if it re-appears.
pub fn snooze(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.snooze_log(connection, log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "snooze"))
}

/// Reverses [snoozing](#snooze) for a given log.
pub fn wakeup(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.wake_up_log(connection, log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "wakeup"))
}

/// Resolves a log.
///
/// The log is frozen on the DB for future reference, but there is no way to unresolve.
/// A new log should be created if the error re-appears.
///
/// If the function fails, it returns a string describing the error.
///
/// If really needed, SQL can be used to toggle resolution.
pub fn resolve(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.resolve_log(connection, log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "resolve"))
}

/// Retrieves all logs neither snoozed nor resolved.
pub fn show(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "show"

  case sql.show(connection) {
    Ok(pog.Returned(_, l)) -> list.map(l, show_row_to_log) |> Ok

    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Retrieves all snoozed logs.
pub fn snoozed(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "snoozed"

  case sql.snoozed(connection) {
    Ok(pog.Returned(_, l)) ->
      list.map(l, snoozed_row_to_log)
      |> Ok
    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Retrieves all logs marked as solved.
///
/// This hides both active and snoozed logs.
pub fn solved(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "solved"

  case sql.solved(connection) {
    Ok(pog.Returned(_, l)) ->
      list.map(l, solved_row_to_log)
      |> Ok
    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Retrieves a log's occurrences.
pub fn occurrences(
  log_id: Int,
  connection: pog.Connection,
) -> Result(List(Occurrence), String) {
  let occurrences = sql.occurrences(connection, log_id)

  case occurrences {
    Ok(pog.Returned(_, l)) -> {
      let occurrence_encoder = fn(occurrence) {
        occurrences_row_to_occurrence(occurrence)
      }

      list.map(l, occurrence_encoder)
      |> Ok
    }

    Error(error) ->
      describe_error(error:, location: "show failed occurrences")
      |> Error
  }
}

/// Retrieves the data for a specific log.
pub fn log_data(
  log_id: Int,
  connection: pog.Connection,
) -> Result(ErrorLog, String) {
  let log = sql.logs(connection, log_id)

  case log {
    Ok(pog.Returned(1, [log])) -> {
      let level = sql_level_to_level(log.level)

      Ok(ErrorLog(
        id: log.id,
        message: log.message,
        level:,
        module: log.module,
        function: log.function,
        arity: log.arity,
        file: log.file,
        line: log.line,
        resolved: log.resolved,
        last_occurrence: log.last_occurrence,
        snoozed: log.snoozed,
      ))
    }

    Ok(v) -> panic as { "unexpected log amount: " <> string.inspect(v) }

    Error(error) ->
      describe_error(error:, location: "show failed occurrences")
      |> Error
  }
}

// ------------------- JSON encoders and decoders ------------------- //

pub fn encode_error_log(error_log: ErrorLog) -> json.Json {
  let ErrorLog(
    id:,
    message:,
    level:,
    module:,
    function:,
    arity:,
    file:,
    line:,
    resolved:,
    last_occurrence:,
    snoozed:,
  ) = error_log

  json.object([
    #("id", json.int(id)),
    #("message", json.string(message)),
    #("level", encode_level(level)),
    #("module", json.string(module)),
    #("function", json.string(function)),
    #("arity", json.int(arity)),
    #("file", json.string(file)),
    #("line", json.int(line)),
    #("resolved", json.bool(resolved)),
    #("last_occurrence", json.int(last_occurrence)),
    #("snoozed", json.bool(snoozed)),
  ])
}

pub fn level_decoder() -> decode.Decoder(Level) {
  use variant <- decode.then(decode.string)
  case variant {
    "alert" -> decode.success(Alert)
    "critical" -> decode.success(Critical)
    "debug" -> decode.success(Debug)
    "emergency" -> decode.success(Emergency)
    "error_level" -> decode.success(ErrorLevel)
    "info" -> decode.success(Info)
    "notice" -> decode.success(Notice)
    "warning" -> decode.success(Warning)
    _ -> decode.success(Warning)
  }
}

pub fn encode_level(level: Level) -> json.Json {
  case level {
    Alert -> json.string("alert")
    Critical -> json.string("critical")
    Debug -> json.string("debug")
    Emergency -> json.string("emergency")
    ErrorLevel -> json.string("error")
    Info -> json.string("info")
    Notice -> json.string("notice")
    Warning -> json.string("warning")
  }
}

// ------------------- sql autogenerated rows to ErrorLog ------------------- //

fn show_row_to_log(show_row: sql.ShowRow) -> ErrorLog {
  let level = show_row.level |> sql_level_to_level

  ErrorLog(
    id: show_row.id,
    message: show_row.message,
    level:,
    module: show_row.module,
    function: show_row.function,
    arity: show_row.arity,
    file: show_row.file,
    line: show_row.line,
    resolved: show_row.resolved,
    last_occurrence: show_row.last_occurrence,
    snoozed: show_row.snoozed,
  )
}

fn snoozed_row_to_log(snoozed_row: sql.SnoozedRow) -> ErrorLog {
  let level = snoozed_row.level |> sql_level_to_level

  ErrorLog(
    id: snoozed_row.id,
    message: snoozed_row.message,
    level:,
    module: snoozed_row.module,
    function: snoozed_row.function,
    arity: snoozed_row.arity,
    file: snoozed_row.file,
    line: snoozed_row.line,
    resolved: snoozed_row.resolved,
    last_occurrence: snoozed_row.last_occurrence,
    snoozed: snoozed_row.snoozed,
  )
}

fn solved_row_to_log(solved_row: sql.SolvedRow) -> ErrorLog {
  let level = solved_row.level |> sql_level_to_level

  ErrorLog(
    id: solved_row.id,
    message: solved_row.message,
    level:,
    module: solved_row.module,
    function: solved_row.function,
    arity: solved_row.arity,
    file: solved_row.file,
    line: solved_row.line,
    resolved: solved_row.resolved,
    last_occurrence: solved_row.last_occurrence,
    snoozed: solved_row.snoozed,
  )
}

fn sql_level_to_level(level: sql.Level) -> Level {
  case level {
    sql.Debug -> Debug
    sql.Info -> Info
    sql.Notice -> Notice
    sql.Warning -> Warning
    sql.Error -> ErrorLevel
    sql.Critical -> Critical
    sql.Alert -> Alert
    sql.Emergency -> Emergency
  }
}

pub fn encode_occurrence(occurrence: Occurrence) -> json.Json {
  let Occurrence(id:, log_id:, timestamp:, full_contents:) = occurrence
  json.object([
    #("id", json.int(id)),
    #("log_id", case log_id {
      option.None -> json.null()
      option.Some(value) -> json.int(value)
    }),
    #("timestamp", json.int(timestamp)),
    #("full_contents", case full_contents {
      option.None -> json.null()
      option.Some(value) -> json.string(value)
    }),
  ])
}

pub fn occurrence_decoder() -> decode.Decoder(Occurrence) {
  use id <- decode.field("id", decode.int)
  use log_id <- decode.field("log_id", decode.optional(decode.int))
  use timestamp <- decode.field("timestamp", decode.int)
  use full_contents <- decode.field(
    "full_contents",
    decode.optional(decode.string),
  )
  decode.success(Occurrence(id:, log_id:, timestamp:, full_contents:))
}

fn occurrences_row_to_occurrence(
  occurrences_row: sql.OccurrencesRow,
) -> Occurrence {
  Occurrence(
    id: occurrences_row.id,
    log_id: occurrences_row.log,
    timestamp: occurrences_row.timestamp,
    full_contents: occurrences_row.full_contents,
  )
}

fn describe_error(
  error error: pog.QueryError,
  location origin: String,
) -> String {
  "Query: "
  <> origin
  <> " - "
  <> case error {
    pog.ConstraintViolated(message:, constraint:, detail:) ->
      "Constraint violated: "
      <> constraint
      <> " - "
      <> message
      <> " - "
      <> detail
    pog.PostgresqlError(code:, name:, message:) ->
      "Postgres error: (" <> code <> ")" <> name <> " - " <> message
    pog.UnexpectedArgumentCount(expected:, got:) ->
      "THIS SHOULD NEVER HAPPEN - Argument quantity mismatch, expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected:, got:) ->
      "THIS SHOULD NEVER HAPPEN - Argument type mismatch, expected "
      <> expected
      <> ", got "
      <> got
    pog.UnexpectedResultType(decode_errors) ->
      "THIS SHOULD NEVER HAPPEN - Unable to decode result type: "
      <> string.inspect(decode_errors)
    pog.QueryTimeout -> "Database query timed out"
    pog.ConnectionUnavailable -> "Connection to database unavaliable"
  }
}
