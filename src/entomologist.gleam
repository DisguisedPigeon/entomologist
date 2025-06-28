import entomologist/internal/sql
import gleam/dynamic/decode
import gleam/list

import gleam/int
import gleam/json
import gleam/option.{type Option}
import gleam/result
import gleam/string
import pog

pub type Level {
  Alert
  Critical
  Debug
  Emergency
  ErrorLevel
  Info
  Notice
  Warning
}

pub type ErrorLog {
  ErrorLog(
    id: Int,
    message: String,
    level: Level,
    module: String,
    function: String,
    arity: Int,
    file: String,
    line: Int,
    resolved: Bool,
    last_occurrence: Int,
    snoozed: Bool,
  )
}

pub type Occurrence {
  Occurrence(
    id: Int,
    log_id: Option(Int),
    timestamp: Int,
    full_contents: Option(String),
  )
}

/// Configuring function for the erlang logger.
///
/// It registers the database as a logging handler and saves the DB connection
/// for later usage.
@external(erlang, "entomologist_logger_ffi", "configure")
pub fn configure(connection: pog.Connection) -> Result(Nil, Nil)

/// Snoozes a log. It hides it until it happens again.
///
/// This is different than resolving because this will add a special
/// "reappeared" mark and it is reversible
pub fn snooze(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.snooze_log(connection, log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "snooze"))
}

/// Resets the snoozed state on a log.
///
/// This won't cause the "reappeared" mark to appear
pub fn wakeup(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.wake_up_log(connection, log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "wakeup"))
}

/// Resolves a log.
///
/// This keeps it frozen on the DB for future reference, but there is no way to
/// unresolve. A new log will be created instead.
///
/// If it fails, it returns a string describing the error.
///
/// If you really want to you can use SQL though (toggle the resolved boolean).
pub fn resolve(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.resolve_log(connection, log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "resolve"))
}

/// Shows active logs.
///
/// This hides both snoozed and resolved logs.
pub fn show(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "show"

  case sql.show(connection) {
    Ok(pog.Returned(_, l)) -> list.map(l, show_row_to_log) |> Ok

    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Shows snoozed logs.
///
/// This hides both active and resolved logs.
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

/// Shows solved logs.
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

/// Shows solved logs.
///
/// This hides both active and snoozed logs.
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
