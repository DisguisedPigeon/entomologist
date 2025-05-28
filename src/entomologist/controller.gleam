import entomologist/sql.{
  type Level, Alert, Critical, Debug, Emergency, Info, Notice, Warning,
}
import gleam/int
import gleam/json
import gleam/option
import gleam/result
import gleam/string
import pog

/// Snoozes an error. It hides it until it happens again.
///
/// This is different than resolving because this will add a special
/// "reappeared" mark and it is reversible
pub fn snooze(id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.snooze_error(connection, id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "snooze"))
}

/// Resets the snoozed state on an error.
///
/// This won't cause the "reappeared" mark to appear
pub fn wakeup(id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.wake_up_error(connection, id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "wakeup"))
}

/// Resolves an error.
///
/// This keeps it frozen on the DB for future reference, but there is no way to
/// unresolve. A new error will be created instead.
///
/// If it fails, it returns a string describing the error.
///
/// If you really want to you can use SQL though (toggle the resolved boolean).
pub fn resolve(id: Int, connection: pog.Connection) -> Result(Nil, String) {
  sql.resolve_error(connection, id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "resolve"))
}

/// Shows active errors.
///
/// This hides both snoozed and resolved errors.
pub fn show(connection: pog.Connection) -> Result(json.Json, String) {
  case sql.show(connection) {
    Ok(pog.Returned(_, l)) -> Ok(json.array(l, encode_show_row))
    Error(e) -> Error(describe_error(e, "show"))
  }
}

/// Shows snoozed errors.
///
/// This hides both active and resolved errors.
pub fn snoozed(connection: pog.Connection) -> Result(json.Json, String) {
  case sql.snoozed(connection) {
    Ok(pog.Returned(_, l)) -> Ok(json.array(l, encode_snoozed_row))
    Error(e) -> Error(describe_error(e, "show"))
  }
}

/// Shows solved errors.
///
/// This hides both active and snoozed errors.
pub fn solved(connection: pog.Connection) -> Result(json.Json, String) {
  case sql.solved(connection) {
    Ok(pog.Returned(_, l)) -> Ok(json.array(l, encode_solved_row))
    Error(e) -> Error(describe_error(e, "show"))
  }
}

/// Shows solved errors.
///
/// This hides both active and snoozed errors.
pub fn occurrences(
  error_id: Int,
  connection: pog.Connection,
) -> Result(json.Json, String) {
  case sql.occurrences(connection, error_id), sql.error(connection, error_id) {
    Ok(pog.Returned(_, l)), Ok(pog.Returned(1, [e])) ->
      Ok(
        json.object([
          #("error", encode_error_row(e)),
          #("occurrences", json.array(l, encode_occurrences_row)),
        ]),
      )
    Error(e), _ -> Error(describe_error(e, "show failed occurrences"))
    Ok(_), Error(e) -> Error(describe_error(e, "show failed error"))
    Ok(_), Ok(v) ->
      Error(
        "something_weird_happened, a select with ID returned more than one value. Values: "
        <> string.inspect(v),
      )
  }
}

// ------------------- JSON Encoders ------------------- //

fn encode_level(level: Level) -> json.Json {
  case level {
    Debug -> json.string("debug")
    Info -> json.string("info")
    Notice -> json.string("notice")
    Warning -> json.string("warning")
    sql.Error -> json.string("error")
    Critical -> json.string("critical")
    Alert -> json.string("alert")
    Emergency -> json.string("emergency")
  }
}

fn encode_show_row(show_row: sql.ShowRow) -> json.Json {
  let sql.ShowRow(
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
  ) = show_row
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

fn encode_snoozed_row(snoozed_row: sql.SnoozedRow) -> json.Json {
  let sql.SnoozedRow(
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
  ) = snoozed_row
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

fn encode_solved_row(solved_row: sql.SolvedRow) -> json.Json {
  let sql.SolvedRow(
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
  ) = solved_row
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

fn encode_error_row(error_row: sql.ErrorRow) -> json.Json {
  let sql.ErrorRow(
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
  ) = error_row
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

fn encode_occurrences_row(occurrences_row: sql.OccurrencesRow) -> json.Json {
  let sql.OccurrencesRow(id:, error:, timestamp:, full_contents:) =
    occurrences_row
  json.object([
    #("id", json.int(id)),
    #("error", case error {
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

/// Turns a pog.QueryError into a string.
///
/// The string contains a brief error description and the error's origin function location
fn describe_error(error: pog.QueryError, origin: String) -> String {
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
