import entomologist/sql.{
  type Level, Alert, Critical, Debug, Emergency, Info, Notice, Warning,
}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import pog

type Timestamp =
  Int

type Log {
  Log(level: Level, msg: String, meta: Metadata, rest: String)
}

type Metadata {
  Metadata(
    time: Timestamp,
    module: Option(String),
    function: Option(String),
    arity: Option(Int),
    file: Option(String),
    line: Option(Int),
  )
}

/// Configuring function for the erlang logger.
///
/// It registers the library as a logging handler and saves the DB connection for later usage.
@external(erlang, "entomologist_logger_ffi", "configure")
pub fn configure(connection: pog.Connection) -> Result(Nil, Nil)

/// Function called by the erlang logger module when a log is generated.
///
/// This should never be called by a user.
pub fn save_to_db(
  log: Dict(Atom, Dynamic),
  connection: pog.Connection,
) -> Result(Nil, String) {
  case log |> cast_log {
    Ok(log) -> do_save(log, connection)

    Error(_) -> Error("")
  }
}

fn do_save(log: Log, connection: pog.Connection) {
  let Log(level:, msg:, meta: Metadata(module:, function:, arity:, ..), ..) =
    handle_none_values(log)

  let function = option.unwrap(function, "")
  let module = option.unwrap(module, "")
  let arity = option.unwrap(arity, -1)

  case sql.exist_error(connection, msg, level, function, module, arity) {
    // There is already an error for that occurrence. We have encountered it before.
    Ok(pog.Returned(1, [sql.ExistErrorRow(id:)])) ->
      save_occurrence(log, id, connection)

    // The error isn't there. It's the first time we encounter that kind of error
    Ok(pog.Returned(0, [])) -> save_error(log, connection)

    Ok(_) ->
      panic as "This should not happen. There should only be one active error with a given message and origin."

    Error(pog.PostgresqlError(_, _, _)) -> Error("Postgres error.")

    Error(pog.QueryTimeout) -> Error("Query timed out.")

    Error(pog.ConnectionUnavailable) ->
      Error("The database connection was missing.")

    Error(pog.ConstraintViolated(_, _, _)) ->
      Error("Postgres constraint violated. Invalid data provided.")

    Error(pog.UnexpectedArgumentCount(_, _)) ->
      Error("Unexpected argument count.")

    Error(pog.UnexpectedArgumentType(_, _)) ->
      Error("Unexpected argument type.")

    Error(pog.UnexpectedResultType(_)) -> Error("Unexpected result type.")
  }
}

fn save_error(log: Log, connection: pog.Connection) -> Result(Nil, String) {
  let change_for_string = fn(returned) { fn(_ignored) { returned } }

  let Log(
    level:,
    msg:,
    meta: Metadata(time:, module:, function:, arity:, file:, line:),
    rest:,
  ) = log

  let module = option.unwrap(module, "")
  let function = option.unwrap(function, "")
  let arity = option.unwrap(arity, -1)
  let file = option.unwrap(file, "")
  let line = option.unwrap(line, -1)

  let json = json.string(rest)

  let query_result =
    sql.add_error(
      connection,
      msg,
      level,
      module,
      function,
      arity,
      file,
      line,
      time,
      json,
    )
    |> result.map_error(change_for_string("failed creating error"))

  use value <- result.try(query_result)

  case value {
    pog.Returned(1, [sql.AddErrorRow(_)]) -> Ok(Nil)
    _ -> panic as "This should not happen. A error creation only returns a ID"
  }
}

fn save_occurrence(
  log: Log,
  error_id: Int,
  connection: pog.Connection,
) -> Result(Nil, String) {
  let change_for_string = fn(returned) { fn(_ignored) { returned } }

  let Log(meta: Metadata(time:, ..), rest:, ..) = log

  let json = json.string(rest)

  let query_result =
    sql.add_occurrence(connection, error_id, time, json)
    |> result.map_error(change_for_string("Failed creating occurrence."))

  use value <- result.try(query_result)

  case value {
    pog.Returned(count: 1, rows: [sql.AddOccurrenceRow(id)]) ->
      sql.update_error_timestamp(connection, id)
      |> result.map_error(change_for_string(
        "Failed updating the error timestamp to the new occurrence.",
      ))

    _ ->
      panic as "this should never happen. Only one occurrence should be created."
  }
  |> result.map(fn(_) { Nil })
}

fn handle_none_values(log: Log) -> Log {
  let Log(meta:, ..) = log
  let Metadata(time:, module:, function:, arity:, file:, line:) = meta

  Log(
    ..log,
    meta: Metadata(
      time:,
      function: Some(option.unwrap(function, "-")),
      module: Some(option.unwrap(module, "-")),
      file: Some(option.unwrap(file, "-")),
      arity: Some(option.unwrap(arity, -1)),
      line: Some(option.unwrap(line, -1)),
    ),
  )
}

// ------------------- Decoders ------------------- //

fn decode_nil_error(
  value: Dynamic,
  decoder: decode.Decoder(a),
) -> Result(a, Nil) {
  decode.run(value, decoder) |> result.map_error(fn(_) { Nil })
}

@external(erlang, "unicode", "characters_to_binary")
fn decode_charlist_to_string(s: Dynamic) -> String

/// This function takes in a dict with the log data and tries converting it into a Log
fn cast_log(log: Dict(Atom, Dynamic)) -> Result(Log, Nil) {
  // This atoms should exist already, so this doesn't create any more.
  let meta_atom = atom.create_from_string("meta")
  let msg_atom = atom.create_from_string("msg")
  let level_atom = atom.create_from_string("level")
  let rest_atom = atom.create_from_string("rest")

  use meta <- result.try(dict.get(log, meta_atom))
  use rest <- result.try(dict.get(log, rest_atom))
  use msg <- result.try(dict.get(log, msg_atom))
  use level <- result.try(dict.get(log, level_atom))

  use msg <- result.try(decode_nil_error(msg, decode.string))
  use meta <- result.try(decode_nil_error(meta, metadata_decoder()))
  use rest <- result.try(decode_nil_error(rest, decode.string))
  let level = case decode_charlist_to_string(level) {
    "emergency" -> Emergency
    "alert" -> Alert
    "critical" -> Critical
    "error" -> sql.Error
    "warning" -> Warning
    "notice" -> Notice
    "info" -> Info
    "debug" -> Debug
    _ -> Warning
  }

  Ok(Log(level:, msg:, meta:, rest:))
}

fn metadata_decoder() -> decode.Decoder(Metadata) {
  let optional_string = decode.optional(decode.string)
  let time = atom.create_from_string("time")

  use time <- decode.field(time, decode.int)
  use module <- decode.optional_field("module", None, optional_string)
  use function <- decode.optional_field("function", None, optional_string)
  use arity <- decode.optional_field("arity", None, decode.optional(decode.int))
  use file <- decode.optional_field("file", None, optional_string)
  use line <- decode.optional_field("line", None, decode.optional(decode.int))
  decode.success(Metadata(time:, module:, function:, arity:, file:, line:))
}
