import entomologist/sql.{
  type Level, Alert, Critical, Debug, Emergency, Info, Notice, Warning,
}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/io
import gleam/json
import gleam/result
import gleam/string
import pog

type Timestamp =
  Int

type Log {
  Log(level: Level, msg: String, meta: Metadata, rest: String)
}

type Metadata {
  Metadata(
    time: Timestamp,
    module: String,
    function: String,
    arity: Int,
    file: String,
    line: Int,
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
  log: Dynamic,
  connection: pog.Connection,
) -> Result(Nil, String) {
  case decode.run(log, log_decoder()) {
    Ok(log) -> do_save(log, connection)
    Error(_) -> Error("")
  }
}

fn do_save(log: Log, connection: pog.Connection) {
  let Log(level:, msg:, meta: Metadata(module:, function:, arity:, ..), ..) =
    log

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

// ------------------- Decoders ------------------- //

/// Tries decoding a log.
fn log_decoder() {
  // This atoms should exist already, so this doesn't create any more.
  let meta_atom = atom.create_from_string("meta")
  let msg_atom = atom.create_from_string("msg")
  let level_atom = atom.create_from_string("level")
  let rest_atom = atom.create_from_string("rest")

  use level <- decode.field(level_atom, level_decoder())
  use msg <- decode.field(msg_atom, decode.string)
  use meta <- decode.field(meta_atom, metadata_decoder())
  use rest <- decode.field(rest_atom, decode.string)

  decode.success(Log(level:, msg:, meta:, rest:))
}

/// Tries decoding a metadata value.
fn metadata_decoder() -> decode.Decoder(Metadata) {
  let time = atom.create_from_string("time")

  use time <- decode.field(time, decode.int)
  use module <- decode.optional_field("module", "-", decode.string)
  use function <- decode.optional_field("function", "-", decode.string)
  use arity <- decode.optional_field("arity", -1, decode.int)
  use file <- decode.optional_field("file", "-", decode.string)
  use line <- decode.optional_field("line", -1, decode.int)
  decode.success(Metadata(time:, module:, function:, arity:, file:, line:))
}

///external erlang function to transform a charlist to a string.
@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(s: Dynamic) -> String

/// Tries decoding a level stored as a charlist.
fn level_decoder() -> decode.Decoder(Level) {
  // the level is stored as a charlist, and as such, we can't decode it to string directly
  use charlist <- decode.then(decode.dynamic)

  case charlist_to_string(charlist) {
    "debug" -> decode.success(Debug)
    "info" -> decode.success(Info)
    "notice" -> decode.success(Notice)
    "warning" -> decode.success(Warning)
    "error" -> decode.success(sql.Error)
    "critical" -> decode.success(Critical)
    "alert" -> decode.success(Alert)
    "emergency" -> decode.success(Emergency)
    not_recognized -> {
      io.print_error(
        "[WARNING] failed decoding entomologist level \""
        <> string.inspect(not_recognized)
        <> "\", falling back to warning.",
      )
      decode.success(Warning)
    }
  }
}
