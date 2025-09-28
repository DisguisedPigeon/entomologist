import entomologist/internal/sql.{
  type Level, Alert, Critical, Debug, Emergency, Info, Notice, Warning,
}
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/charlist
import gleam/int
import gleam/io
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import pog

/// A log.
///
/// The `rest` field is the metadata in json format, in case some extra fields
/// are provided by the user when logging from erlang-land.
type Log {
  Log(level: Level, msg: String, meta: Metadata, rest: String)
}

/// Log's metadata.
///
/// The fields `module`, `function`, `arity`, `file` and `line` are optional.
/// They will be substituted with "-" and -1 if they aren't present.
///
/// This might change in the near future.
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

/// A erlang occurrence timestamp.
type Timestamp =
  Int

/// Function called by the erlang logger module when a log is generated.
///
/// **This should never be called by a user**
///
/// You should use a logging library, like logging or wisp's log/rescue_crashes
pub fn save_to_db(log: Dynamic, connection: pog.Connection) -> Nil {
  // Parses the erlang log and calls do_save.
  // It then prints any errors that happened down the line and exits.
  let result = case decode.run(log, log_decoder()) {
    Ok(log) -> do_save(log, connection)
    Error(e) ->
      { "Failed decoding the log - " <> string.inspect(e) }
      |> Error
  }

  case result {
    Error(s) -> io.print_error(s)
    Ok(Nil) -> Nil
  }
}

/// Log saving
///
/// Checks if a log is already in DB
/// If it exists, calls `save_occurrence`
/// If not, it calls `save_log`
fn do_save(log: Log, connection: pog.Connection) -> Result(Nil, String) {
  let Log(level:, msg:, meta: Metadata(module:, function:, arity:, ..), ..) =
    log

  case sql.exist_log(connection, msg, level, function, module, arity) {
    // There is already a log for that occurrence. We have encountered it before.
    Ok(pog.Returned(1, [sql.ExistLogRow(id:)])) ->
      save_occurrence(log, id, connection)

    // The log isn't there. It's the first time we encounter that kind of log
    Ok(pog.Returned(0, [])) -> save_log(log, connection)

    Error(e) -> Error(describe_error(e, "Log existance"))

    Ok(ret_value) -> something_weird_happened(ret_value, log, "DO_SAVE")
  }
}

/// Saves a new log to DB and its first occurrence.
fn save_log(log: Log, connection: pog.Connection) -> Result(Nil, String) {
  let Log(
    level:,
    msg:,
    meta: Metadata(time:, module:, function:, arity:, file:, line:),
    rest:,
  ) = log

  let json = json.string(rest)

  let query_result =
    sql.add_log(
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
    |> result.map_error(describe_error(_, "Error creation"))

  use value <- result.try(query_result)

  case value {
    pog.Returned(1, [sql.AddLogRow(_)]) -> Ok(Nil)

    // Once again, no more and no less than one value should be returned.
    pog.Returned(..) as v -> something_weird_happened(v, log, "SAVE_LOG")
  }
}

/// Saves an existing log's new occurrence to DB and updates its most_recent_occurence field.
fn save_occurrence(
  log: Log,
  log_id: Int,
  connection: pog.Connection,
) -> Result(Nil, String) {
  let Log(meta: Metadata(time:, ..), rest:, ..) = log
  let json = json.string(rest)

  let query_result =
    sql.add_occurrence(connection, log_id, time, json)
    |> result.map_error(describe_error(_, "occurrence creation"))

  use value <- result.try(query_result)

  let result = case value {
    // Get the ID from the returned value
    pog.Returned(count: 1, rows: [sql.AddOccurrenceRow(id)]) ->
      sql.update_log_timestamp(connection, id)
      |> result.map_error(describe_error(_, "log timestamp update"))
      |> result.map(fn(_) { Nil })

    // It should never return any other amount of values.
    pog.Returned(..) as value ->
      something_weird_happened(value, log, "SAVE_OCCURRENCE")
  }

  use _ <- result.try(result)

  // TODO : Add the special "reappeared" mark when they are working on another 
  // SQL file.
  //
  // should also probably return the amount of rows modified to at least notify 
  // the user if black magic was performed with their data.
  sql.wake_up_log(connection, log_id)
  |> result.map_error(describe_error(_, "log wakeup"))
  |> result.map(fn(_) { Nil })
}

/// Turns a pog.QueryLog into a string.
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

/// This function catches some theoretically impossible cases,
/// it's here just in case I forget some edge case.
fn something_weird_happened(
  value value: b,
  log log: Log,
  location location: String,
) -> d {
  panic as {
      "[ERROR] something weird happened when processing the logs.\nAt :"
      <> location
      <> "\nValue: "
      <> string.inspect(value)
      <> "\nLog: "
      <> string.inspect(log)
    }
}

// ------------------- Decoders ------------------- //

/// Log decoder.
///
/// Internally, a log is provided as a dictionary with atoms as keys.
fn log_decoder() -> decode.Decoder(Log) {
  // This atoms should exist already, so this doesn't create any more.
  let meta_atom = atom.create("meta")
  let level_atom = atom.create("level")
  let rest_atom = atom.create("rest")

  let msg_string_atom = atom.create("msg_str")
  let msg_dict_atom = atom.create("msg_dict")

  use level <- decode.field(level_atom, level_decoder())
  use rest <- decode.field(rest_atom, decode.string)

  use msg <- decode.optional_field(
    msg_dict_atom,
    None,
    decode.optional(decode.dict(atom_decoder(), decode.dynamic)),
  )
  case msg {
    None -> {
      use msg <- decode.field(msg_string_atom, decode.string)
      use meta <- decode.field(meta_atom, metadata_decoder())
      decode.success(Log(level:, msg:, meta:, rest:))
    }

    Some(report_data) -> {
      use meta <- decode.field(meta_atom, metadata_decoder())

      let function =
        dict.get(report_data, "function")
        |> result.unwrap(dynamic.string("-"))
        |> decode.run(decode.string)
        |> result.unwrap("-")

      let module =
        dict.get(report_data, "module")
        |> result.unwrap(dynamic.string("-"))
        |> decode.run(decode.string)
        |> result.unwrap("-")

      let line =
        dict.get(report_data, "line")
        |> result.unwrap(dynamic.int(-1))
        |> decode.run(decode.int)
        |> result.unwrap(-1)

      let msg =
        dict.get(report_data, "message")
        |> result.unwrap(dynamic.string("No message could be decoded"))
        |> decode.run(decode.string)
        |> result.unwrap("No message could be decoded")

      decode.success(Log(
        level:,
        msg:,
        meta: Metadata(..meta, function:, line:, module:),
        rest:,
      ))
    }
  }
}

@external(erlang, "entomologist_logger_ffi", "id")
fn decode_atom(atom: Dynamic) -> atom.Atom

fn atom_decoder() -> decode.Decoder(String) {
  decode.new_primitive_decoder("Atom", fn(d) {
    decode_atom(d) |> atom.to_string() |> Ok
  })
}

/// Tries decoding a metadata value.
///
/// The module, function, arity and line should be optional fields, but for
/// simplicity I just substitute them for default values for now.
fn metadata_decoder() -> decode.Decoder(Metadata) {
  let time = atom.create("time")

  use time <- decode.optional_field(time, -1, decode.int)
  use module <- decode.optional_field("module", "-", decode.string)
  use function <- decode.optional_field("function", "-", decode.string)
  use arity <- decode.optional_field("arity", -1, decode.int)
  use file <- decode.optional_field("file", "-", decode.string)
  use line <- decode.optional_field("line", -1, decode.int)

  decode.success(Metadata(time:, module:, function:, arity:, file:, line:))
}

fn dynamic_to_charlist(ch) -> Result(charlist.Charlist, Level) {
  decode_charlist(ch) |> result.map_error(fn(_) { Warning })
}

/// Tries decoding a level (provided as a erlang charlist).
fn level_decoder() -> decode.Decoder(Level) {
  //decoder for level
  use chlist <- decode.new_primitive_decoder("level")
  //get the charlist from the dynamic and
  use chlist <- result.try(dynamic_to_charlist(chlist))
  //Get the actual value
  case charlist.to_string(chlist) {
    "debug" -> Ok(Debug)
    "info" -> Ok(Info)
    "notice" -> Ok(Notice)
    "warning" -> Ok(Warning)
    "error" -> Ok(sql.Error)
    "critical" -> Ok(Critical)
    "alert" -> Ok(Alert)
    "emergency" -> Ok(Emergency)
    not_recognized -> {
      io.print_error(
        "[WARNING] failed decoding entomologist level \""
        <> string.inspect(not_recognized)
        <> "\", falling back to warning.",
      )
      Error(Warning)
    }
  }
}

@external(erlang, "utils", "charlist_decoder")
fn decode_charlist(charlist: dynamic.Dynamic) -> Result(charlist.Charlist, Nil)
