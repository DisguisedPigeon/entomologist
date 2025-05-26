import entomologist/sql.{
  type Level, Alert, Critical, Debug, Emergency, Info, Notice, Warning,
}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/int
import gleam/io
import gleam/json
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

/// Configuring function for the erlang logger.
///
/// It registers the library as a logging handler and saves the DB connection
/// for later usage.
@external(erlang, "entomologist_logger_ffi", "configure")
pub fn configure(connection: pog.Connection) -> Result(Nil, Nil)

/// Function called by the erlang logger module when a log is generated.
///
/// **This should never be called by a user**
///
/// You should use a logging library, like logging or wisp's log/rescue_crashes
pub fn save_to_db(log: Dynamic, connection: pog.Connection) -> Nil {
  // Parses the erlang error and calls do_save.
  // It then prints any errors that happened down the line and exits.
  let result = case decode.run(log, log_decoder()) {
    Ok(log) -> do_save(log, connection)
    Error(_) -> Error("Failed decoding the log")
  }

  case result {
    Ok(_) -> Nil
    Error(s) -> io.print_error("[WARNING] Entomologist failed: " <> s)
  }
}

/// Snoozes an error. It hides it until it happens again.
///
/// This is different than resolving because this will add a special
/// "reappeared" mark and it is reversible
pub fn snooze(id: Int, connection: pog.Connection) {
  sql.snooze_error(connection, id)
}

/// Resets the snoozed state on an error.
///
/// This won't cause the "reappeared" mark to appear
pub fn wakeup(id: Int, connection: pog.Connection) {
  sql.wake_up_error(connection, id)
}

/// Resolves an error.
///
/// This keeps it frozen on the DB for future reference, but there is no way to
/// unresolve. A new error will be created instead.
///
///If you really want to you can use SQL though (toggle the resolved boolean).
pub fn resolve(id: Int, connection: pog.Connection) {
  sql.resolve_error(connection, id)
}

/// Log saving
///
/// Checks if an error is already in DB
/// If it exists, calls `save_occurrence`
/// If not, it calls `save_error`
fn do_save(log: Log, connection: pog.Connection) -> Result(Nil, String) {
  let Log(level:, msg:, meta: Metadata(module:, function:, arity:, ..), ..) =
    log

  case sql.exist_error(connection, msg, level, function, module, arity) {
    // There is already an error for that occurrence. We have encountered it before.
    Ok(pog.Returned(1, [sql.ExistErrorRow(id:)])) ->
      save_occurrence(log, id, connection)

    // The error isn't there. It's the first time we encounter that kind of error
    Ok(pog.Returned(0, [])) -> save_error(log, connection)

    Error(e) -> Error(describe_error(e, "Error existance"))

    Ok(ret_value) ->
      something_weird_happened(connection, ret_value, log, "DO_SAVE")
  }
}

/// Saves a new error to DB and its first occurrence.
fn save_error(log: Log, connection: pog.Connection) -> Result(Nil, String) {
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
    |> result.map_error(describe_error(_, "Error creation"))

  use value <- result.try(query_result)

  case value {
    pog.Returned(1, [sql.AddErrorRow(_)]) -> Ok(Nil)

    // Once again, no more and no less than one value should be returned.
    pog.Returned(..) as v ->
      something_weird_happened(connection, v, log, "SAVE_ERROR")
  }
}

/// Saves an existing error's new occurrence to DB and updates its most_recent_occurence field.
fn save_occurrence(
  log: Log,
  error_id: Int,
  connection: pog.Connection,
) -> Result(Nil, String) {
  let Log(meta: Metadata(time:, ..), rest:, ..) = log
  let json = json.string(rest)

  let query_result =
    sql.add_occurrence(connection, error_id, time, json)
    |> result.map_error(describe_error(_, "occurrence creation"))

  use value <- result.try(query_result)

  let result = case value {
    // Get the ID from the returned value
    pog.Returned(count: 1, rows: [sql.AddOccurrenceRow(id)]) ->
      sql.update_error_timestamp(connection, id)
      |> result.map_error(describe_error(_, "error timestamp update"))
      |> result.map(fn(_) { Nil })

    // It should never return any other amount of values.
    pog.Returned(..) as value ->
      something_weird_happened(connection, value, log, "SAVE_OCCURRENCE")
  }

  use _ <- result.try(result)

  // TODO : Add the special "reappeared" mark when they are working on another 
  // SQL file.
  //
  // should also probably return the amount of rows modified to at least notify 
  // the user if black magic was performed with their data.
  sql.wake_up_error(connection, error_id)
  |> result.map_error(describe_error(_, "error wakeup"))
  |> result.map(fn(_) { Nil })
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

/// My own gamma ray detector
///
/// This function is called in places where it should be theoretically
/// impossible to reach, it's here just in case I forget some edge case.
///
/// Before the jokes, it instructs the user to open an issue if this is
/// actually called.
fn something_weird_happened(
  connection connection: pog.Connection,
  value value: b,
  log log: Log,
  location location: String,
) -> d {
  let s = "[WARNING] something weird happened when processing the logs.
        This should never print.
        If you see this, write an issue with this log on https://github.com/DisguisedPigeon/entomologist.
        -------------------------BEGINNING--------------------------------------------------------------
        " <> location <> "
        " <> string.inspect(value) <> "
        --SOLAR RAY DETECTED--
        " <> string.inspect(log) <> "
        --We'll have to call <titlecard>THE EXORCIST</titlecard>--
        " <> string.inspect(connection) <> "
        -------------------------END--------------------------------------------------------------------"
  panic as s
}

// ------------------- Decoders ------------------- //

/// Log decoder.
///
/// Internally, a log is provided as a dictionary with atoms as keys.
fn log_decoder() -> decode.Decoder(Log) {
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
///
/// The module, function, arity and line should be optional fields, but for
/// simplicity I just substitute them for default values for now.
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

/// Tries decoding a level (provided as a erlang charlist).
fn level_decoder() -> decode.Decoder(Level) {
  // the level is given as a charlist, and as such, we can't decode it to string directly
  use charlist <- decode.then(decode.dynamic)

  // charlist_to_string calls erlang's unicode:characters_to_binary
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

/// Transforms a charlist to a string.
///
/// This just calls erlang's unicode:characters_to_binary.
///
/// Since there's no way to decode a dynamic into a
/// gleam_erlang/charlist.Charlist, I just pass it in as a dynamic value.
@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(s: Dynamic) -> String
