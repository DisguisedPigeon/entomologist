import entomologist/internal/sql.{type Level}
import entomologist/internal/utils
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/charlist
import gleam/int
import gleam/io
import gleam/json
import gleam/option
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
  let final_value = {
    // parse log
    use log <- result.try(
      decode.run(log, log_decoder())
      |> result.map_error(fn(e) {
        "Failed decoding the log - " <> string.inspect(e)
      }),
    )

    let Log(level:, msg:, meta: Metadata(module:, function:, arity:, ..), ..) =
      log

    // Check if there is already any instance of that log
    use returned <- result.try(
      sql.exist_log(connection, msg, level, function, module, arity)
      |> result.map_error(utils.describe_error(_, "Log existance")),
    )

    case returned.count {
      // The log isn't there. Save it to DB
      0 -> save_log(log, connection)

      // There is already a log for that occurrence. We have encountered it before.
      1 -> {
        let assert [sql.ExistLogRow(id:)] = returned.rows
          as "there should only be one row, since we checked the count"
        // Save as a new occurrence of a known log
        save_occurrence(log, id, connection)
      }

      // Logs are duplicated. This shouldn't happen. Panic
      _ ->
        panic as {
          "[ERROR] Duplicated log. Detected at: save_to_db. Returned rows: "
          <> string.inspect(returned.rows)
        }
    }
  }

  use <- result.lazy_unwrap(final_value)
  result.unwrap_error(final_value, "NOT PRINTED") |> io.print_error()
}

/// Saves a new log to DB and its first occurrence.
fn save_log(log: Log, connection: pog.Connection) -> Result(Nil, String) {
  let Log(
    level:,
    msg:,
    meta: Metadata(time:, module:, function:, arity:, file:, line:),
    rest:,
  ) = log

  let rest = json.string(rest)

  let returned_log =
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
      rest,
    )
    |> result.map_error(utils.describe_error(_, "Error creation"))

  use returned_log <- result.try(returned_log)
  case returned_log.count {
    1 -> Ok(Nil)

    // Once again, no more and no less than one value should be returned.
    _ ->
      panic as {
        "[ERROR] Unexpected amount of logs returned. Detected at: save_log. Rows: "
        <> string.inspect(returned_log.rows)
      }
  }
}

/// Saves an existing log's new occurrence to DB and updates its most_recent_occurence field.
fn save_occurrence(
  log: Log,
  log_id: Int,
  connection: pog.Connection,
) -> Result(Nil, String) {
  let Log(meta: Metadata(time:, ..), rest:, ..) = log
  let rest = json.string(rest)

  let returned_occurrence =
    sql.add_occurrence(connection, log_id, time, rest)
    |> result.map_error(utils.describe_error(_, "occurrence creation"))

  use returned_occurrence <- result.try(returned_occurrence)

  let result = case returned_occurrence.count {
    // Get the ID from the returned value
    1 -> {
      let assert [sql.AddOccurrenceRow(id)] = returned_occurrence.rows

      // update last timestamp for the log
      sql.update_log_timestamp(connection, id)
      |> result.map(fn(_) { Nil })
      |> result.map_error(utils.describe_error(_, "log timestamp update"))
    }

    // It should never return any other amount of values.
    _ ->
      panic as {
        "[ERROR] Unexpected amount of occurrences returned. Detected at: save_occurrence. Rows: "
        <> string.inspect(returned_occurrence.rows)
      }
  }

  use Nil <- result.try(result)

  // TODO : Add the special "reappeared" tag when the log was muted.
  //
  // should also probably return the amount of rows modified to at least notify
  // the user if black magic was performed with their data.
  use unmuted <- result.try(
    sql.unmute(connection, log_id)
    |> result.map_error(utils.describe_error(_, "log unmute")),
  )
  case unmuted.count {
    1 -> Ok(Nil)
    _ ->
      panic as {
        "[ERROR] Unexpected amount of updated queries. Detected at: save_occurrence. Rows: "
        <> string.inspect(returned_occurrence.rows)
      }
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
    option.None,
    decode.optional(decode.dict(atom_decoder(), decode.dynamic)),
  )

  case msg {
    option.None -> {
      use msg <- decode.field(msg_string_atom, decode.string)
      use meta <- decode.field(meta_atom, metadata_decoder())
      decode.success(Log(level:, msg:, meta:, rest:))
    }

    option.Some(report_data) -> {
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
  use module <- decode.optional_field("module", "-", decode.string)
  use function <- decode.optional_field("function", "-", decode.string)
  use arity <- decode.optional_field("arity", -1, decode.int)
  use file <- decode.optional_field("file", "-", decode.string)
  use line <- decode.optional_field("line", -1, decode.int)

  let time = atom.create("time")

  use time <- decode.optional_field(time, -1, decode.int)

  // detect the timestamp format.
  let size = time |> int.to_string() |> string.length

  // This should work for the next few years...
  // like around 3115 years
  let time = case size {
    // assume seconds
    n if n < 13 -> time

    // assume milliseconds
    13 | 14 | 15 -> time / 1000

    // assume microseconds
    _ -> time / 1_000_000
  }

  decode.success(Metadata(time:, module:, function:, arity:, file:, line:))
}

fn dynamic_to_charlist(ch) -> Result(charlist.Charlist, Level) {
  decode_charlist(ch) |> result.map_error(fn(_) { sql.Warning })
}

/// Tries decoding a level (provided as a erlang charlist).
fn level_decoder() -> decode.Decoder(Level) {
  //decoder for level
  use chlist <- decode.new_primitive_decoder("level")
  //get the charlist from the dynamic and
  use chlist <- result.try(dynamic_to_charlist(chlist))
  //Get the actual value
  case charlist.to_string(chlist) {
    "debug" -> Ok(sql.Debug)
    "info" -> Ok(sql.Info)
    "notice" -> Ok(sql.Notice)
    "warning" -> Ok(sql.Warning)
    "error" -> Ok(sql.Error)
    "critical" -> Ok(sql.Critical)
    "alert" -> Ok(sql.Alert)
    "emergency" -> Ok(sql.Emergency)
    not_recognized -> {
      io.print_error(
        "[WARNING] failed decoding entomologist level \""
        <> string.inspect(not_recognized)
        <> "\", falling back to warning.",
      )
      Error(sql.Warning)
    }
  }
}

@external(erlang, "utils", "charlist_decoder")
fn decode_charlist(charlist: dynamic.Dynamic) -> Result(charlist.Charlist, Nil)
