import entomologist/sql.{type Level}
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import pog

pub opaque type Log {
  Log(
    level: Level,
    message: String,
    description: Option(String),
    code: Option(Int),
  )
}

/// Turns a log to a string.
/// Formats as [LEVEL]: (CODE) MESSAGE | DESCRIPTION
/// If the description or the code are not present, they get omitted.
pub fn to_string_full(log: Log) {
  case log.description, log.code {
    Some(_), Some(code) ->
      "["
      <> level_to_string(log.level)
      <> "]: "
      <> "("
      <> int.to_string(code)
      <> ") "
      <> log.message
      <> " | "
      <> description_to_string(log.description)

    Some(_), None ->
      "["
      <> level_to_string(log.level)
      <> "]: "
      <> log.message
      <> "  |  "
      <> description_to_string(log.description)

    None, Some(code) ->
      "["
      <> level_to_string(log.level)
      <> "]: "
      <> "("
      <> int.to_string(code)
      <> ") "
      <> log.message

    None, None -> "[" <> level_to_string(log.level) <> "]: " <> log.message
  }
}

/// Turns a log to a string.
/// Formats as [LEVEL]: message.
/// For description and code, use `to_string_full`
pub fn to_string(log: Log) {
  to_string_full(Log(..log, code: None, description: None))
}

fn description_to_string(description: Option(String)) -> String {
  case description {
    Some(s) -> s
    None -> ""
  }
}

fn level_to_string(level: Level) {
  case level {
    sql.Debug -> "❔"
    sql.Info -> "❕"
    sql.Notice -> "❕"
    sql.Warning -> "❗"
    sql.Error -> "⁉️"
    sql.Critical -> "❌"
    sql.Alert -> "🚨"
    sql.Emergency -> "💀"
  }
}

/// Creates a valid Log value to be saved on DB.
///
/// Change its level or set its code and description with the corresponding function.
///
///
/// ## Examples:
/// ```gleam
/// default_log("example_log") 
/// |> info()
/// |> description("This is a example, informative log that provides some info about the status of the execution.")
/// ```
/// Change its level or set its code and description with the corresponding function.
pub fn default_log(message: String) -> Log {
  Log(level: sql.Debug, message:, code: None, description: None)
}

/// Changes the description of a log.
///
/// The code of a log should be used for fast DB sorting and, as such, a log's purpose and criticality should be identifiable only by its message and level.
pub fn description(log: Log, description: String) -> Log {
  Log(..log, description: Some(description))
}

/// Changes the code of a log.
///
/// The code of a log should be used for fast DB sorting and, as such, a log's purpose and criticality should be identifiable only by its message and level.
pub fn code(log: Log, code: Int) -> Log {
  Log(..log, code: Some(code))
}

/// Changes the level of a log to debug.
///
/// Information useful to devs and other tech-savy users.
pub fn debug(log: Log) -> Log {
  Log(..log, level: sql.Debug)
}

/// Changes the level of a log to info.
///
/// Useful information about the app. Starts/stops, configurations etc.
pub fn info(log: Log) -> Log {
  Log(..log, level: sql.Info)
}

/// Changes the level of a log to warning.
///
/// Odd but not failing states. Automatically recoverable.
pub fn warning(log: Log) -> Log {
  Log(..log, level: sql.Warning)
}

/// Changes the level of a log to notice.
pub fn notice(log: Log) -> Log {
  Log(..log, level: sql.Notice)
}

/// Changes the level of a log to error.
///
/// Current operation will fail, but the app can keep running.
pub fn error(log: Log) -> Log {
  Log(..log, level: sql.Error)
}

/// Changes the level of a log to critical.
///
/// Current operation will fail, the app might crash.
pub fn critical(log: Log) -> Log {
  Log(..log, level: sql.Critical)
}

/// Changes the level of a log to emergency.
///
/// Current operation will fail, the app state is not recoverable. Say goodbye to 99.9% uptime.
pub fn alert(log: Log) -> Log {
  Log(..log, level: sql.Alert)
}

/// Changes the level of a log to emergency.
///
/// Current operation will fail, the app is on fire, the fire department is too, and society will collapse briefly.
pub fn emergency(log: Log) -> Log {
  Log(..log, level: sql.Emergency)
}

type SqlRow {
  SlimRow(sql.AddLogRow)
  FullRow(sql.AddLogFullRow)
  DescRow(sql.AddLogDescRow)
  CodeRow(sql.AddLogCodeRow)
}

fn to_slim_row(returned: pog.Returned(sql.AddLogRow)) -> SqlRow {
  case returned {
    pog.Returned(count: 1, rows: [row]) -> SlimRow(row)
    _ -> panic as "called to_slim_row on invalid returned value"
  }
}

fn to_full_row(returned: pog.Returned(sql.AddLogFullRow)) -> SqlRow {
  case returned {
    pog.Returned(count: 1, rows: [row]) -> FullRow(row)
    _ -> panic as "called to_slim_row on invalid returned value"
  }
}

fn to_desc_row(returned: pog.Returned(sql.AddLogDescRow)) -> SqlRow {
  case returned {
    pog.Returned(count: 1, rows: [row]) -> DescRow(row)
    _ -> panic as "called to_slim_row on invalid returned value"
  }
}

fn to_code_row(returned: pog.Returned(sql.AddLogCodeRow)) -> SqlRow {
  case returned {
    pog.Returned(count: 1, rows: [row]) -> CodeRow(row)
    _ -> panic as "called to_slim_row on invalid returned value"
  }
}

fn get_sqlrow_id(row: SqlRow) -> Int {
  case row {
    SlimRow(row) -> row.id
    FullRow(row) -> row.id
    DescRow(row) -> row.id
    CodeRow(row) -> row.id
  }
}

// Add a log to DB using the required squirrel method.
fn do_log(db: pog.Connection, log: Log) -> Result(Int, Nil) {
  let to_nil = fn(_) { Nil }

  let Log(level:, message:, ..) = log

  case log.description, log.code {
    Some(description), Some(code) -> {
      sql.add_log_full(db, level, message, description, code)
      |> result.map_error(to_nil)
      |> result.map(fn(log) {
        to_full_row(log)
        |> get_sqlrow_id
      })
    }

    Some(description), None ->
      sql.add_log_desc(db, level, message, description)
      |> result.map_error(to_nil)
      |> result.map(fn(log) {
        to_desc_row(log)
        |> get_sqlrow_id
      })

    None, Some(code) ->
      sql.add_log_code(db, level, message, code)
      |> result.map_error(to_nil)
      |> result.map(fn(log) {
        to_code_row(log)
        |> get_sqlrow_id
      })
    None, None ->
      sql.add_log(db, level, message)
      |> result.map_error(to_nil)
      |> result.map(fn(log) {
        to_slim_row(log)
        |> get_sqlrow_id
      })
  }
}

/// Adds a log to DB and passes it to the erlang logger
///
/// It returns `Ok(Int)` with the created log's id if it succeeds or `Error(Nil)` if it fails.
pub fn log(log: Log, db: pog.Connection) -> Result(Int, Nil) {
  do_log(db, log)
}

/// Adds a log to DB and passes it to the erlang logger. Crashes immediatly after.
pub fn log_and_crash(log: Log, db: pog.Connection) -> Result(Nil, Nil) {
  let log = log |> critical()
  case do_log(db, log) {
    Error(a) -> io.println("failed log creation: " <> string.inspect(a))
    Ok(id) -> io.println("created log with id " <> int.to_string(id))
  }
  panic as "Crashing..."
}
