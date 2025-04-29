import entomologist/sql.{type Level}
import gleam/bool
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import pog

pub opaque type Log {
  Log(
    level: Level,
    message: String,
    description: Option(String),
    code: Option(Int),
  )
}

pub fn to_string(log: Log) {
  "["
  <> level_to_string(log.level)
  <> "]: "
  <> code_to_string(log.code)
  <> log.message
}

pub fn to_string_full(log: Log) {
  "["
  <> level_to_string(log.level)
  <> "]: "
  <> code_to_string(log.code)
  <> log.message
  <> "  |  "
  <> description_to_string(log.description)
}

fn description_to_string(description: Option(String)) -> String {
  case description {
    Some(s) -> s
    None -> ""
  }
}

fn code_to_string(code: Option(Int)) -> String {
  case code {
    Some(i) -> " (" <> int.to_string(i) <> ") "
    None -> ""
  }
}

pub fn level_to_string(level: Level) {
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

// Add a log to DB using the required squirrel method.
fn do_log(db: pog.Connection, log: Log) -> Result(Int, Nil) {
  case log {
    Log(level:, message:, description: Some(description), code: Some(code)) -> {
      use pog.Returned(count:, rows:) <- result.try(
        sql.add_log_full(db, level, message, description, code)
        |> result.map_error(fn(_) { Nil }),
      )
      use <- bool.guard(when: 1 == count, return: Error(Nil))
      let assert [sql.AddLogFullRow(id:)] = rows
      Ok(id)
    }
    Log(level:, message:, description: Some(description), code: None) -> {
      use pog.Returned(count:, rows:) <- result.try(
        sql.add_log_desc(db, level, message, description)
        |> result.map_error(fn(_) { Nil }),
      )
      use <- bool.guard(when: 1 == count, return: Error(Nil))
      let assert [sql.AddLogDescRow(id:)] = rows
      Ok(id)
    }
    Log(level:, message:, description: None, code: Some(code)) -> {
      use pog.Returned(count:, rows:) <- result.try(
        sql.add_log_code(db, level, message, code)
        |> result.map_error(fn(_) { Nil }),
      )
      use <- bool.guard(when: 1 == count, return: Error(Nil))
      let assert [sql.AddLogCodeRow(id:)] = rows
      Ok(id)
    }
    Log(level:, message:, description: None, code: None) -> {
      use pog.Returned(count:, rows:) <- result.try(
        sql.add_log(db, level, message)
        |> result.map_error(fn(_) { Nil }),
      )
      use <- bool.guard(when: 1 == count, return: Error(Nil))
      let assert [sql.AddLogRow(id:)] = rows
      Ok(id)
    }
  }
}

/// Adds a log to DB and passes it to the erlang logger
///
/// Due to erlang's logger implementation, the level is only useful to the DB side (???)
/// It returns `Ok(Nil)` if it succeeds or `Error(Nil)` if it fails
pub fn log(log: Log, db: pog.Connection) -> Result(Nil, Nil) {
  do_log(db, log) |> result.map(fn(_) { Nil })
}

/// Adds a log to DB and passes it to the erlang logger. Crashes immediatly after.
pub fn log_and_crash(log: Log, db: pog.Connection) -> Result(Nil, Nil) {
  let log = log |> critical()
  do_log(db, log) |> result.map(fn(_) { Nil })
}
