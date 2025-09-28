import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option}
import pog

/// A row you get from running the `show` query
/// defined in `./src/entomologist/internal/sql/show.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type ShowRow {
  ShowRow(
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

/// Runs the `show` query
/// defined in `./src/entomologist/internal/sql/show.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn show(db) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use module <- decode.field(3, decode.string)
    use function <- decode.field(4, decode.string)
    use arity <- decode.field(5, decode.int)
    use file <- decode.field(6, decode.string)
    use line <- decode.field(7, decode.int)
    use resolved <- decode.field(8, decode.bool)
    use last_occurrence <- decode.field(9, decode.int)
    use snoozed <- decode.field(10, decode.bool)
    decode.success(ShowRow(
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
    ))
  }

  "select * from logs
where resolved = false and snoozed = false;
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `logs` query
/// defined in `./src/entomologist/internal/sql/logs.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type LogsRow {
  LogsRow(
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

/// Runs the `logs` query
/// defined in `./src/entomologist/internal/sql/logs.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn logs(db, arg_1) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use module <- decode.field(3, decode.string)
    use function <- decode.field(4, decode.string)
    use arity <- decode.field(5, decode.int)
    use file <- decode.field(6, decode.string)
    use line <- decode.field(7, decode.int)
    use resolved <- decode.field(8, decode.bool)
    use last_occurrence <- decode.field(9, decode.int)
    use snoozed <- decode.field(10, decode.bool)
    decode.success(LogsRow(
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
    ))
  }

  "select * from logs
where id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `snooze_log` query
/// defined in `./src/entomologist/internal/sql/snooze_log.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn snooze_log(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update logs set snoozed = true where id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `exist_log` query
/// defined in `./src/entomologist/internal/sql/exist_log.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type ExistLogRow {
  ExistLogRow(id: Int)
}

/// Runs the `exist_log` query
/// defined in `./src/entomologist/internal/sql/exist_log.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn exist_log(db, arg_1, arg_2, arg_3, arg_4, arg_5) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(ExistLogRow(id:))
  }

  "select id
from logs
where message = $1
    and level = $2
    and function = $3
    and module = $4
    and arity = $5
    and resolved = false;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(level_encoder(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.parameter(pog.int(arg_5))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `update_log_timestamp` query
/// defined in `./src/entomologist/internal/sql/update_log_timestamp.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn update_log_timestamp(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update logs
set last_occurrence = occurrences.timestamp
from occurrences
where logs.id = occurrences.log and occurrences.id = $1
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `snoozed` query
/// defined in `./src/entomologist/internal/sql/snoozed.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type SnoozedRow {
  SnoozedRow(
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

/// Runs the `snoozed` query
/// defined in `./src/entomologist/internal/sql/snoozed.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn snoozed(db) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use module <- decode.field(3, decode.string)
    use function <- decode.field(4, decode.string)
    use arity <- decode.field(5, decode.int)
    use file <- decode.field(6, decode.string)
    use line <- decode.field(7, decode.int)
    use resolved <- decode.field(8, decode.bool)
    use last_occurrence <- decode.field(9, decode.int)
    use snoozed <- decode.field(10, decode.bool)
    decode.success(SnoozedRow(
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
    ))
  }

  "select * from logs
where resolved = false and snoozed = true;
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `add_occurrence` query
/// defined in `./src/entomologist/internal/sql/add_occurrence.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddOccurrenceRow {
  AddOccurrenceRow(id: Int)
}

/// Runs the `add_occurrence` query
/// defined in `./src/entomologist/internal/sql/add_occurrence.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_occurrence(db, arg_1, arg_2, arg_3) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddOccurrenceRow(id:))
  }

  "insert into
    occurrences(log, timestamp, full_contents)
values
    ($1, $2, $3)
returning id;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.parameter(pog.int(arg_2))
  |> pog.parameter(pog.text(json.to_string(arg_3)))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `solved` query
/// defined in `./src/entomologist/internal/sql/solved.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type SolvedRow {
  SolvedRow(
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

/// Runs the `solved` query
/// defined in `./src/entomologist/internal/sql/solved.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn solved(db) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use module <- decode.field(3, decode.string)
    use function <- decode.field(4, decode.string)
    use arity <- decode.field(5, decode.int)
    use file <- decode.field(6, decode.string)
    use line <- decode.field(7, decode.int)
    use resolved <- decode.field(8, decode.bool)
    use last_occurrence <- decode.field(9, decode.int)
    use snoozed <- decode.field(10, decode.bool)
    decode.success(SolvedRow(
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
    ))
  }

  "select * from logs
where resolved = true and snoozed = true;
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `resolve_log` query
/// defined in `./src/entomologist/internal/sql/resolve_log.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn resolve_log(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update logs set (resolved , snoozed) = (true, false)
where id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `add_log` query
/// defined in `./src/entomologist/internal/sql/add_log.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddLogRow {
  AddLogRow(id: Int)
}

/// Runs the `add_log` query
/// defined in `./src/entomologist/internal/sql/add_log.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_log(
  db,
  arg_1,
  arg_2,
  arg_3,
  arg_4,
  arg_5,
  arg_6,
  arg_7,
  arg_8,
  arg_9,
) {
  let decoder =
  {
    use id <- decode.field(0, decode.int)
    decode.success(AddLogRow(id:))
  }

  "with log as (
    insert into logs (
        message, level, module, function, arity, file, line, last_occurrence
    ) values (
        $1, $2, $3, $4, $5, $6, $7, $8
    ) returning id, last_occurrence
)
insert into occurrences (
    log, timestamp, full_contents
) values (
    (select id from log), (select last_occurrence from log), $9
) returning id
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(level_encoder(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.parameter(pog.int(arg_5))
  |> pog.parameter(pog.text(arg_6))
  |> pog.parameter(pog.int(arg_7))
  |> pog.parameter(pog.int(arg_8))
  |> pog.parameter(pog.text(json.to_string(arg_9)))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `occurrences` query
/// defined in `./src/entomologist/internal/sql/occurrences.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type OccurrencesRow {
  OccurrencesRow(
    id: Int,
    log: Option(Int),
    timestamp: Int,
    full_contents: Option(String),
  )
}

/// Runs the `occurrences` query
/// defined in `./src/entomologist/internal/sql/occurrences.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn occurrences(db, arg_1) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use log <- decode.field(1, decode.optional(decode.int))
    use timestamp <- decode.field(2, decode.int)
    use full_contents <- decode.field(3, decode.optional(decode.string))
    decode.success(OccurrencesRow(id:, log:, timestamp:, full_contents:))
  }

  "select * from occurrences
where log = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `wake_up_log` query
/// defined in `./src/entomologist/internal/sql/wake_up_log.sql`.
///
/// > 🐿️ This function was generated automatically using v3.1.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn wake_up_log(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update logs set snoozed = false where id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

// --- Enums -------------------------------------------------------------------

/// Corresponds to the Postgres `level` enum.
///
/// > 🐿️ This type definition was generated automatically using v3.1.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type Level {
  Debug
  Info
  Notice
  Warning
  Error
  Critical
  Alert
  Emergency
}

fn level_decoder() {
  use level <- decode.then(decode.string)
  case level {
    "debug" -> decode.success(Debug)
    "info" -> decode.success(Info)
    "notice" -> decode.success(Notice)
    "warning" -> decode.success(Warning)
    "error" -> decode.success(Error)
    "critical" -> decode.success(Critical)
    "alert" -> decode.success(Alert)
    "emergency" -> decode.success(Emergency)
    _ -> decode.failure(Debug, "Level")
  }
}

fn level_encoder(level) {
  case level {
    Debug -> "debug"
    Info -> "info"
    Notice -> "notice"
    Warning -> "warning"
    Error -> "error"
    Critical -> "critical"
    Alert -> "alert"
    Emergency -> "emergency"
  }
  |> pog.text
}
