//// This module contains the code to run the sql queries defined in
//// `./src/entomologist/internal/sql`.
//// > 🐿️ This module was generated automatically using v4.6.0 of
//// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
////

import gleam/dynamic/decode
import gleam/json.{type Json}
import gleam/option.{type Option}
import pog

/// A row you get from running the `add_log` query
/// defined in `./src/entomologist/internal/sql/add_log.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddLogRow {
  AddLogRow(id: Int)
}

/// Runs the `add_log` query
/// defined in `./src/entomologist/internal/sql/add_log.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_log(
  db: pog.Connection,
  arg_1: String,
  arg_2: Level,
  arg_3: String,
  arg_4: String,
  arg_5: Int,
  arg_6: String,
  arg_7: Int,
  arg_8: Int,
  arg_9: Json,
) -> Result(pog.Returned(AddLogRow), pog.QueryError) {
  let decoder = {
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

/// A row you get from running the `add_occurrence` query
/// defined in `./src/entomologist/internal/sql/add_occurrence.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddOccurrenceRow {
  AddOccurrenceRow(id: Int)
}

/// Runs the `add_occurrence` query
/// defined in `./src/entomologist/internal/sql/add_occurrence.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_occurrence(
  db: pog.Connection,
  arg_1: Int,
  arg_2: Int,
  arg_3: Json,
) -> Result(pog.Returned(AddOccurrenceRow), pog.QueryError) {
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

/// Runs the `add_tag` query
/// defined in `./src/entomologist/internal/sql/add_tag.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_tag(
  db: pog.Connection,
  arg_1: Int,
  arg_2: Int,
) -> Result(pog.Returned(Nil), pog.QueryError) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "insert into log2tag (log, tag)
values ( $1, $2 );
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.parameter(pog.int(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_tag` query
/// defined in `./src/entomologist/internal/sql/create_tag.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateTagRow {
  CreateTagRow(id: Int)
}

/// Runs the `create_tag` query
/// defined in `./src/entomologist/internal/sql/create_tag.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_tag(
  db: pog.Connection,
  arg_1: String,
) -> Result(pog.Returned(CreateTagRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(CreateTagRow(id:))
  }

  "insert into tags (name)
values ($1)
returning id;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `exist_log` query
/// defined in `./src/entomologist/internal/sql/exist_log.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type ExistLogRow {
  ExistLogRow(id: Int)
}

/// Runs the `exist_log` query
/// defined in `./src/entomologist/internal/sql/exist_log.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn exist_log(
  db: pog.Connection,
  arg_1: String,
  arg_2: Level,
  arg_3: String,
  arg_4: String,
  arg_5: Int,
) -> Result(pog.Returned(ExistLogRow), pog.QueryError) {
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

/// A row you get from running the `find_tag` query
/// defined in `./src/entomologist/internal/sql/find_tag.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type FindTagRow {
  FindTagRow(id: Int)
}

/// Runs the `find_tag` query
/// defined in `./src/entomologist/internal/sql/find_tag.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn find_tag(
  db: pog.Connection,
  arg_1: String,
) -> Result(pog.Returned(FindTagRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(FindTagRow(id:))
  }

  "select id
from tags
where name like $1;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `log_data` query
/// defined in `./src/entomologist/internal/sql/log_data.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type LogDataRow {
  LogDataRow(
    id: Int,
    message: String,
    level: Level,
    module: String,
    function: String,
    arity: Int,
    file: String,
    line: Int,
    last_occurrence: Int,
    resolved: Bool,
    muted: Bool,
  )
}

/// Runs the `log_data` query
/// defined in `./src/entomologist/internal/sql/log_data.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn log_data(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(LogDataRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use module <- decode.field(3, decode.string)
    use function <- decode.field(4, decode.string)
    use arity <- decode.field(5, decode.int)
    use file <- decode.field(6, decode.string)
    use line <- decode.field(7, decode.int)
    use last_occurrence <- decode.field(8, decode.int)
    use resolved <- decode.field(9, decode.bool)
    use muted <- decode.field(10, decode.bool)
    decode.success(LogDataRow(
      id:,
      message:,
      level:,
      module:,
      function:,
      arity:,
      file:,
      line:,
      last_occurrence:,
      resolved:,
      muted:,
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

/// A row you get from running the `log_tags` query
/// defined in `./src/entomologist/internal/sql/log_tags.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type LogTagsRow {
  LogTagsRow(name: String)
}

/// Runs the `log_tags` query
/// defined in `./src/entomologist/internal/sql/log_tags.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn log_tags(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(LogTagsRow), pog.QueryError) {
  let decoder = {
    use name <- decode.field(0, decode.string)
    decode.success(LogTagsRow(name:))
  }

  "select name
from tags as t
join log2tag as lt on t.id = lt.tag
join logs as l on lt.log = l.id
where l.id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `logs` query
/// defined in `./src/entomologist/internal/sql/logs.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type LogsRow {
  LogsRow(id: Int, level: Level, message: String, last_occurrence: Int)
}

/// Runs the `logs` query
/// defined in `./src/entomologist/internal/sql/logs.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn logs(
  db: pog.Connection,
) -> Result(pog.Returned(LogsRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use level <- decode.field(1, level_decoder())
    use message <- decode.field(2, decode.string)
    use last_occurrence <- decode.field(3, decode.int)
    decode.success(LogsRow(id:, level:, message:, last_occurrence:))
  }

  "select id, level, message, last_occurrence
from logs;
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `mute_log` query
/// defined in `./src/entomologist/internal/sql/mute_log.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn mute_log(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(Nil), pog.QueryError) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update logs
set muted = true
where id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `muted` query
/// defined in `./src/entomologist/internal/sql/muted.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type MutedRow {
  MutedRow(id: Int, message: String, level: Level, last_occurrence: Int)
}

/// Runs the `muted` query
/// defined in `./src/entomologist/internal/sql/muted.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn muted(
  db: pog.Connection,
) -> Result(pog.Returned(MutedRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use last_occurrence <- decode.field(3, decode.int)
    decode.success(MutedRow(id:, message:, level:, last_occurrence:))
  }

  "select id, message, level, last_occurrence
from logs
where resolved = false and muted = true;
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `occurrences` query
/// defined in `./src/entomologist/internal/sql/occurrences.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type OccurrencesRow {
  OccurrencesRow(id: Int, timestamp: Int, full_contents: Option(String))
}

/// Runs the `occurrences` query
/// defined in `./src/entomologist/internal/sql/occurrences.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn occurrences(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(OccurrencesRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use timestamp <- decode.field(1, decode.int)
    use full_contents <- decode.field(2, decode.optional(decode.string))
    decode.success(OccurrencesRow(id:, timestamp:, full_contents:))
  }

  "select id, timestamp, full_contents
from occurrences
where log = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `resolve_log` query
/// defined in `./src/entomologist/internal/sql/resolve_log.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn resolve_log(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(Nil), pog.QueryError) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update logs
set (resolved , muted) = (true, false)
where id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `search_log` query
/// defined in `./src/entomologist/internal/sql/search_log.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type SearchLogRow {
  SearchLogRow(id: Int, message: String, level: Level, last_occurrence: Int)
}

/// Since nullability is not detected by squirrel, I'll have to give up type-safety and implement this query in gleam on a custom function in custom_sql.gleam.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn search_log(
  db: pog.Connection,
  arg_1: String,
  arg_2: Level,
  arg_3: String,
  arg_4: String,
  arg_5: Int,
  arg_6: String,
  arg_7: Int,
  arg_8: Bool,
  arg_9: Int,
  arg_10: Bool,
) -> Result(pog.Returned(SearchLogRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use last_occurrence <- decode.field(3, decode.int)
    decode.success(SearchLogRow(id:, message:, level:, last_occurrence:))
  }

  "-- Since nullability is not detected by squirrel, I'll have to give up type-safety and implement this query in gleam on a custom function in custom_sql.gleam.
select id, message, level, last_occurrence
from logs
where ($1::text is null or LOWER(message) LIKE $1)
  and ($2::level is null or level = $2)
  and ($3::text is null or LOWER(module) LIKE $3)
  and ($4::text is null or LOWER(function) LIKE $4)
  and ($5::int is null or arity = $5)
  and ($6::text is null or LOWER(file) LIKE $6)
  and ($7::int is null or line = $7)
  and ($8::bool is null or resolved = $8)
  and ($9::bigint is null or last_occurrence = $9)
  and ($10::bool is null or muted = $10)
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(level_encoder(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.parameter(pog.int(arg_5))
  |> pog.parameter(pog.text(arg_6))
  |> pog.parameter(pog.int(arg_7))
  |> pog.parameter(pog.bool(arg_8))
  |> pog.parameter(pog.int(arg_9))
  |> pog.parameter(pog.bool(arg_10))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `show` query
/// defined in `./src/entomologist/internal/sql/show.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type ShowRow {
  ShowRow(id: Int, message: String, level: Level, last_occurrence: Int)
}

/// Runs the `show` query
/// defined in `./src/entomologist/internal/sql/show.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn show(
  db: pog.Connection,
) -> Result(pog.Returned(ShowRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use last_occurrence <- decode.field(3, decode.int)
    decode.success(ShowRow(id:, message:, level:, last_occurrence:))
  }

  "select id, message, level, last_occurrence
from logs
where resolved = false and muted = false;
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `solved` query
/// defined in `./src/entomologist/internal/sql/solved.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type SolvedRow {
  SolvedRow(id: Int, message: String, level: Level, last_occurrence: Int)
}

/// Runs the `solved` query
/// defined in `./src/entomologist/internal/sql/solved.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn solved(
  db: pog.Connection,
) -> Result(pog.Returned(SolvedRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    use message <- decode.field(1, decode.string)
    use level <- decode.field(2, level_decoder())
    use last_occurrence <- decode.field(3, decode.int)
    decode.success(SolvedRow(id:, message:, level:, last_occurrence:))
  }

  "select id, message, level, last_occurrence
from logs
where resolved = true and muted = true;
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `unmute` query
/// defined in `./src/entomologist/internal/sql/unmute.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn unmute(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(Nil), pog.QueryError) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update logs set muted = false where id = $1;
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// Runs the `update_log_timestamp` query
/// defined in `./src/entomologist/internal/sql/update_log_timestamp.sql`.
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn update_log_timestamp(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(Nil), pog.QueryError) {
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

// --- Enums -------------------------------------------------------------------

/// Corresponds to the Postgres `level` enum.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
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

fn level_decoder() -> decode.Decoder(Level) {
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

fn level_encoder(level) -> pog.Value {
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
