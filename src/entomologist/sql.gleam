import gleam/dynamic/decode
import gleam/json
import pog

/// A row you get from running the `add_occurrence` query
/// defined in `./src/entomologist/sql/add_occurrence.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddOccurrenceRow {
  AddOccurrenceRow(id: Int)
}

/// Runs the `add_occurrence` query
/// defined in `./src/entomologist/sql/add_occurrence.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_occurrence(db, arg_1, arg_2, arg_3) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddOccurrenceRow(id:))
  }

  "insert into
    occurrences(error, timestamp, full_contents)
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

/// Runs the `update_error_timestamp` query
/// defined in `./src/entomologist/sql/update_error_timestamp.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn update_error_timestamp(db, arg_1) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "update errors
set last_occurrence = occurrences.timestamp
from occurrences
where errors.id = occurrences.error and occurrences.id = $1
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `add_error` query
/// defined in `./src/entomologist/sql/add_error.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddErrorRow {
  AddErrorRow(id: Int)
}

/// Runs the `add_error` query
/// defined in `./src/entomologist/sql/add_error.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_error(
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
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddErrorRow(id:))
  }

  "with error as (
    insert into errors (
        message, level, module, function, arity, file, line, last_occurrence
    ) values (
        $1, $2, $3, $4, $5, $6, $7, $8
    ) returning id, last_occurrence
)
insert into occurrences (
    error, timestamp, full_contents
) values (
    (select id from error), (select last_occurrence from error), $9
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

/// A row you get from running the `exist_error` query
/// defined in `./src/entomologist/sql/exist_error.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type ExistErrorRow {
  ExistErrorRow(id: Int)
}

/// Runs the `exist_error` query
/// defined in `./src/entomologist/sql/exist_error.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn exist_error(db, arg_1, arg_2, arg_3, arg_4, arg_5) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(ExistErrorRow(id:))
  }

  "select id
from errors
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

// --- Enums -------------------------------------------------------------------

/// Corresponds to the Postgres `level` enum.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
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

fn level_encoder(variant) {
  case variant {
    Debug -> "debug"
    Info -> "info"
    Notice -> "notice"
    Warning -> "Warning"
    Error -> "error"
    Critical -> "critical"
    Alert -> "alert"
    Emergency -> "emergency"
  }
  |> pog.text
}
