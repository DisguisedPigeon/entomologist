import gleam/dynamic/decode
import gleam/json
import pog

/// Runs the `add_occurrence` query
/// defined in `./src/entomologist/sql/add_occurrence.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_occurrence(
  db,
  arg_1,
  arg_2,
  arg_3,
  arg_4,
  arg_5,
  arg_6,
  arg_7,
  arg_8,
) {
  let decoder = decode.map(decode.dynamic, fn(_) { Nil })

  "insert into
    occurrences(error, reason, context, module, function, arity, file, line)
values
    ($1, $2, $3, $4, $5, $6, $7, $8);
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(json.to_string(arg_3)))
  |> pog.parameter(pog.text(arg_4))
  |> pog.parameter(pog.text(arg_5))
  |> pog.parameter(pog.int(arg_6))
  |> pog.parameter(pog.text(arg_7))
  |> pog.parameter(pog.int(arg_8))
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
  arg_10,
  arg_11,
  arg_12,
  arg_13,
  arg_14,
) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddErrorRow(id:))
  }

  "with error_id as (
    insert into errors (
        message, level, module, function, resolved, last_occurrence, muted
    ) values (
        $1, $2, $3, $4, $5, $6, $7
    ) returning id
)
insert into occurrences (
    error,
    reason, context, module, function, arity, file, line
) values (
    (select id from error_id),
    $8, $9, $10, $11, $12, $13, $14
) returning id
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.parameter(pog.bool(arg_5))
  |> pog.parameter(pog.timestamp(arg_6))
  |> pog.parameter(pog.bool(arg_7))
  |> pog.parameter(pog.text(arg_8))
  |> pog.parameter(pog.text(json.to_string(arg_9)))
  |> pog.parameter(pog.text(arg_10))
  |> pog.parameter(pog.text(arg_11))
  |> pog.parameter(pog.int(arg_12))
  |> pog.parameter(pog.text(arg_13))
  |> pog.parameter(pog.int(arg_14))
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
  ExistErrorRow(count: Int)
}

/// Runs the `exist_error` query
/// defined in `./src/entomologist/sql/exist_error.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn exist_error(db, arg_1, arg_2, arg_3, arg_4) {
  let decoder = {
    use count <- decode.field(0, decode.int)
    decode.success(ExistErrorRow(count:))
  }

  "select count(1)
from errors
where message = $1
    and level = $2
    and function = $3
    and module = $4
    and resolved = false;
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.returning(decoder)
  |> pog.execute(db)
}
