import gleam/dynamic/decode
import pog

/// A row you get from running the `add_log_full` query
/// defined in `./src/entomologist/sql/add_log_full.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddLogFullRow {
  AddLogFullRow(id: Int)
}

/// Runs the `add_log_full` query
/// defined in `./src/entomologist/sql/add_log_full.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_log_full(db, arg_1, arg_2, arg_3, arg_4) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddLogFullRow(id:))
  }

  "insert into logs(
	level,
	title,
	description,
	code
)
values (
	$1, $2, $3, $4
)
returning id;
"
  |> pog.query
  |> pog.parameter(level_encoder(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.int(arg_4))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `add_log_desc` query
/// defined in `./src/entomologist/sql/add_log_desc.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddLogDescRow {
  AddLogDescRow(id: Int)
}

/// Runs the `add_log_desc` query
/// defined in `./src/entomologist/sql/add_log_desc.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_log_desc(db, arg_1, arg_2, arg_3) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddLogDescRow(id:))
  }

  "insert into logs(
	level,
	title,
	description
)
values (
	$1, $2, $3
)
returning id;
"
  |> pog.query
  |> pog.parameter(level_encoder(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `add_log_code` query
/// defined in `./src/entomologist/sql/add_log_code.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddLogCodeRow {
  AddLogCodeRow(id: Int)
}

/// Runs the `add_log_code` query
/// defined in `./src/entomologist/sql/add_log_code.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_log_code(db, arg_1, arg_2, arg_3) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddLogCodeRow(id:))
  }

  "insert into logs(
	level,
	title,
	code
)
values (
	$1, $2, $3
)
returning id;
"
  |> pog.query
  |> pog.parameter(level_encoder(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.int(arg_3))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `add_log` query
/// defined in `./src/entomologist/sql/add_log.sql`.
///
/// > 🐿️ This type definition was generated automatically using v3.0.3 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AddLogRow {
  AddLogRow(id: Int)
}

/// Runs the `add_log` query
/// defined in `./src/entomologist/sql/add_log.sql`.
///
/// > 🐿️ This function was generated automatically using v3.0.3 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn add_log(db, arg_1, arg_2) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddLogRow(id:))
  }

  "insert into logs(
	level,
	title
)
values (
	$1, $2
)
returning id;
"
  |> pog.query
  |> pog.parameter(level_encoder(arg_1))
  |> pog.parameter(pog.text(arg_2))
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
    Debug -> "Debug"
    Info -> "Info"
    Notice -> "Notice"
    Warning -> "Warning"
    Error -> "Error"
    Critical -> "Critical"
    Alert -> "Alert"
    Emergency -> "Emergency"
  }
  |> pog.text
}
