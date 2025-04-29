import gleam/dynamic/decode
import pog

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
pub fn add_log(db, arg_1, arg_2, arg_3, arg_4) {
  let decoder = {
    use id <- decode.field(0, decode.int)
    decode.success(AddLogRow(id:))
  }

  "insert into logs(
	level,
	code,
	title,
	description
)
values (
	$1, $2, $3, $4
)
returning id;
"
  |> pog.query
  |> pog.parameter(level_encoder(arg_1))
  |> pog.parameter(pog.int(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
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
  WarningNotice
  Error
  Critical
  Alert
  Emergency
}

fn level_encoder(variant) {
  case variant {
    Debug -> "Debug"
    Info -> "Info"
    WarningNotice -> "WarningNotice"
    Error -> "Error"
    Critical -> "Critical"
    Alert -> "Alert"
    Emergency -> "Emergency"
  }
  |> pog.text
}
