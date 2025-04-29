import entomologist/sql
import exception
import gleam/dict
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/string
import logging
import pog
import wisp.{type Request, type Response}

/// Exit value for the `exception.rescue` function.
/// This type is used for keeping the exit type while unpacking the returned details.
type ErrorKind {
  Errored
  Thrown
  Exited
}

///ok value in erlang, but cooler
type NuhUh

@external(erlang, "logger", "error")
fn log_error_dict(report: dict.Dict(atom.Atom, dynamic.Dynamic)) -> NuhUh

fn atom_dict_decoder() {
  let atom =
    decode.new_primitive_decoder("Atom", fn(data) {
      case atom.from_dynamic(data) {
        Ok(atom) -> Ok(atom)
        Error(_) -> Error(atom.create_from_string("nil"))
      }
    })
  let dynamic = decode.new_primitive_decoder("Dynamic", Ok)
  decode.dict(atom, dynamic)
}

/// This function is a drop-in replacement for `wisp.rescue_crashes`
/// It saves the errors to database and logs them using erlang's logger.
pub fn rescue_and_inspect_crashes(
  request: Request,
  database: pog.Connection,
  callback: fn() -> Response,
) -> Response {
  // Most of this function is a direct copy from wisp's rescue_crashes
  case exception.rescue(callback) {
    Ok(response) -> response
    Error(error) -> {
      let assert Ok(_) =
        sql.add_log_desc(
          database,
          sql.Error,
          "Internal server error",
          "Server crash recovered. Crash details:
          " <> string.inspect(error),
        )

      let #(kind, detail) = case error {
        exception.Errored(details) -> #(Errored, details)
        exception.Thrown(details) -> #(Thrown, details)
        exception.Exited(details) -> #(Exited, details)
      }

      case decode.run(detail, atom_dict_decoder()) {
        Ok(details) -> {
          let c = atom.create_from_string("class")
          let _ = #(request, database)
          log_error_dict(dict.insert(details, c, dynamic.from(kind)))
          Nil
        }
        Error(_) -> {
          let _ = #(request, database)
          logging.log(logging.Error, string.inspect(error))
        }
      }
      // return 500: Internal server error
      wisp.response(500)
    }
  }
}
