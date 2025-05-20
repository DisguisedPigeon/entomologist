import entomologist/sql.{
  type Level, Alert, Critical, Debug, Emergency, Info, Notice, Warning,
}
import gleam
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import pog

fn level_decoder() -> decode.Decoder(Level) {
  use variant <- decode.then(decode.string)
  case variant {
    "emergency" -> decode.success(Emergency)
    "alert" -> decode.success(Alert)
    "critical" -> decode.success(Critical)
    "error" -> decode.success(sql.Error)
    "warning" -> decode.success(Warning)
    "notice" -> decode.success(Notice)
    "info" -> decode.success(Info)
    "debug" -> decode.success(Debug)
    _ -> decode.success(Warning)
  }
}

type Timestamp =
  Int

type Metadata {
  Metadata(
    time: Timestamp,
    module: Option(String),
    function: Option(String),
    arity: Option(Int),
    file: Option(String),
    line: Option(Int),
  )
}

fn metadata_decoder() -> decode.Decoder(Metadata) {
  use time <- decode.field("time", decode.int)
  use module <- decode.field("module", decode.optional(decode.string))
  use function <- decode.field("function", decode.optional(decode.string))
  use arity <- decode.field("arity", decode.optional(decode.int))
  use file <- decode.field("file", decode.optional(decode.string))
  use line <- decode.field("line", decode.optional(decode.int))
  decode.success(Metadata(time:, module:, function:, arity:, file:, line:))
}

type Log {
  Log(level: Level, msg: String, meta: Metadata, rest: String)
}

fn log_decoder() -> decode.Decoder(Log) {
  use level <- decode.field("level", level_decoder())
  use msg <- decode.field("msg", decode.string)
  use meta <- decode.field("meta", metadata_decoder())
  use rest <- decode.field("json", decode.string)
  decode.success(Log(level:, msg:, meta:, rest:))
}

/// Configuring function for the erlang logger.
///
/// It registers the library as a logging handler and saves the DB connection for later usage.
@external(erlang, "entomologist_logger_ffi", "configure")
pub fn configure(connection: pog.Connection) -> Result(Nil, Nil)

/// Function called by the erlang logger module when a log is generated.
///
/// This should never be called by a user.
pub fn save_to_db(
  log: decode.Dynamic,
  connection: pog.Connection,
) -> Result(Nil, String) {
  let to_string = fn(returned) { fn(_ignored) { returned } }

  case decode.run(log, log_decoder()) {
    Ok(Log(
      level:,
      msg:,
      meta: Metadata(time:, module:, function:, arity:, file:, line:),
      rest:,
    )) -> {
      let assert [function, module, file] =
        [function, module, file] |> list.map(option.unwrap(_, "-"))
      let assert [arity, line] = [arity, line] |> list.map(option.unwrap(_, -1))
      let json = json.string(rest)

      case sql.exist_error(connection, msg, level, function, module, arity) {
        // There is already an error for that occurrence. We have encountered it before.
        Ok(pog.Returned(1, [sql.ExistErrorRow(id: error_id)])) ->
          {
            let query_result =
              sql.add_occurrence(connection, error_id, time, json)
              |> result.map_error(to_string("Failed creating occurrence."))

            use value <- result.try(query_result)

            case value {
              pog.Returned(count: 1, rows: [sql.AddOccurrenceRow(id)]) ->
                sql.update_error_timestamp(connection, id)
                |> result.map_error(to_string(
                  "Failed updating the error timestamp to the new occurrence.",
                ))

              _ ->
                panic as "this should never happen. Only one occurrence should be created."
            }
          }
          |> result.map(fn(_) { Nil })

        // The error isn't there. It's the first time we encounter that kind of error
        Ok(pog.Returned(0, [])) -> {
          let query_result =
            sql.add_error(
              connection,
              msg,
              level,
              module,
              function,
              arity,
              file,
              line,
              time,
              json,
            )
            |> result.map_error(to_string("failed creating error"))

          use value <- result.try(query_result)
          case value {
            pog.Returned(1, [sql.AddErrorRow(_)]) -> Ok(Nil)
            _ ->
              panic as "This should not happen. A error creation only returns a ID"
          }
        }

        Ok(_) ->
          panic as "This should not happen. There should only be one active error with a given message and origin."
        gleam.Error(pog.ConnectionUnavailable) ->
          gleam.Error("The database connection was missing")
        gleam.Error(pog.ConstraintViolated(_, _, _)) ->
          gleam.Error("Postgres constraint violated. Invalid data provided.")
        gleam.Error(pog.PostgresqlError(_, _, _)) ->
          gleam.Error("Postgres error.")
        gleam.Error(pog.QueryTimeout) -> gleam.Error("Query timed out.")
        gleam.Error(pog.UnexpectedArgumentCount(_, _)) ->
          gleam.Error("Unexpected argument count.")
        gleam.Error(pog.UnexpectedArgumentType(_, _)) ->
          gleam.Error("Unexpected argument type.")
        gleam.Error(pog.UnexpectedResultType(_)) ->
          gleam.Error("Unexpected result type.")
      }
    }
    Error(_) -> Error("")
  }
}
