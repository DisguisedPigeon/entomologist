import gleam/int
import gleam/string
import gleam/string_tree
import pog

/// Utility function to turn a pog QueryError error into a readable log and add the location of the failure.
pub fn describe_error(
  error error: pog.QueryError,
  location origin: String,
) -> String {
  let prefix = string_tree.from_strings(["Query: ", origin, " - "])
  let error_description = case error {
    pog.ConstraintViolated(message:, constraint:, detail:) ->
      string_tree.from_strings([
        "Constraint violated: ", constraint, " - ", message, " - ", detail,
      ])
    pog.PostgresqlError(code:, name:, message:) ->
      string_tree.from_strings([
        "Postgres error: (", code, ")", name, " - ", message,
      ])
    pog.UnexpectedArgumentCount(expected:, got:) ->
      string_tree.from_strings([
        "Argument quantity mismatch, expected ",
        int.to_string(expected),
        ", got ",
        int.to_string(got),
        ".\nThis is probably a internal entomologist error.",
      ])
    pog.UnexpectedArgumentType(expected:, got:) ->
      string_tree.from_strings([
        "Argument type mismatch, expected ",
        expected,
        ", got ",
        got,
        ".\nThis is probably a internal entomologist error.",
      ])
    pog.UnexpectedResultType(decode_errors) -> {
      string_tree.from_strings([
        "Unable to decode result type: ",
        string.inspect(decode_errors),
        ".\nThis is probably a internal entomologist error.",
      ])
    }
    pog.QueryTimeout -> string_tree.from_string("Database query timed out")
    pog.ConnectionUnavailable ->
      string_tree.from_string("Connection to database unavaliable")
  }

  string_tree.concat([prefix, error_description])
  |> string_tree.to_string
}
