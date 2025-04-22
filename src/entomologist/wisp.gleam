import pog
import wisp.{type Request, type Response}

/// This function is a drop-in replacement for `wisp.rescue_crashes`
/// It saves the errors to database and logs them using erlang's logger through the `logging` package.
pub fn rescue_and_inspect_crashes(
  request: Request,
  database: pog.Connection,
  callback: fn() -> Response,
) -> Response {
  let _ = #(request, database, callback)
  todo as "implement"
}
