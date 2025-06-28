//// When running the tests, a postgres database should be running.
////
//// This is the expected config for that postgres DB:
//// - Port "5431" or the POSTGRES_PORT env variable
//// - Running on "localhost"
//// - User "postgres"
//// - Password set to "postgres"
//// - Database named "test"

import entomologist
import entomologist/internal/logger_api
import envoy
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/charlist
import gleam/int
import gleam/option
import gleam/result
import gleam/string
import gleeunit
import logging
import pog

type AuxAtoms {
  Meta
  MsgDict
  Level
  Rest
  Time
  Function
  Module
  Line
  Message
}

pub fn main() {
  // Creates the tests' DB conection.
  let connection = create_connection()

  // HACK : Creates a erlang GenServer storing the connection for later usage.
  //        Maybe this isn't the best solution...
  set_connection(connection)

  let assert Ok(Nil) = entomologist.configure(connection)
  let Nil = logging.configure()
  let Nil = logging.set_level(logging.Debug)

  create_tables(connection)

  gleeunit.main()
}

/// For this test to pass, all of these must work:
/// - log creation (with text as a message)
/// - the configure function (called in main)
pub fn string_message_test() {
  use connection <- transactional()
  let message = "string_message_test message"

  // Log a Info message
  logging.log(logging.Info, message)

  // DB checks.
  // This looks weird, the goal is to reduce noise while keeping the debug info about the assert location.
  use count, query <- execute_query("select count(*) from logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- execute_query(
    "select count(*) from occurrences",
    connection,
  )
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  Error("rollback transaction")
}

/// For this test to pass, all of these must work:
/// - log creation (with text as a message)
/// - the configure function (called in main)
/// - grouping
pub fn string_message_grouping_test() {
  use connection <- transactional()
  let message = "string_message_grouping_test message"

  // Log a Info message
  logging.log(logging.Info, message)

  // DB checks
  // This looks weird, the goal is to reduce noise while keeping the debug info about the assert location.
  use count, query <- execute_query("select count(*) from logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }
  use count, query <- execute_query(
    "select count(*) from occurrences",
    connection,
  )
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  // Log a Info message
  logging.log(logging.Info, message)

  // DB checks
  use count, query <- execute_query("select count(*) from logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- execute_query(
    "select count(*) from occurrences",
    connection,
  )
  assert 2 == count as { "while executing\"" <> query <> "\"" }

  Error("rollback transaction")
}

/// For this test to pass, all of these must work:
/// - report creation
/// - the configure function (called in main)
pub fn report_message_test() {
  // Create a log with a dict message instead of a simple string.
  // This is called a report.
  let report = fn(timestamp: Int) {
    to_dynamic(
      dict.from_list([
        #(
          to_dynamic(Meta),
          to_dynamic(
            dict.from_list([#(to_dynamic(Time), to_dynamic(timestamp))]),
          ),
        ),
        #(
          to_dynamic(MsgDict),
          to_dynamic(
            dict.from_list([
              #(Function, to_dynamic("fn")),
              #(Module, to_dynamic("mod")),
              #(Line, to_dynamic(10)),
              #(Message, to_dynamic("hallo")),
            ]),
          ),
        ),
        #(to_dynamic(Level), to_dynamic(charlist.from_string("info"))),
        #(to_dynamic(Rest), to_dynamic("{}")),
      ]),
    )
  }

  use connection <- transactional()

  let timestamp = 10_000_000_000_000_000

  report(timestamp) |> logger_api.save_to_db(connection)

  use count, query <- execute_query("select count(*) from logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- execute_query(
    "select count(*) from occurrences",
    connection,
  )
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  Error("rollback transaction")
}

/// For this test to pass, all of these must work:
/// - report creation
/// - report grouping
/// - the configure function (called in main)
pub fn report_message_gropuing_test() {
  // Create a log with a dict message instead of a simple string.This is called a report.
  let log = fn(timestamp: Int) {
    to_dynamic(
      dict.from_list([
        #(
          to_dynamic(Meta),
          to_dynamic(
            dict.from_list([#(to_dynamic(Time), to_dynamic(timestamp))]),
          ),
        ),
        #(
          to_dynamic(MsgDict),
          to_dynamic(
            dict.from_list([
              #(Function, to_dynamic("fn")),
              #(Module, to_dynamic("mod")),
              #(Line, to_dynamic(10)),
              #(Message, to_dynamic("hallo")),
            ]),
          ),
        ),
        #(to_dynamic(Level), to_dynamic(charlist.from_string("info"))),
        #(to_dynamic(Rest), to_dynamic("{}")),
      ]),
    )
  }

  use connection <- transactional()

  let timestamp = 10_000_000_000_000_000

  log(timestamp) |> logger_api.save_to_db(connection)

  use count, query <- execute_query("select count(*) from logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- execute_query(
    "select count(*) from occurrences",
    connection,
  )
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  log(1 + timestamp) |> logger_api.save_to_db(connection)

  use count, query <- execute_query("select count(*) from logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- execute_query(
    "select count(*) from occurrences",
    connection,
  )
  assert 2 == count as { "while executing\"" <> query <> "\"" }

  Error("rollback transaction")
}

@external(erlang, "test_ffi", "id")
fn to_dynamic(val: a) -> decode.Dynamic

@external(erlang, "test_ffi", "get")
fn get_connection() -> pog.Connection

@external(erlang, "test_ffi", "set")
fn set_connection(conneciton: pog.Connection) -> Nil

fn create_connection() -> pog.Connection {
  let port = case envoy.get("POSTGRES_PORT") {
    Ok(v) ->
      result.lazy_unwrap(int.parse(v), fn() {
        panic as { "failed parsing port from: " <> string.inspect(v) }
      })
    Error(_) -> 5431
  }

  pog.default_config()
  |> pog.port(port)
  |> pog.host("localhost")
  |> pog.user("postgres")
  |> pog.password(option.Some("postgres"))
  |> pog.database("test")
  |> pog.pool_size(10)
  |> pog.connect
}

fn create_tables(connection: pog.Connection) -> Nil {
  case
    "
    create type level as enum (
        'emergency',
        'alert',
        'critical',
        'error',
        'warning',
        'notice',
        'info',
        'debug'
    )"
    |> pog.query
    |> pog.execute(connection)
  {
    Ok(_) -> Nil
    Error(pog.PostgresqlError(
      "42710",
      "duplicate_object",
      "type \"level\" already exists",
    )) -> Nil
    Error(e) -> panic as { "unexpected error: " <> string.inspect(e) }
  }
  let assert Ok(_) =
    "
    create table if not exists logs (
        id bigserial not null unique primary key,
        message text not null,
        level level not null,
        module text not null,
        function text not null,
        arity int not null,
        file text not null,
        line int not null,
        resolved bool not null default false,
        last_occurrence bigint not null,
        snoozed bool not null default false
    )"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "
    create table if not exists occurrences (
        id bigserial not null unique primary key,
        log bigint references logs(id) on delete cascade,
        timestamp bigint not null,
        full_contents json
    )"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "delete from occurrences"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "delete from logs"
    |> pog.query
    |> pog.execute(connection)

  Nil
}

fn transactional(callback: fn(pog.Connection) -> Result(a, String)) -> Nil {
  let assert Error(_) = {
    use connection <- pog.transaction(get_connection())

    let assert Ok(_) =
      pog.query("set transaction isolation level serializable")
      |> pog.execute(connection)

    callback(connection)
  }

  Nil
}

fn execute_query(
  query: String,
  connection: pog.Connection,
  cb: fn(Int, String) -> Result(a, String),
) -> Result(a, String) {
  let assert Ok(pog.Returned(1, [[count]])) =
    pog.query(query)
    |> pog.returning(decode.list(decode.int))
    |> pog.execute(connection)

  cb(count, query)
}
