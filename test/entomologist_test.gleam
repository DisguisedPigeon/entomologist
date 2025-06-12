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
import gleeunit/should
import logging
import pog

type AuxAtoms {
  Meta
  MsgStr
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
  let connection = get_connection()

  create_tables(connection)
  delete_table_contents(connection)
  let assert Ok(Nil) = entomologist.configure(connection)

  logging.configure()
  logging.set_level(logging.Debug)
  gleeunit.main()
}

pub fn string_message_test() {
  let connection = get_connection()

  use <- transactional(connection)
  let assert Ok(_) =
    "set transaction read write"
    |> pog.query
    |> pog.execute(connection)

  reset_db(connection)

  // Log a Info message
  logging.log(logging.Info, "hallo")

  // DB checks
  check_count("select count(*) from occurrences", 1, connection)

  check_count("select count(*) from errors", 1, connection)

  // Create other occurrence of the same log.
  // This log is created manually to make it merge with the first one.
  logger_api.save_to_db(
    to_dynamic(
      dict.from_list([
        #(
          to_dynamic(Meta),
          to_dynamic(
            dict.from_list([
              #(to_dynamic(Time), to_dynamic(10_000_000_000_000_000)),
            ]),
          ),
        ),
        #(to_dynamic(MsgStr), to_dynamic("hallo")),
        #(to_dynamic(Level), to_dynamic(charlist.from_string("info"))),
        #(to_dynamic(Rest), to_dynamic("{}")),
      ]),
    ),
    connection,
  )

  check_count("select count(*) from occurrences", 2, connection)

  check_count("select count(*) from errors", 1, connection)
}

pub fn report_message_test() {
  let connection = get_connection()

  use <- transactional(connection)

  reset_db(connection)

  // Create an error with a dict report instead of a simple string message.
  // logging.log sadly doesn't support this directly.
  logger_api.save_to_db(
    to_dynamic(
      dict.from_list([
        #(
          to_dynamic(Meta),
          to_dynamic(
            dict.from_list([
              #(to_dynamic(Time), to_dynamic(10_000_000_000_000_000)),
            ]),
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
    ),
    connection,
  )

  check_count("select count(*) from occurrences", 1, connection)
  check_count("select count(*) from errors", 1, connection)
}

fn check_count(string: String, int: Int, connection: pog.Connection) -> Nil {
  pog.query(string)
  |> pog.returning(decode.list(decode.int))
  |> pog.execute(connection)
  |> should.equal(Ok(pog.Returned(1, [[int]])))
}

@external(erlang, "dynamic_ffi", "id")
fn to_dynamic(val: a) -> decode.Dynamic

fn get_connection() -> pog.Connection {
  let port = case envoy.get("POSTGRES_PORT") {
    Ok(v) ->
      result.lazy_unwrap(int.parse(v), fn() {
        let s = "failed parsing port from: " <> string.inspect(v)
        panic as s
      })
    Error(_) -> 5431
  }

  pog.default_config()
  |> pog.port(port)
  |> pog.host("localhost")
  |> pog.user("postgres")
  |> pog.password(option.Some("postgres"))
  |> pog.database("test")
  |> pog.pool_size(1)
  |> pog.connect
}

fn create_tables(connection: pog.Connection) -> Nil {
  case
    "
    create type level as enum (
        'emergency',
        'alert',
        'critical',
        'error','warning',
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
    Error(e) -> panic as string.inspect(e)
  }
  let assert Ok(_) =
    "
    create table if not exists errors (
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
        error bigint references errors(id) on delete cascade,
        timestamp bigint not null,
        full_contents json
    )"
    |> pog.query
    |> pog.execute(connection)
  Nil
}

fn delete_table_contents(connection: pog.Connection) -> Nil {
  let assert Ok(_) =
    "delete from occurrences"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "delete from errors"
    |> pog.query
    |> pog.execute(connection)

  Nil
}

fn reset_db(connection: pog.Connection) -> Nil {
  let assert Ok(_) =
    "delete from occurrences"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "delete from errors"
    |> pog.query
    |> pog.execute(connection)

  Nil
}

fn transactional(connection: pog.Connection, callback: fn() -> a) -> Nil {
  let assert Ok(_) =
    "begin"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "set transaction read write"
    |> pog.query
    |> pog.execute(connection)

  callback()

  let assert Ok(_) =
    "rollback"
    |> pog.query
    |> pog.execute(connection)

  Nil
}
