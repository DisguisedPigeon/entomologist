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

// When running the tests the DB should be setted up

pub fn main() {
  let connection = get_connection()

  create_tables(connection)
  delete_table_contents(connection)
  let assert Ok(Nil) = entomologist.configure(connection)

  logging.configure()
  logging.set_level(logging.Debug)
  gleeunit.main()
}

@external(erlang, "dynamic_ffi", "id")
fn to_dynamic(val: a) -> decode.Dynamic

fn get_connection() {
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
  |> pog.connect
}

fn create_tables(connection) {
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
    );"
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
    );"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "
    create table if not exists occurrences (
        id bigserial not null unique primary key,
        error bigint references errors(id) on delete cascade,
        timestamp bigint not null,
        full_contents json
    );"
    |> pog.query
    |> pog.execute(connection)
  Nil
}

fn delete_table_contents(connection) {
  let assert Ok(_) =
    "delete from occurrences;"
    |> pog.query
    |> pog.execute(connection)

  let assert Ok(_) =
    "delete from errors;"
    |> pog.query
    |> pog.execute(connection)

  Nil
}

type AuxAtoms {
  Meta
  Msg
  Level
  Rest
  Time
}

pub fn uwu_test() {
  let connection = get_connection()
  logging.log(logging.Info, "hallo")

  let result =
    "select count(*) from occurrences"
    |> pog.query
    |> pog.returning(decode.list(decode.int))
    |> pog.execute(connection)

  should.equal(result, Ok(pog.Returned(1, [[1]])))

  let result =
    "select count(*) from errors"
    |> pog.query
    |> pog.returning(decode.list(decode.int))
    |> pog.execute(connection)

  should.equal(result, Ok(pog.Returned(1, [[1]])))

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
        #(to_dynamic(Msg), to_dynamic("hallo")),
        #(to_dynamic(Level), to_dynamic(charlist.from_string("info"))),
        #(to_dynamic(Rest), to_dynamic("{}")),
      ]),
    ),
    connection,
  )

  let result =
    "select count(*) from occurrences"
    |> pog.query
    |> pog.returning(decode.list(decode.int))
    |> pog.execute(connection)

  should.equal(result, Ok(pog.Returned(1, [[2]])))

  let result =
    "select count(*) from errors"
    |> pog.query
    |> pog.returning(decode.list(decode.int))
    |> pog.execute(connection)

  should.equal(result, Ok(pog.Returned(1, [[1]])))
}
