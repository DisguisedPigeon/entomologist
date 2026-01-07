//// When running the tests, a postgres database should be running.
////
//// This is the expected config for that postgres DB:
//// - Port "5431" or the POSTGRES_PORT env variable
//// - Running on "localhost"
//// - User "postgres"
//// - Password set to "postgres"
//// - Database named "test"

import birdie
import entomologist
import entomologist/internal/logger_api
import envoy
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/charlist
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/static_supervisor as supervisor
import gleam/result
import gleam/string
import logging
import pog
import qcheck_gleeunit_utils/run
import qcheck_gleeunit_utils/test_spec

pub fn main() {
  // Creates the tests' DB conection in a process pool.
  let pool_name = process.new_name("postgres_pool")
  let assert Ok(_) = create_pool(pool_name:)

  let connection = pog.named_connection(pool_name)

  // HACK : Creates a erlang GenServer storing the connection for later usage.
  //        Maybe this isn't the best solution...
  set_connection(pool_name)

  logging.configure()
  logging.set_level(logging.Debug)
  let assert Ok(Nil) = entomologist.configure(connection)
    as "configuration should end successfully"

  create_tables(pog.named_connection(pool_name))

  let assert Ok(_) =
    "set session characteristics as transaction isolation level serializable"
    |> pog.query
    |> pog.execute(connection)
    as "There should be no issue setting the isolation level for the session."

  run.run_gleeunit()
}

pub fn run_all_inorder_test_() {
  [
    // Low level
    // // Log creation
    string_message,
    report_message,

    // // Grouping
    string_message_grouping,
    report_message_grouping,

    // High-level features
    log_show,
    log_search,
    log_search_multiple,
    tag_creation,
    tag_removal,
    tag_deletion,
  ]
  |> list.map(test_spec.make)
  |> test_spec.run_in_order
}

fn string_message() {
  use connection <- transactional()
  let message = "string_message_test message"

  logging.log(logging.Info, message)

  use count, query <- count_query("logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- count_query("occurrences", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  Nil
}

fn string_message_grouping() {
  use connection <- transactional()
  let message = "string_message_grouping_test message"

  logging.log(logging.Info, message)

  use count, query <- count_query("logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- count_query("occurrences", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  logging.log(logging.Info, message)

  use count, query <- count_query("logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- count_query("occurrences", connection)
  assert 2 == count as { "while executing\"" <> query <> "\"" }

  Nil
}

fn report_message() {
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
  let timestamp = 10_000

  report(timestamp) |> logger_api.save_to_db(connection)

  use count, query <- count_query("logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- count_query("occurrences", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  Nil
}

fn report_message_grouping() {
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

  let timestamp = 10_000

  log(timestamp) |> logger_api.save_to_db(connection)

  use count, query <- count_query("logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- count_query("occurrences", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  log(1 + timestamp) |> logger_api.save_to_db(connection)

  use count, query <- count_query("logs", connection)
  assert 1 == count as { "while executing\"" <> query <> "\"" }

  use count, query <- count_query("occurrences", connection)
  assert 2 == count as { "while executing\"" <> query <> "\"" }

  Nil
}

fn log_show() {
  use connection <- transactional()
  let message = "show test"
  logging.log(logging.Info, message)

  let assert Ok([log]) = entomologist.show(connection)

  print_log(log:, prefix: "")
  |> birdie.snap("show")
}

fn log_search() {
  use connection <- transactional()
  let message = "search one"
  logging.log(logging.Info, message)

  let message = option.Some(message)
  let assert Ok([log]) =
    entomologist.search(
      connection,
      entomologist.SearchData(..entomologist.default_search_data(), message:),
    )

  print_log(log, "")
  |> birdie.snap("search log")
}

fn log_search_multiple() {
  use connection <- transactional()
  let message = "search multiple"
  let message2 = "search multiple 2"
  logging.log(logging.Info, message)
  logging.log(logging.Info, message2)

  let message = option.Some(message)
  let assert Ok(logs) =
    entomologist.search(
      connection,
      entomologist.SearchData(..entomologist.default_search_data(), message:),
    )

  print_multiple_logs(logs)
  |> birdie.snap("search two logs")

  Nil
}

fn tag_creation() {
  use connection <- transactional()
  let message = "tag creation"
  logging.log(logging.Info, message)

  let assert Ok([log]) = entomologist.show(connection)

  let assert Ok(Nil) = entomologist.add_tag(connection, log.id, "New Tag")

  let assert Ok(log) = entomologist.log_data(log.id, connection)

  let assert ["new tag"] = log.tags

  Nil
}

fn tag_removal() {
  use connection <- transactional()
  let message = "tag removal"
  logging.log(logging.Info, message)

  let message = "tag removal 2"
  logging.log(logging.Info, message)

  let assert Ok([log, log2]) = entomologist.show(connection)

  let assert Ok(Nil) = entomologist.add_tag(connection, log.id, "New Tag")
  let assert Ok(Nil) = entomologist.add_tag(connection, log2.id, "New tag")

  let assert Ok(log) = entomologist.log_data(log.id, connection)
  assert log.tags == ["new tag"]

  let assert Ok(Nil) = entomologist.remove_tag(connection, log.id, "new Tag")

  let assert Ok(log) = entomologist.log_data(log.id, connection)
  assert log.tags == []

  use count, _query <- count_query("tags", connection)
  assert count == 1

  let assert Ok(log) = entomologist.log_data(log2.id, connection)

  assert ["new tag"] == log.tags

  Nil
}

fn tag_deletion() {
  use connection <- transactional()
  let message = "tag deletion"
  logging.log(logging.Info, message)

  let assert Ok([log]) = entomologist.show(connection)
  let assert Ok(Nil) = entomologist.add_tag(connection, log.id, "New Tag")

  let assert Ok(log) = entomologist.log_data(log.id, connection)
  assert log.tags == ["new tag"]

  let assert Ok(Nil) = entomologist.remove_tag(connection, log.id, "new Tag")

  let assert Ok(log) = entomologist.log_data(log.id, connection)
  assert log.tags == []

  use count, _query <- count_query("tags", connection)

  assert count == 0

  Nil
}

// This function is structured this way to allow assert to return the correct
// location while providing a ergonomic way to run the same pog query across
// various tests.
/// Auxiliary function to get the number of elements in a table.
///
/// The query is expected to have the form "select count(*) from [SOME_TABLE]".
/// It gets passed to the callback to retain assertion failure location instead
/// of just checking in place and every error happening here.
///
/// ## Example:
/// ```gleam
/// use count, query <- execute_query("logs", connection)
/// assert count == 3 as { "Query: " <> query }
/// ```
///
fn count_query(
  table: String,
  connection: pog.Connection,
  callback: fn(Int, String) -> b,
) -> b {
  let query = "select count(id) from " <> table
  let assert Ok(pog.Returned(1, [[count]])) =
    pog.query(query)
    |> pog.timeout(100_000_000_000)
    |> pog.returning(decode.list(decode.int))
    |> pog.execute(connection)

  callback(count, query)
}

/// Auxiliary function to run a test as a transaction.
///
/// ## Example:
/// ```gleam
/// fn some_test() {
///   use connection <- transactional()
///   //...
///   Error("")
/// }
/// ```
fn transactional(callback: fn(pog.Connection) -> Nil) -> Nil {
  // Ensures a rollback happens
  let connection = get_connection() |> pog.named_connection

  let assert Ok(_) =
    "begin"
    |> pog.query
    |> pog.execute(connection)
    as "There should be no issue begginning the transaction."

  let _ = callback(connection)

  let assert Ok(_) =
    "rollback"
    |> pog.query
    |> pog.execute(connection)
    as "There should be no issue rolling back."

  Nil
}

/// DB setup
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
    Error(e) ->
      panic as {
        "unexpected error when creating type level: " <> string.inspect(e)
      }
  }

  let assert Ok(_) =
    "create table if not exists logs (
       id bigserial not null unique primary key,
       message text not null,
       level level not null,
       module text not null,
       function text not null,
       arity int not null,
       file text not null,
       line int not null,
       last_occurrence bigint not null,
       resolved bool not null default false,
       muted bool not null default false
     );"
    |> pog.query
    |> pog.execute(connection)
    as "table logs should be created without issues."

  let assert Ok(_) =
    "create table if not exists occurrences (
       id bigserial not null unique primary key,
       log bigint not null references logs(id) on delete cascade,
       timestamp bigint not null,
       full_contents json
     );"
    |> pog.query
    |> pog.execute(connection)
    as "table occurrences should be created without issues."

  let assert Ok(_) =
    "create table if not exists tags (
       id bigserial not null unique primary key,
       name text not null unique
     );"
    |> pog.query
    |> pog.execute(connection)
    as "tag table should be created successfully."

  let assert Ok(_) =
    "create table if not exists log2tag (
       log bigserial not null references logs(id) on delete cascade,
       tag bigserial not null references tags(id),
       constraint logtag_pkey primary key (log, tag)
     );"
    |> pog.query
    |> pog.execute(connection)
    as "log2tag table should be created successfully."

  let assert Ok(_) =
    "delete from log2tag"
    |> pog.query
    |> pog.execute(connection)
    as "table log2tag should be emptied. Even if its empty it won't fail."

  let assert Ok(_) =
    "delete from tags"
    |> pog.query
    |> pog.execute(connection)
    as "table tags should be emptied. Even if its empty it won't fail. This wont fail since all log2tag mappings are deleted."

  let assert Ok(_) =
    "delete from occurrences"
    |> pog.query
    |> pog.execute(connection)
    as "table occurrences should be emptied. Even if its empty it won't fail."

  let assert Ok(_) =
    "delete from logs"
    |> pog.query
    |> pog.execute(connection)
    as "table logs should be emptied. If its empty it won't fail. This wont fail since all occurrences are deleted and all log2tag mappings are too."

  Nil
}

fn print_multiple_logs(logs: List(entomologist.ErrorLog)) {
  list.index_map(logs, fn(log, i) {
    print_log(log:, prefix: "Log" <> int.to_string(i + 1)) <> "\n\n"
  })
  |> string.concat
}

fn print_log(log log: entomologist.ErrorLog, prefix prefix: String) {
  // We ignore the id and the last occurrence since they may change on different runs
  let entomologist.ErrorLog(
    id: _,
    message:,
    level:,
    module:,
    function:,
    arity:,
    file:,
    line:,
    resolved:,
    last_occurrence: _,
    muted:,
    tags:,
  ) = log

  let prefix = case prefix {
    "" -> "Log"
    _ -> prefix
  }
  prefix <> "(
  message: \"" <> message <> "\"
  level: " <> string.inspect(level) <> "
  module: \"" <> module <> "\"
  function: \"" <> function <> "\"
  arity: " <> string.inspect(arity) <> "
  file: \"" <> file <> "\"
  line: " <> string.inspect(line) <> "
  resolved: " <> string.inspect(resolved) <> "
  muted: " <> string.inspect(muted) <> "
  tags: " <> string.inspect(tags) <> "
)"
}

// TODO: Might have to rewrite to erlang FFI in the future to not depend on
//       gleam's implementation just in case.
//
/// Utility type.
/// This gets translated by the compiler to erlang atoms.
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

@external(erlang, "test_ffi", "id")
fn to_dynamic(val: t) -> decode.Dynamic

@external(erlang, "test_ffi", "get")
fn get_connection() -> process.Name(pog.Message)

@external(erlang, "test_ffi", "set")
fn set_connection(connection: process.Name(pog.Message)) -> Nil

// Creates a named pool to be transformed into a pog connection
fn create_pool(pool_name pool_name: process.Name(pog.Message)) {
  let port = case envoy.get("POSTGRES_PORT") {
    Ok(v) ->
      result.lazy_unwrap(int.parse(v), fn() {
        panic as { "failed parsing port from: " <> string.inspect(v) }
      })
    Error(_) -> 5431
  }

  let pool =
    pog.default_config(pool_name)
    |> pog.port(port)
    |> pog.host("localhost")
    |> pog.user("postgres")
    |> pog.password(option.Some("postgres"))
    |> pog.database("test")
    |> pog.pool_size(1)
    |> pog.supervised

  supervisor.new(supervisor.RestForOne)
  |> supervisor.add(pool)
  |> supervisor.start()
}
