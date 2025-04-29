import argv
import gleam/dict
import gleam/int
import gleam/io
import gleam/option
import gleam/result
import gleam/string
import pog

type Options {
  Help
  Host
  Port
  Database
  User
  Password
  Ssl
  ConnectionParameters
  PoolSize
  QueueTarget
  QueueInterval
  IdleInterval
  Trace
  IpVersion
  RowsAsMap
  DefaultTimeout
}

fn parse_config(parameters: dict.Dict(Options, String)) {
  pog.default_config()
  |> pog.pool_size(10)
  |> dict.fold(parameters, _, fn(acc, k, v) {
    case k {
      Host -> pog.host(acc, v)
      Port -> {
        let assert Ok(v) = int.parse(v) as "Port should be an integer"
        pog.port(acc, v)
      }
      Database -> pog.database(acc, v)
      User -> pog.user(acc, v)
      Password -> pog.password(acc, option.Some(v))
      Ssl -> {
        let v = case v {
          "verified" -> pog.SslVerified
          "unverified" -> pog.SslUnverified
          "disabled" -> pog.SslDisabled
          _ ->
            panic as "invalid SSL value. Valid values are:
          - \"verified\"
          - \"unverified\"
          - \"disabled\""
        }
        pog.ssl(acc, v)
      }
      ConnectionParameters -> {
        let assert [name, value] = string.split(v, on: ",")
          as "Invalid ConnectionParameters value. It should be connection_parameters=name,value"
        pog.connection_parameter(acc, name:, value:)
      }

      PoolSize -> {
        let assert Ok(v) = int.parse(v) as "pool_size should be an integer"
        pog.pool_size(acc, v)
      }
      QueueTarget -> {
        let assert Ok(v) = int.parse(v) as "queue_target should be an integer"
        pog.queue_target(acc, v)
      }
      QueueInterval -> {
        let assert Ok(v) = int.parse(v) as "queue_interval should be an integer"
        pog.queue_interval(acc, v)
      }
      IdleInterval -> {
        let assert Ok(v) = int.parse(v) as "idle_interval should be an integer"
        pog.idle_interval(acc, v)
      }
      Trace -> {
        let v = case v {
          "true" -> True
          "false" -> False
          _ -> panic as "trace should be either true or false"
        }
        pog.trace(acc, v)
      }
      IpVersion -> {
        let v = case int.parse(v) {
          Ok(4) -> pog.Ipv4
          Ok(6) -> pog.Ipv6
          _ -> panic as "idle_interval should be 4 or 6"
        }
        pog.ip_version(acc, v)
      }
      RowsAsMap -> {
        let v = case v {
          "true" -> True
          "false" -> False
          _ -> panic as "trace should be either true or false"
        }
        pog.rows_as_map(acc, v)
      }
      DefaultTimeout -> {
        let assert Ok(v) = int.parse(v)
          as "default_timeout should be an integer"
        pog.default_timeout(acc, v)
      }
      _ -> panic as "no more values are allowed"
    }
  })
}

pub fn main() {
  let parameters =
    argv.load().arguments
    |> get_args(dict.new(), _)

  let assert Ok(v) = case dict.has_key(parameters, Help) {
    True -> {
      io.println(
        "
      Usage:
        gleam run -m \"entomologist_setup\" [OPTIONS]

      Avaliable options:
        help - prints this help
        host
        port
        database
        user
        password
        ssl
        connection_parameters
        pool_size
        queue_target
        queue_interval
        idle_interval
        trace
        ip_version
        rows_as_map
        default_timeout

      For more information on each option, visit https://hexdocs.pm/pog/pog.html#Config
      ",
      )
      Ok(Nil)
    }
    False -> {
      let connection = parse_config(parameters) |> pog.connect
      case setup_db(connection) {
        Ok(_) -> {
          io.println("Succesfully setted up the DB.")
          Ok(Nil)
        }
        Error(_) -> {
          io.println("Something went wrong.")
          Ok(Nil)
        }
      }
    }
  }
}

const levels_definition = "
create type level as enum (
    'Emergency',
    'Alert',
    'Critical',
    'Error',
    'Warning'
    'Notice',
    'Info',
    'Debug'
);"

const table_creation = "
create table if not exists LOGS (
    id serial not null unique primary key,
    level level not null,
    code smallint,
    title text not null,
    description text
);"

fn setup_db(
  connection: pog.Connection,
) -> Result(pog.Returned(Nil), pog.QueryError) {
  let levels_definition =
    levels_definition
    |> pog.query
    |> pog.execute(connection)

  use _ <- result.try(levels_definition)
  let table_creation =
    table_creation
    |> pog.query
    |> pog.execute(connection)

  use _ <- result.try(table_creation)

  Ok(pog.Returned(0, []))
}

fn get_args(acc, l) {
  case l {
    ["host=" <> host, ..l] -> dict.insert(acc, Host, host) |> get_args(l)
    ["port=" <> port, ..l] -> dict.insert(acc, Port, port) |> get_args(l)
    ["database=" <> database, ..l] ->
      dict.insert(acc, Database, database) |> get_args(l)
    ["user=" <> user, ..l] -> dict.insert(acc, User, user) |> get_args(l)
    ["password=" <> password, ..l] ->
      dict.insert(acc, Password, password) |> get_args(l)
    ["ssl=" <> ssl, ..l] -> dict.insert(acc, Ssl, ssl) |> get_args(l)
    ["connection_parameters=" <> connection_parameters, ..l] ->
      dict.insert(acc, ConnectionParameters, connection_parameters)
      |> get_args(l)
    ["pool_size=" <> pool_size, ..l] ->
      dict.insert(acc, PoolSize, pool_size) |> get_args(l)
    ["queue_target=" <> queue_target, ..l] ->
      dict.insert(acc, QueueTarget, queue_target) |> get_args(l)
    ["queue_interval=" <> queue_interval, ..l] ->
      dict.insert(acc, QueueInterval, queue_interval) |> get_args(l)
    ["idle_interval=" <> idle_interval, ..l] ->
      dict.insert(acc, IdleInterval, idle_interval) |> get_args(l)
    ["trace=" <> trace, ..l] -> dict.insert(acc, Trace, trace) |> get_args(l)
    ["ip_version=" <> ip_version, ..l] ->
      dict.insert(acc, IpVersion, ip_version) |> get_args(l)
    ["rows_as_map=" <> rows_as_map, ..l] ->
      dict.insert(acc, RowsAsMap, rows_as_map) |> get_args(l)
    ["default_timeout=" <> default_timeout, ..l] ->
      dict.insert(acc, DefaultTimeout, default_timeout) |> get_args(l)
    [_, _, ..l] -> get_args(acc, l)
    ["help", ..] -> dict.from_list([#(Help, "")])
    [_] -> acc
    [] -> acc
  }
}
