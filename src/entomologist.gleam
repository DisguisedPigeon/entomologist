import entomologist/internal/sql
import entomologist/internal/sql_custom
import gleam/dynamic/decode
import gleam/list
import gleam/string_tree

import gleam/int
import gleam/json
import gleam/option.{type Option}
import gleam/result
import gleam/string
import pog

/// The logged level of an [error](#Error).
pub type Level {
  /// Action must be taken immediately.
  Alert
  /// Critical for fuctionality, but does not require immediate action.
  Critical
  /// Useful information for debugging.
  Debug
  /// System is unusable. Fix NOW.
  Emergency
  /// Severe issues, but core functionality is stable.
  ErrorLevel
  /// Informational messages.
  Info
  /// Normal but noticeable conditions.
  Notice
  /// Important issues. Does not alter most functionality.
  Warning
}

/// A kind of error.
///
/// Error [occurrences](#Occurrence) are grouped into errors, that share location and message.
/// This same structure is replicated on the database.
pub type ErrorLog {
  ErrorLog(
    /// The unique identifier of an error kind.
    id: Int,
    /// The message logged.
    message: String,
    /// The [error level](#Level) associated with the error.
    level: Level,
    /// The module that raised the error.
    module: String,
    /// The function that raised the error.
    function: String,
    /// The arity of the function.
    arity: Int,
    /// The file from which the error was raised.
    file: String,
    /// The line where the error occurred.
    line: Int,
    /// Whether the error was tagged as resolved.
    resolved: Bool,
    /// Timestamp of the last occurrence associated with this error
    last_occurrence: Int,
    /// Whether the error is muted.
    ///
    /// A muted error is hidden from the user, but it can be brought back and, as soon as it gets risen again, the error appears highlighted.
    ///
    /// See also:
    /// - [mute](#mute)
    /// - [unmute](#unmute)
    muted: Bool,
    /// Taglist for the log.
    tags: List(String),
  )
}

fn default_log() {
  ErrorLog(
    id: -1,
    message: "",
    level: Info,
    last_occurrence: -1,
    resolved: False,
    muted: False,
    module: "",
    function: "",
    arity: -1,
    file: "",
    line: -1,
    tags: [],
  )
}

/// A singular raise instance of an [error](#ErrorLog).
///
/// This type holds all non-general information relative to its error, like the timestamp or the specific data it returned.
pub type Occurrence {
  Occurrence(
    /// Unique identifier of this occurrence.
    ///
    /// This is shared between all error types, there is only one error for a given occurrence identifier.
    id: Int,
    /// Unique identifier of the associated error type.
    log_id: Int,
    /// OS-relative timestamp of the occurrence in microseconds.
    ///
    /// > Since this is provided by the OS, It may be warped forwards or backwards by the user, so it may not be consistent.
    timestamp: Int,
    /// The body of the occurrence.
    ///
    /// Usually in json format.
    full_contents: Option(String),
  )
}

fn default_occurrence() {
  Occurrence(id: -1, log_id: -1, timestamp: -1, full_contents: option.None)
}

/// Configuring function for the erlang logger.
///
/// It registers the database as a logging handler and saves the DB connection
/// for later usage.
/// ## Example
///
/// ```gleam
/// let connection = pog.connect(config)
/// logging.configure(connection)
/// ```
@external(erlang, "entomologist_logger_ffi", "configure")
pub fn configure(connection: pog.Connection) -> Result(Nil, Nil)

/// Filtered fields datatype.
pub type SearchData {
  SearchData(
    /// The error message to search for.
    message: Option(String),
    /// The level to search for.
    level: Option(Level),
    /// The module where the error happens.
    module: Option(String),
    /// The function where the error happens.
    function: Option(String),
    /// The function's arity.
    arity: Option(Int),
    /// The file where the error happens.
    file: Option(String),
    /// The line where the error happens.
    line: Option(Int),
    /// Whether the searched error is resolved.
    resolved: Option(Bool),
    /// Last happenned timestamp.
    last_occurrence: Option(Int),
    /// Whether the searched error is maked as muted.
    muted: Option(Bool),
  )
}

/// Returns a [SearchData](#SearchData) object initialized to all None
pub fn default_search_data() -> SearchData {
  SearchData(
    message: option.None,
    level: option.None,
    module: option.None,
    function: option.None,
    arity: option.None,
    file: option.None,
    line: option.None,
    resolved: option.None,
    last_occurrence: option.None,
    muted: option.None,
  )
}

/// Error search function.
///
/// Sends a query to DB that returns a list of errors matching the fields given.
///
/// This takes a subset of [ErrorLog](#ErrorLog) fields encoded in [SearchData](#SearchData)
pub fn search(
  connection: pog.Connection,
  data: SearchData,
) -> Result(List(ErrorLog), String) {
  let location = "search"

  // For a string "a b c", it adds .* before and after each word, like this: ".*a.* .*b.* .*c.*", this makes the search less strict, giving an easier time with the search
  let fuzzify = fn(string: String) -> String {
    string
    |> string_tree.from_string()
    |> string_tree.split(on: " ")
    |> string_tree.join(with: "%")
    |> string_tree.prepend(prefix: "%")
    |> string_tree.append(suffix: "%")
    |> string_tree.lowercase
    |> string_tree.to_string
  }

  let query_result =
    sql_custom.search_log(
      connection,
      data.message |> option.map(fuzzify),
      data.level |> option.map(level_to_sql_level),
      data.module,
      data.function,
      data.arity,
      data.file,
      data.line,
      data.resolved,
      data.last_occurrence,
      data.muted,
    )

  case query_result {
    Ok(pog.Returned(_number, rows)) ->
      rows
      |> list.map(searchlogrow_to_errorlog)
      |> Ok

    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Mutes a log. It hides it until it happens again.
///
/// This is different than resolving because it is reversible and it highlights the error if it re-appears.
pub fn mute(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  connection
  |> sql.mute_log(log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "mute"))
}

/// Reverses [muting](#mute) for a given log.
pub fn unmute(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  connection
  |> sql.unmute(log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "unmute"))
}

/// Resolves a log.
///
/// The log is frozen on the DB for future reference, but there is no way to unresolve.
/// A new log should be created if the error re-appears.
///
/// If the function fails, it returns a string describing the error.
///
/// If really needed, SQL can be used to toggle resolution.
pub fn resolve(log_id: Int, connection: pog.Connection) -> Result(Nil, String) {
  connection
  |> sql.resolve_log(log_id)
  |> result.map(fn(_) { Nil })
  |> result.map_error(describe_error(_, "resolve"))
}

/// Retrieves all logs neither muted nor resolved.
pub fn show(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "show"

  case sql.show(connection) {
    Ok(pog.Returned(_number, rows)) ->
      rows
      |> list.map(show_row_to_log)
      |> Ok

    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Retrieves all muted logs.
pub fn muted(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "muted"

  case sql.muted(connection) {
    Ok(pog.Returned(_number, rows)) ->
      rows
      |> list.map(muted_row_to_log)
      |> Ok
    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Retrieves all logs marked as solved.
///
/// This hides both active and muted logs.
pub fn solved(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "solved"

  case sql.solved(connection) {
    Ok(pog.Returned(_number, rows)) ->
      rows
      |> list.map(solved_row_to_log)
      |> Ok
    Error(error) ->
      describe_error(error:, location:)
      |> Error
  }
}

/// Retrieves every log in the DB.
pub fn logs(connection: pog.Connection) -> Result(List(ErrorLog), String) {
  let location = "logs"

  case sql.logs(connection) {
    Ok(pog.Returned(_number, rows)) ->
      rows
      |> list.map(logs_row_to_log)
      |> Ok
    Error(error) -> describe_error(error:, location:) |> Error
  }
}

/// Retrieves a log's occurrences.
pub fn occurrences(
  log_id: Int,
  connection: pog.Connection,
) -> Result(List(Occurrence), String) {
  case sql.occurrences(connection, log_id) {
    Ok(pog.Returned(_number, rows)) ->
      list.map(rows, occurrences_row_to_occurrence)
      |> Ok

    Error(error) ->
      describe_error(error:, location: "show failed occurrences")
      |> Error
  }
}

/// Retrieves the data for a specific log.
pub fn log_data(
  log_id: Int,
  connection: pog.Connection,
) -> Result(ErrorLog, String) {
  let log = sql.log_data(connection, log_id)
  let tags = sql.log_tags(connection, log_id)

  case log, tags {
    Ok(pog.Returned(1, [log_as_row])), Ok(pog.Returned(_, rows: tags)) -> {
      let level = sql_level_to_level(log_as_row.level)
      let tags = list.map(tags, with: fn(v) { v.name })

      Ok(ErrorLog(
        id: log_as_row.id,
        message: log_as_row.message,
        level:,
        module: log_as_row.module,
        function: log_as_row.function,
        arity: log_as_row.arity,
        file: log_as_row.file,
        line: log_as_row.line,
        resolved: log_as_row.resolved,
        last_occurrence: log_as_row.last_occurrence,
        muted: log_as_row.muted,
        tags:,
      ))
    }

    Ok(v), Ok(_) -> panic as { "unexpected log amount: " <> string.inspect(v) }

    Error(error), _ ->
      describe_error(error:, location: "show failed occurrences")
      |> Error
    _, Error(error) ->
      describe_error(error:, location: "show failed occurrences")
      |> Error
  }
}

/// Add tag
///
/// Adds a tag to a log. Creates it if it doesn't exist.
pub fn add_tag(
  connection: pog.Connection,
  log_id: Int,
  tag: String,
) -> Result(Nil, String) {
  let location = "add_tag"

  let fuzzify = fn(s) { string.trim(s) |> string.lowercase }

  case sql.find_tag(connection, tag |> fuzzify) {
    Ok(pog.Returned(count: 0, rows: [])) -> {
      let tag_result =
        sql.create_tag(connection, tag)
        |> result.map_error(describe_error(_, location:))

      use query_result <- result.try(tag_result)

      let assert pog.Returned(1, [sql.CreateTagRow(id: tag_id)]) = query_result
        as "create_tag only creates one entry"

      sql.add_tag(connection, log_id, tag_id)
      |> result.map(fn(_) { Nil })
      |> result.map_error(describe_error(_, location:))
    }

    Ok(pog.Returned(count: 1, rows: [sql.FindTagRow(id:)])) ->
      sql.add_tag(connection, log_id, id)
      |> result.map(fn(_) { Nil })
      |> result.map_error(describe_error(_, location:))

    Error(error) -> describe_error(error:, location:) |> Error

    _ -> panic as "Find tag should only return 1 or no item"
  }
}

/// Utility function to turn a pog QueryError error into a readable log and add the location of the failure.
fn describe_error(
  error error: pog.QueryError,
  location origin: String,
) -> String {
  "Query: "
  <> origin
  <> " - "
  <> case error {
    pog.ConstraintViolated(message:, constraint:, detail:) ->
      "Constraint violated: "
      <> constraint
      <> " - "
      <> message
      <> " - "
      <> detail
    pog.PostgresqlError(code:, name:, message:) ->
      "Postgres error: (" <> code <> ")" <> name <> " - " <> message
    pog.UnexpectedArgumentCount(expected:, got:) ->
      "THIS SHOULD NEVER HAPPEN - Argument quantity mismatch, expected "
      <> int.to_string(expected)
      <> ", got "
      <> int.to_string(got)
    pog.UnexpectedArgumentType(expected:, got:) ->
      "THIS SHOULD NEVER HAPPEN - Argument type mismatch, expected "
      <> expected
      <> ", got "
      <> got
    pog.UnexpectedResultType(decode_errors) ->
      "THIS SHOULD NEVER HAPPEN - Unable to decode result type: "
      <> string.inspect(decode_errors)
    pog.QueryTimeout -> "Database query timed out"
    pog.ConnectionUnavailable -> "Connection to database unavaliable"
  }
}

// ------------------- JSON encoders and decoders ------------------- //

pub fn encode_error_log(error_log: ErrorLog) -> json.Json {
  let ErrorLog(
    id:,
    message:,
    level:,
    module:,
    function:,
    arity:,
    file:,
    line:,
    resolved:,
    last_occurrence:,
    muted:,
    tags:,
  ) = error_log

  json.object([
    #("id", json.int(id)),
    #("message", json.string(message)),
    #("level", encode_level(level)),
    #("module", json.string(module)),
    #("function", json.string(function)),
    #("arity", json.int(arity)),
    #("file", json.string(file)),
    #("line", json.int(line)),
    #("resolved", json.bool(resolved)),
    #("last_occurrence", json.int(last_occurrence)),
    #("muted", json.bool(muted)),
    #("tags", json.array(tags, of: json.string)),
  ])
}

pub fn level_decoder() -> decode.Decoder(Level) {
  use variant <- decode.then(decode.string)
  case variant {
    "alert" -> decode.success(Alert)
    "critical" -> decode.success(Critical)
    "debug" -> decode.success(Debug)
    "emergency" -> decode.success(Emergency)
    "error_level" -> decode.success(ErrorLevel)
    "info" -> decode.success(Info)
    "notice" -> decode.success(Notice)
    "warning" -> decode.success(Warning)
    _ -> decode.success(Warning)
  }
}

pub fn encode_level(level: Level) -> json.Json {
  case level {
    Alert -> json.string("alert")
    Critical -> json.string("critical")
    Debug -> json.string("debug")
    Emergency -> json.string("emergency")
    ErrorLevel -> json.string("error")
    Info -> json.string("info")
    Notice -> json.string("notice")
    Warning -> json.string("warning")
  }
}

// ------------------- sql autogenerated rows to ErrorLog ------------------- //

fn logs_row_to_log(logs_row: sql.LogsRow) -> ErrorLog {
  let level = logs_row.level |> sql_level_to_level

  ErrorLog(
    ..default_log(),
    id: logs_row.id,
    message: logs_row.message,
    level:,
    last_occurrence: logs_row.last_occurrence,
  )
}

fn show_row_to_log(show_row: sql.ShowRow) -> ErrorLog {
  let level = show_row.level |> sql_level_to_level

  ErrorLog(
    ..default_log(),
    id: show_row.id,
    message: show_row.message,
    level:,
    last_occurrence: show_row.last_occurrence,
  )
}

fn muted_row_to_log(muted_row: sql.MutedRow) -> ErrorLog {
  let level = muted_row.level |> sql_level_to_level

  ErrorLog(
    ..default_log(),
    id: muted_row.id,
    message: muted_row.message,
    level:,
    last_occurrence: muted_row.last_occurrence,
  )
}

fn solved_row_to_log(solved_row: sql.SolvedRow) -> ErrorLog {
  let level = solved_row.level |> sql_level_to_level

  ErrorLog(
    ..default_log(),
    id: solved_row.id,
    message: solved_row.message,
    level:,
    last_occurrence: solved_row.last_occurrence,
  )
}

fn searchlogrow_to_errorlog(search_log_row: sql.SearchLogRow) -> ErrorLog {
  let level = sql_level_to_level(search_log_row.level)

  ErrorLog(
    ..default_log(),
    id: search_log_row.id,
    message: search_log_row.message,
    level:,
    last_occurrence: search_log_row.last_occurrence,
  )
}

fn level_to_sql_level(level: Level) -> sql.Level {
  case level {
    Alert -> sql.Alert
    Critical -> sql.Critical
    Debug -> sql.Debug
    Emergency -> sql.Emergency
    ErrorLevel -> sql.Error
    Info -> sql.Info
    Notice -> sql.Notice
    Warning -> sql.Warning
  }
}

fn sql_level_to_level(level: sql.Level) -> Level {
  case level {
    sql.Debug -> Debug
    sql.Info -> Info
    sql.Notice -> Notice
    sql.Warning -> Warning
    sql.Error -> ErrorLevel
    sql.Critical -> Critical
    sql.Alert -> Alert
    sql.Emergency -> Emergency
  }
}

pub fn encode_occurrence(occurrence: Occurrence) -> json.Json {
  let Occurrence(id:, log_id:, timestamp:, full_contents:) = occurrence
  json.object([
    #("id", json.int(id)),
    #("log_id", json.int(log_id)),
    #("timestamp", json.int(timestamp)),
    #("full_contents", case full_contents {
      option.None -> json.null()
      option.Some(value) -> json.string(value)
    }),
  ])
}

pub fn occurrence_decoder() -> decode.Decoder(Occurrence) {
  use id <- decode.field("id", decode.int)
  use log_id <- decode.field("log_id", decode.int)
  use timestamp <- decode.field("timestamp", decode.int)
  use full_contents <- decode.field(
    "full_contents",
    decode.optional(decode.string),
  )
  decode.success(Occurrence(id:, log_id:, timestamp:, full_contents:))
}

fn occurrences_row_to_occurrence(
  occurrences_row: sql.OccurrencesRow,
) -> Occurrence {
  Occurrence(
    ..default_occurrence(),
    id: occurrences_row.id,
    timestamp: occurrences_row.timestamp,
    full_contents: occurrences_row.full_contents,
  )
}
