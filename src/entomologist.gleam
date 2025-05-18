import entomologist/sql
import pog

pub type Level {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
  All
  None
}

pub type Metadata {
  Metadata(
    timestamp: Int,
    module: String,
    function: String,
    arity: String,
    file: String,
    line: Int,
  )
}

@external(erlang, "entomologist_logger_ffi", "configure")
pub fn configure(connection: pog.Connection) -> Result(Nil, Nil)

pub type Log {
  Log(level: Level, message: String, metadata: Metadata)
}

pub fn save_to_db(log: Log, connection: pog.Connection) -> Result(Nil, Nil) {
  let Log(
    level:,
    message:,
    metadata: Metadata(timestamp:, module:, function:, arity:, file:, line:),
  ) = log

  Ok(Nil)
}
