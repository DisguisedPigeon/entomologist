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
}

// FIXME: is it true that the level is only useful on DB side?
/// Adds a log to DB and passes it to the erlang logger
///
/// Due to erlang's logger implementation, the level is only useful to the DB side (???)
/// It returns `Ok(Nil)` if it succeeds or `Error(Nil)` if it fails
pub fn log(
  level: Level,
  message: String,
  db: pog.Connection,
) -> Result(Nil, Nil) {
  let _ = #(level, message, db)
  todo as "implement"
}

/// Adds a log to DB and passes it to the erlang logger. Crashes immediatly after.
pub fn log_and_crash(message: String, db: pog.Connection) -> Nil {
  let _ = #(message, db)
  todo as "implement"
}
