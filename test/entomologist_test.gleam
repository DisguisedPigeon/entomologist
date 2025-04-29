import birdie
import entomologist/logging
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn full_log_to_string_test() {
  logging.default_log("I'm a teapot")
  |> logging.description("HTCPCP 418: You tried brewing coffee on a teapot.")
  |> logging.code(418)
  |> logging.error
  |> logging.to_string_full
  |> birdie.snap(title: "log with all optional fields")
}

pub fn basic_log_to_string_test() {
  logging.default_log("I'm a teapot")
  |> logging.emergency
  |> logging.to_string_full
  |> birdie.snap(title: "log with no optional fields")
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}
