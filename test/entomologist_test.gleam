import gleeunit
import gleeunit/should

// Idk how to test anything I have written for now. Tests will come eventually. I hope.

pub fn main() {
  gleeunit.main()
}

// Should succeed until the end of time
pub fn green_test() {
  1
  |> should.equal(1)
}
