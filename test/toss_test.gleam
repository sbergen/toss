import checkmark
import envoy
import gleeunit
import simplifile
import toss/example

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn rum_example_test() {
  example.main()
}

pub fn check_example_test() {
  assert checkmark.new(simplifile.read, simplifile.write)
    |> checkmark.file("README.md")
    |> checkmark.should_contain_contents_of(
      "./test/toss/example.gleam",
      tagged: "gleam",
    )
    // Update locally, check on CI
    |> checkmark.check_or_update(
      when: envoy.get("GITHUB_WORKFLOW") == Error(Nil),
    )
    == Ok(Nil)
}
