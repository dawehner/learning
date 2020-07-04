import toplist
import gleam/should

pub fn hello_world_test() {
  toplist.hello_world()
  |> should.equal("Hello, from toplist!")
}
