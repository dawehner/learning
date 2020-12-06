import server
import gleam/should

pub fn hello_world_test() {
  server.hello_world()
  |> should.equal("Hello, from server!")
}
