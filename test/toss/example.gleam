import gleam/erlang/process
import gleam/result
import toss.{type MultiPeerSocket}

const port = 56_789

pub fn main() {
  process.spawn(echo_server)

  let assert Ok(socket) =
    toss.connect(
      local_port: 0,
      host: toss.Hostname("localhost"),
      remote_port: port,
    )

  let assert Ok(_) = toss.send(socket, <<"Hello, Joe!">>)
  echo toss.receive(socket, 1024, 100) as "Received echo"
}

fn echo_server() -> Result(Nil, toss.Error) {
  use socket <- result.try(toss.open(port))
  let assert Ok(_) = echo_on_socket(socket)
}

fn echo_on_socket(socket: MultiPeerSocket) -> Result(Nil, toss.Error) {
  use #(address, port, data) <- result.try(toss.receive_any(
    socket,
    max_length: 1024,
    timeout_milliseconds: 10_000,
  ))

  use _ <- result.try(toss.send_to(socket, address, port, data))

  // Repeat via recursion:
  echo_on_socket(socket)
}
