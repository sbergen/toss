# toss

[![Package Version](https://img.shields.io/hexpm/v/toss)](https://hex.pm/packages/toss)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/toss/)

A work in progress UDP library for Gleam on Erlang (wraps `gen_udp`).

```sh
gleam add toss@1
```
```gleam
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
```

Further documentation can be found at <https://hexdocs.pm/toss>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
