# toss

[![Package Version](https://img.shields.io/hexpm/v/toss)](https://hex.pm/packages/toss)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/toss/)

A UDP socket library for Gleam on Erlang
(wraps [`gen_udp`](https://www.erlang.org/doc/apps/kernel/gen_udp.html)).

```sh
gleam add toss@1
```
```gleam
import gleam/erlang/process
import gleam/result
import toss

const port = 56_789

pub fn main() {
  process.spawn(fn() {
    assert echo_once_server() == Ok(Nil)
  })

  // Open a IPv4 socket on an OS-assigned (ephemeral) port.
  let assert Ok(socket) =
    toss.new(0)
    |> toss.use_ipv4()
    |> toss.open()

  // Only receive data from the specific host
  let assert Ok(sender) =
    toss.connect(socket, toss.Hostname("localhost"), port: port)

  // Send a datagram
  let assert Ok(_) = toss.send(sender, <<"Hello, Joe!">>)

  // ...and wait for a response
  let assert Ok(#(_, _, response)) = toss.receive(socket, 1024, 100)
  echo response as "Received echo"

  toss.close(socket)
}

/// Starts a UDP "server" that echoes back one datagram
fn echo_once_server() -> Result(Nil, toss.Error) {
  use socket <- result.try(toss.open(toss.new(port)))
  use #(address, port, data) <- result.try(toss.receive(
    socket,
    max_length: 1024,
    timeout_milliseconds: 10_000,
  ))

  // Assume we got a valid address
  let assert Ok(address) = address

  use _ <- result.try(toss.send_to(socket, address, port, data))
  Ok(toss.close(socket))
}
```

Further documentation can be found at <https://hexdocs.pm/toss>.

## Development

```sh
gleam test  # Run the tests
```
