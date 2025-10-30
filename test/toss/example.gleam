import gleam/erlang/process
import gleam/result
import toss

const port = 56_789

pub fn main() {
  process.spawn(fn() {
    let assert Ok(Nil) = echo_once_server()
  })

  // Open a IPv4 socket on an OS-assigned (ephemeral) port.
  let assert Ok(socket) =
    toss.new(0)
    |> toss.use_ipv4()
    |> toss.open()

  // Only receive data from the specific host
  let assert Ok(sender) = toss.connect_to_host(socket, "localhost", port: port)

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
