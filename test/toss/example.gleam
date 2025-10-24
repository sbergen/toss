import gleam/erlang/process
import gleam/result
import toss

const port = 56_789

pub fn main() {
  process.spawn(fn() {
    assert echo_once_server() == Ok(Nil)
  })

  let assert Ok(socket) =
    toss.new(port: 0)
    |> toss.connect(toss.Hostname("localhost"), port: port)

  let assert Ok(_) = toss.send(socket, <<"Hello, Joe!">>)
  let assert Ok(response) = toss.receive(socket, 1024, 100)
  echo response as "Received echo"

  toss.disconnect(socket)
}

fn echo_once_server() -> Result(Nil, toss.Error) {
  use socket <- result.try(toss.open(toss.new(port)))
  use #(address, port, data) <- result.try(toss.receive_any(
    socket,
    max_length: 1024,
    timeout_milliseconds: 10_000,
  ))

  // Assume we got a know address
  let assert Ok(address) = address

  use _ <- result.try(toss.send_to(socket, address, port, data))
  Ok(toss.close(socket))
}
