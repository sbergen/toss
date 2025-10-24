import checkmark
import envoy
import gleeunit
import simplifile
import toss.{
  type MultiPeerSocket, type SocketOptions, Hostname, Ipv4Address, Ipv6Address,
}
import toss/example

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn run_example_test() {
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

pub fn ipv6_hostname_test() {
  let #(rcv_socket, port) = open(toss.use_ipv6)
  let assert Ok(send_socket) =
    toss.new(0)
    |> toss.use_ipv6()
    |> toss.connect(Hostname("localhost"), port)
  assert toss.send(send_socket, <<"data">>) == Ok(Nil)

  let assert Ok(#(address, _port, <<"data">>)) =
    toss.receive_any(rcv_socket, 100, 10)
  let assert Ok(Ipv6Address(..)) = address

  toss.close(rcv_socket)
  toss.disconnect(send_socket)
}

pub fn ipv4_hostname_test() {
  let #(rcv_socket, port) = open(toss.use_ipv4)
  let assert Ok(send_socket) =
    toss.new(0)
    |> toss.use_ipv4()
    |> toss.connect(Hostname("localhost"), port)
  assert toss.send(send_socket, <<"data">>) == Ok(Nil)

  let assert Ok(#(address, _port, <<"data">>)) =
    toss.receive_any(rcv_socket, 100, 10)
  let assert Ok(Ipv4Address(..)) = address

  toss.close(rcv_socket)
  toss.disconnect(send_socket)
}

// Tests my assumptions and also error handling
pub fn ipv4_over_ipv6_test() {
  let #(rcv_socket, port) = open(toss.use_ipv6)
  let assert Error(toss.Eafnosupport) =
    toss.new(0)
    |> toss.use_ipv6()
    |> toss.connect(Ipv4Address(127, 0, 0, 1), port)

  toss.close(rcv_socket)
}

fn open(set_opts: fn(SocketOptions) -> SocketOptions) -> #(MultiPeerSocket, Int) {
  let assert Ok(socket) = toss.new(0) |> set_opts |> toss.open()
  let assert Ok(port) = toss.listening_port(socket)
  #(socket, port)
}
