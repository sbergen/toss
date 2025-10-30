import checkmark
import envoy
import gleam/erlang/process
import gleam/function
import gleam/int
import gleeunit
import simplifile
import toss.{type Socket, type SocketOptions, Ipv4Address, Ipv6Address}
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

pub fn ip_to_string_test() {
  assert toss.ip_to_string(Ipv4Address(1, 2, 3, 4)) == "1.2.3.4"
  assert toss.ip_to_string(Ipv6Address(
      0x01,
      0x23,
      0x45,
      0x67,
      0x89,
      0xab,
      0xcd,
      0xef,
    ))
    == "1:23:45:67:89:ab:cd:ef"
}

pub fn parse_ip() {
  assert toss.prase_ip("1.2.3.4") == Ok(Ipv4Address(1, 2, 3, 4))
  assert toss.prase_ip("1:23:45:67:89:ab:cd:ef")
    == Ok(Ipv6Address(0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef))
  assert toss.prase_ip("not-an-ip") == Error(Nil)
}

pub fn ipv6_hostname_test() {
  let #(rcv_socket, port) = open(toss.use_ipv6)
  let assert Ok(send_socket) =
    toss.new(0)
    |> toss.use_ipv6()
    |> toss.open

  assert toss.send_to_host(send_socket, "localhost", port, <<"data">>)
    == Ok(Nil)

  let assert Ok(#(address, _port, <<"data">>)) =
    toss.receive(rcv_socket, 100, 10)
  let assert Ok(Ipv6Address(..)) = address

  toss.close(rcv_socket)
  toss.close(send_socket)
}

pub fn ipv4_hostname_test() {
  let #(rcv_socket, port) = open(toss.use_ipv4)
  let assert Ok(send_socket) =
    toss.new(0)
    |> toss.use_ipv4()
    |> toss.open

  assert toss.send_to_host(send_socket, "localhost", port, <<"data">>)
    == Ok(Nil)

  let assert Ok(#(address, _port, <<"data">>)) =
    toss.receive(rcv_socket, 100, 10)
  let assert Ok(Ipv4Address(..)) = address

  toss.close(rcv_socket)
  toss.close(send_socket)
}

pub fn receive_timeout_test() {
  let #(socket, port) = open(function.identity)
  let assert Ok(_) = toss.connect_to_host(socket, "localhost", port)

  let assert Error(toss.Timeout) = toss.receive(socket, 100, 1)

  toss.close(socket)
}

pub fn receive_forever_test() {
  let #(rcv_socket, port) = open(function.identity)
  let assert Ok(send_socket) =
    toss.new(0)
    |> toss.open
  let assert Ok(sender) = toss.connect_to_host(send_socket, "localhost", port)

  assert toss.send(sender, <<1>>) == Ok(Nil)
  let assert Ok(#(address, port, data)) = toss.receive_forever(rcv_socket, 100)

  let assert Ok(Ipv4Address(_, _, _, _)) = address
  assert Ok(port) == toss.local_port(send_socket)
  assert data == <<1>>

  toss.close(rcv_socket)
  toss.close(send_socket)
}

// Tests my assumptions and also error handling
pub fn ipv4_over_ipv6_test() {
  let assert Ok(socket) =
    toss.new(0)
    |> toss.use_ipv6()
    |> toss.open

  let assert Error(toss.Eafnosupport) =
    toss.connect_to(socket, Ipv4Address(0, 0, 0, 1), 42)

  let assert Error(toss.Eafnosupport) =
    toss.send_to(socket, Ipv4Address(0, 0, 0, 1), 42, <<>>)

  toss.close(socket)
}

pub fn recieve_ipv4_over_ipv6_test() {
  let #(rcv_socket, port) = open(toss.use_ipv6)
  let assert Ok(send_socket) = toss.new(0) |> toss.open
  let assert Ok(sender) =
    toss.connect_to(send_socket, Ipv4Address(127, 0, 0, 1), port)

  assert toss.send(sender, <<>>) == Ok(Nil)

  let assert Ok(#(Ok(address), _, _)) = toss.receive(rcv_socket, 10, 10)
  let assert Ipv6Address(0, 0, 0, 0, 0, 0xFFFF, high, low) = address
  assert high == int.bitwise_shift_left(127, 8)
  assert low == 1
}

pub fn message_test() {
  let #(rcv_socket, port) = open(function.identity)
  let assert Ok(send_socket) = toss.new(0) |> toss.open
  let assert Ok(sender) = toss.connect_to_host(send_socket, "localhost", port)
  let assert Ok(send_port) = toss.local_port(send_socket)

  let selector =
    toss.select_udp_messages(process.new_selector(), function.identity)
  toss.receive_next_datagram_as_message(rcv_socket)

  assert toss.send(sender, <<1>>) == Ok(Nil)
  assert toss.send(sender, <<2>>) == Ok(Nil)

  // Receive first datagram
  let assert Ok(msg) = process.selector_receive(selector, 1)
  let assert toss.Datagram(socket, address, port, data) = msg
  assert socket == rcv_socket
  let assert Ok(Ipv4Address(_, _, _, _)) = address
  assert port == send_port
  assert data == <<1>>

  // The next one shouldn't be received until calling receive_next...
  let assert Error(Nil) = process.selector_receive(selector, 1)

  // Receive the second datagram
  toss.receive_next_datagram_as_message(rcv_socket)
  let assert Ok(msg) = process.selector_receive(selector, 1)
  let assert toss.Datagram(_, _, _, <<2>>) = msg

  toss.close(rcv_socket)
  toss.close(send_socket)
}

fn open(set_opts: fn(SocketOptions) -> SocketOptions) -> #(Socket, Int) {
  let assert Ok(socket) = toss.new(0) |> set_opts |> toss.open()
  let assert Ok(port) = toss.local_port(socket)
  #(socket, port)
}
