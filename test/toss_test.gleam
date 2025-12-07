import checkmark
import envoy
import gleam/erlang/process
import gleam/function
import gleam/result
import gleeunit
import glip.{Ipv4, Ipv6}
import simplifile
import toss.{type Socket, type SocketOptions}
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
    |> toss.open

  assert toss.send_to_host(send_socket, "localhost", port, <<"data">>)
    == Ok(Nil)

  let assert Ok(#(Ok(address), _port, <<"data">>)) =
    toss.receive(rcv_socket, 100, 10)
  assert glip.address_family(address) == Ipv6

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

  let assert Ok(#(Ok(address), _port, <<"data">>)) =
    toss.receive(rcv_socket, 100, 10)
  assert glip.address_family(address) == Ipv4

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
  let assert Ok(#(Ok(address), port, data)) =
    toss.receive_forever(rcv_socket, 100)

  assert glip.address_family(address) == Ipv4
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

  let ip = coerce_ip("0.0.0.1")

  let assert Error(toss.Eafnosupport) = toss.connect_to(socket, ip, 42)
  let assert Error(toss.Eafnosupport) = toss.send_to(socket, ip, 42, <<>>)

  toss.close(socket)
}

pub fn recieve_ipv4_over_ipv6_test() {
  let ip = coerce_ip("127.0.0.1")
  let #(rcv_socket, port) = open(toss.use_ipv6)
  let assert Ok(send_socket) = toss.new(0) |> toss.open
  let assert Ok(sender) = toss.connect_to(send_socket, ip, port)

  assert toss.send(sender, <<>>) == Ok(Nil)

  let assert Ok(#(Ok(address), _, _)) = toss.receive(rcv_socket, 10, 10)
  assert glip.ip_to_string(address) == "::ffff:127.0.0.1"
}

pub fn multicast_test() {
  let mcast_addr = coerce_ip("224.0.0.42")
  let local_addr = coerce_ip("0.0.0.0")

  let apply_options = fn(opts) {
    toss.reuse_address(opts)
    |> toss.using_interface(mcast_addr)
  }

  let #(rcv_socket, port) = open(apply_options)
  assert toss.join_multicast_group(rcv_socket, mcast_addr, local_addr)
    == Ok(Nil)

  let assert Ok(send_socket) =
    toss.new(port)
    |> apply_options
    |> toss.open

  assert toss.send_to(send_socket, mcast_addr, port, <<"hi!">>) == Ok(Nil)
  let assert Ok(#(_, _, <<"hi!">>)) = toss.receive(rcv_socket, 8, 10)
    as "receiving multicast socket"
  let assert Ok(#(_, _, <<"hi!">>)) = toss.receive(send_socket, 8, 10)
    as "loopback multicast socket"

  assert toss.leave_multicast_group(rcv_socket, mcast_addr, local_addr)
    == Ok(Nil)

  assert toss.send_to(send_socket, mcast_addr, port, <<"hi!">>) == Ok(Nil)
  let assert Error(toss.Timeout) = toss.receive(rcv_socket, 8, 10)
    as "we've left the multicast group"

  // I don't know exactly why this the rejoin is needed:
  // I couldn't get this case to fail otherwise.
  assert toss.join_multicast_group(rcv_socket, mcast_addr, local_addr)
    == Ok(Nil)
  assert toss.loop_mutlicast(send_socket, False) == Ok(Nil)
  assert toss.send_to(send_socket, mcast_addr, port, <<"hi!">>) == Ok(Nil)
  let assert Error(toss.Timeout) = toss.receive(send_socket, 8, 10)
    as "loopback multicast disabled"

  toss.close(rcv_socket)
  toss.close(send_socket)
}

pub fn message_test() {
  let #(rcv_socket, port) = open(function.identity)
  let assert Ok(send_socket) = toss.new(0) |> toss.open
  let assert Ok(sender) = toss.connect_to_host(send_socket, "localhost", port)
  let assert Ok(send_port) = toss.local_port(send_socket)

  let selector =
    toss.select_udp_messages(process.new_selector(), function.identity)
  assert toss.receive_next_datagram_as_message(rcv_socket) == Ok(Nil)

  assert toss.send(sender, <<1>>) == Ok(Nil)
  assert toss.send(sender, <<2>>) == Ok(Nil)

  // Receive first datagram
  let assert Ok(msg) = process.selector_receive(selector, 1)
  let assert toss.Datagram(socket, Ok(address), port, data) = msg
  assert socket == rcv_socket
  assert glip.address_family(address) == Ipv4
  assert port == send_port
  assert data == <<1>>

  // The next one shouldn't be received until calling receive_next...
  let assert Error(Nil) = process.selector_receive(selector, 1)

  // Receive the second datagram
  assert toss.receive_next_datagram_as_message(rcv_socket) == Ok(Nil)
  let assert Ok(msg) = process.selector_receive(selector, 1)
  let assert toss.Datagram(_, _, _, <<2>>) = msg

  toss.close(rcv_socket)
  toss.close(send_socket)
}

pub fn invalid_port_test() {
  assert toss.new(123_456)
    |> toss.open()
    == Error(toss.BadArgument)
}

fn open(set_opts: fn(SocketOptions) -> SocketOptions) -> #(Socket, Int) {
  let assert Ok(socket) = toss.new(0) |> set_opts |> toss.open()
  let assert Ok(port) = toss.local_port(socket)
  #(socket, port)
}

fn coerce_ip(address: String) {
  glip.parse_ip(address) |> result.lazy_unwrap(fn() { panic as "Not an IP!" })
}
