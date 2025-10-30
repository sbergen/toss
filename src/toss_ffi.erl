-module(toss_ffi).

-export([active_once/0, passive/0, connect/3, recv/2, recv/3, send/2, send/4,
         map_udp_message/1, ip_to_string/1, parse_ip/1, from_gleam_address/1, setopts/2]).

% Enable inlining, as all the functions are very small.
-compile(inline).

ip_to_string(Address) ->
  String = inet:ntoa(from_gleam_address(Address)),
  unicode:characters_to_binary(String).

parse_ip(Address) ->
  AddressString = unicode:characters_to_list(Address),
  case inet:parse_address(AddressString) of
    {ok, Ip} ->
      to_gleam_address(Ip);
    _ ->
      {error, nil}
  end.

active_once() ->
  once.

passive() ->
  false.

setopts(Socket, Opts) ->
  case inet:setopts(Socket, Opts) of
    ok ->
      {ok, nil};
    Error ->
      Error
  end.

connect(Socket, Address, Port) ->
  case gen_udp:connect(Socket, from_gleam_address(Address), Port) of
    ok ->
      {ok, Socket};
    Error ->
      Error
  end.

recv(Socket, Length, Timeout) ->
  map_recv_result(gen_udp:recv(Socket, Length, Timeout)).

recv(Socket, Length) ->
  map_recv_result(gen_udp:recv(Socket, Length)).

map_recv_result(Result) ->
  case Result of
    {ok, {Address, Port, Data}} ->
      {ok, {to_gleam_address(Address), Port, Data}};
    Error ->
      Error
  end.

send(Socket, Data) ->
  map_send_result(gen_udp:send(Socket, Data)).

send(Socket, Address, Port, Data) ->
  map_send_result(gen_udp:send(Socket, from_gleam_address(Address), Port, Data)).

map_send_result(Result) ->
  case Result of
    ok ->
      {ok, nil};
    Error ->
      Error
  end.

to_gleam_address({O1, O2, O3, O4}) ->
  {ok, {ipv4_address, O1, O2, O3, O4}};
to_gleam_address({W1, W2, W3, W4, W5, W6, W7, W8}) ->
  {ok, {ipv6_address, W1, W2, W3, W4, W5, W6, W7, W8}};
to_gleam_address(_) ->
  {error, nil}.

from_gleam_address(Hostname) when is_binary(Hostname) ->
  unicode:characters_to_list(Hostname);
from_gleam_address({ipv4_address, O1, O2, O3, O4}) ->
  {O1, O2, O3, O4};
from_gleam_address({ipv6_address, W1, W2, W3, W4, W5, W6, W7, W8}) ->
  {W1, W2, W3, W4, W5, W6, W7, W8}.

map_udp_message({udp, Socket, Address, Port, Data}) ->
  {datagram, Socket, to_gleam_address(Address), Port, Data};
map_udp_message({udp_error, Socket, Error}) ->
  {udp_error, Socket, Error}.
