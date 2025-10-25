-module(toss_ffi).

-export([active_once/0, passive/0, connect/3, recv/2, recv/3, send/2, send/4,
         map_udp_message/1]).

% Enable inlining, as all the functions are very small.
-compile(inline).

active_once() ->
  once.

passive() ->
  false.

connect(Socket, Address, Port) ->
  case gen_udp:connect(Socket, unmap_address(Address), Port) of
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
      {ok, {map_address(Address), Port, Data}};
    Error ->
      Error
  end.

send(Socket, Data) ->
  map_send_result(gen_udp:send(Socket, Data)).

send(Socket, Address, Port, Data) ->
  map_send_result(gen_udp:send(Socket, unmap_address(Address), Port, Data)).

map_send_result(Result) ->
  case Result of
    ok ->
      {ok, nil};
    Error ->
      Error
  end.

map_address(Address) when is_list(Address) ->
  {ok, {hostname, unicode:characters_to_binary(Address)}};
map_address({O1, O2, O3, O4}) ->
  {ok, {ipv4_address, O1, O2, O3, O4}};
map_address({W1, W2, W3, W4, W5, W6, W7, W8}) ->
  {ok, {ipv6_address, W1, W2, W3, W4, W5, W6, W7, W8}};
map_address(_) ->
  {error, nil}.

unmap_address({hostname, Binary}) ->
  unicode:characters_to_list(Binary);
unmap_address({ipv4_address, O1, O2, O3, O4}) ->
  {O1, O2, O3, O4};
unmap_address({ipv6_address, W1, W2, W3, W4, W5, W6, W7, W8}) ->
  {W1, W2, W3, W4, W5, W6, W7, W8}.

map_udp_message({udp, Socket, Address, Port, Data}) ->
  {datagram, Socket, map_address(Address), Port, Data};
map_udp_message({udp_error, Socket, Error}) ->
  {udp_error, Socket, Error}.
