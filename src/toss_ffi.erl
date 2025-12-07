-module(toss_ffi).

-export([active_once/0, passive/0, connect/3, recv/2, recv/3, send/2, send/4,
         map_udp_message/1, setopts/2, open/2]).

% Enable inlining, as all the functions are very small.
-compile(inline).

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

open(Port, Opts) ->
  try
    gen_udp:open(Port, Opts)
  catch
    exit:badarg ->
      {error, bad_argument}
  end.

connect(Socket, Address, Port) ->
  case gen_udp:connect(Socket, map_address(Address), Port) of
    ok ->
      {ok, Socket};
    Error ->
      Error
  end.

% Maps a hostname or IP to the erlang format
map_address(Address) ->
  case is_binary(Address) of
    true ->
      unicode:characters_to_list(Address);
    false ->
      Address
  end.

recv(Socket, Length, Timeout) ->
  map_recv_result(gen_udp:recv(Socket, Length, Timeout)).

recv(Socket, Length) ->
  map_recv_result(gen_udp:recv(Socket, Length)).

map_recv_result(Result) ->
  case Result of
    {ok, {Address, Port, Data}} ->
      {ok, {try_ip_address(Address), Port, Data}};
    Error ->
      Error
  end.

send(Socket, Data) ->
  map_send_result(gen_udp:send(Socket, Data)).

send(Socket, Address, Port, Data) ->
  map_send_result(gen_udp:send(Socket, map_address(Address), Port, Data)).

map_send_result(Result) ->
  case Result of
    ok ->
      {ok, nil};
    Error ->
      Error
  end.

map_udp_message({udp, Socket, Address, Port, Data}) ->
  {datagram, Socket, try_ip_address(Address), Port, Data};
map_udp_message({udp_error, Socket, Error}) ->
  {udp_error, Socket, Error}.

try_ip_address(Address) ->
  case inet:is_ip_address(Address) of
    true ->
      {ok, Address};
    false ->
      {error, nil}
  end.
