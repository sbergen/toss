-module(toss_ffi).

-export([active_once/0, passive/0, connect/3, recv/3, send/2, send/4]).

active_once() ->
   once.

passive() ->
   false.

connect(Socket, Address, Port) ->
   case gen_udp:connect(Socket, unmap_address(Address), Port) of
	ok -> { ok, nil };
	Error -> Error
   end.

recv(Socket, Length, Timeout) ->
   case gen_udp:recv(Socket, Length, Timeout) of
	{ ok, { Address, Port, Data }} -> { ok, { map_address(Address), Port, Data } };
	Error -> Error
   end.

send(Socket, Data) ->
   case gen_udp:send(Socket, Data) of
	ok -> { ok, nil };
	Error -> Error
   end.

send(Socket, Address, Port, Data) ->
   case gen_udp:send(Socket, unmap_address(Address), Port, Data) of
	ok -> { ok, nil };
	Error -> Error
   end.

map_address(Address) when is_list(Address) ->
   { hostname, unicode:characters_to_binary(Address) } ;
map_address({ O1, O2, O3, O4 }) ->
   { ipv4, O1, O2, O3, O4 };
map_address({ W1, W2, W3, W4, W5, W6, W7, W8 }) ->
   { ipv6, W1, W2, W3, W4, W5, W6, W7, W8 }.

unmap_address({ hostname, Binary }) ->
   unicode:characters_to_list(Binary);
unmap_address({ ipv4, O1, O2, O3, O4 }) ->
   { O1, O2, O3, O4 };
unmap_address({ ipv6, W1, W2, W3, W4, W5, W6, W7, W8 }) ->
   { W1, W2, W3, W4, W5, W6, W7, W8 }.
