-module(shapes).
-export([perimeter/1,area/1,bits/1]).

bits(N) ->
  bits(int_to_list_of_bits(N), 0).

bits([], S) ->
  S;
bits([H|L], S) ->
  bits(L, S + char_to_int(H)).

int_to_list_of_bits(N) ->
  hd(io_lib:format("~.2B", [N])).

char_to_int(H) ->
  {N, _} = string:to_integer([H]),
  N.

perimeter({triangle, [E|L]}) ->
  perimeter({shape, [E|L], 0});
perimeter({shape, [E|L]}) ->
  perimeter({shape, [E|L], 0});
perimeter({shape, [], S}) ->
  S;
perimeter({shape, [E|L], S}) ->
  perimeter({shape, L, S + E}).

area({triangle, B, H}) ->
 B * H / 2.
