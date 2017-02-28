-module(shapes).
-export([perimeter/1,area/1,bits/1,bits1/1]).

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

perimeter({circle, R}) ->
  2 * math:pi() * R;
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


bits1(N) when N >= 0 -> do_bits1(N, 0, N rem 2). % Public API

do_bits1(0, Acc, _) -> Acc; 
do_bits1(N, _, Rem) -> Rem + bits1(N div 2).