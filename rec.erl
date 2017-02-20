-module(rec).
-export([fib/1,pieces/1,perfect/1]).

fib(N) when N >= 0 ->
  fib(N, 1, 1).

fib(0, _, B) -> B;
fib(N, A, B) ->
  fib(N-1, A+B, A).

pieces(0) -> 1;
pieces(N) ->
  pieces(N-1) + N.

perfect(N) ->
  perfect(N, N - 1, 0) == N.

perfect(_, 1, A) -> A + 1;
perfect(N, D, A) ->
  case (N rem D) == 0 of
    true -> perfect(N, D - 1, A + D);
    false -> perfect(N, D -1, A)
  end.
