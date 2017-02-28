-module(list_exercises).
-export([double/1,evens/1,nub/1]).

double([]) -> [];
double([X|Xs]) -> double(Xs, [X*2]).

double([X|Xs], D) -> double(Xs, [X*2|D]);
double([], D) -> D.

evens([]) -> [];
evens([_Odd,Even|Rest]) -> evens(Rest, [Even]).

evens([], Evens) -> Evens;
evens([_Odd,Even|Rest], Evens) -> evens(Rest, [Even|Evens]);
evens(_Odd, Evens) -> Evens.

nub([]) -> [];
nub([X|Xs]) -> nub(Xs, [X]).

nub([], N) -> lists:reverse(N, []);
nub([X|Xs], Ns) ->
  case exists_in(X, Ns) of
    true -> nub(Xs, Ns);
    false -> nub(Xs, [X|Ns])
  end.

exists_in(X, Ns) ->
  lists:any(fun(N) -> N == X end, Ns).
