-module(cons).
-export([join/2,concat/1]).

join([], Y) -> Y;
join(X, []) -> X;
join([], []) -> [];
join(Xs, Ys) ->
  join(Xs, Ys, []).

join([], [], Acc) -> lists:reverse(Acc, []);
join([], [Y|Ys], Acc) ->
  join([], Ys, [Y|Acc]);
join([X|Xs], Ys, Acc) ->
  join(Xs, Ys, [X|Acc]).

concat([]) -> [];
concat([X1,X2|Xs]) -> concat([join(X1,X2)|Xs]);
concat([X|Xs]) -> X.
