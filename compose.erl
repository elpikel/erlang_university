-module(compose).
-export([comp/2, a/1, m/1, comp/1]).

a(X) -> X + 1.
m(X) -> X - 1.

comp(Functions) ->
  fun(X) ->
    lists:foldr(fun(Function, Arg) -> Function(Arg) end, X, Functions)
  end.

twice(Function) ->
  comp([Function, Funciton]).

id(X) -> X.
comp(F,G) -> fun(X) -> F(G(X)) end.

iterate(0) -> fun(X) -> id(X);
iterate(N) ->
  comp()
  fun(Function) ->
    lists:foldr(fun comp/2, fun id/1, lists:duplicate(N, Fn))
  end.
