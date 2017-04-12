-module(exceptions).
-export([eval/2,wrap/2,lookup/2,try_lookup/2]).

% simple model of arithmetical expressions, like
% {add, {num, 0}, {mul, {num,1}, {var,a2}}}

% environment gives value of a variable:
% [ {a,2}, {b,1} ]

eval(_Env,{num,N}) -> N;

eval(Env,{var,V}) ->
    lookup(V,Env);

eval(Env,{mul, E1, E2}) ->
    eval(Env,E1) * eval(Env,E2);

eval(Env,{add, E1, E2}) ->
    eval(Env,E1) + eval(Env,E2);

eval(Env,{'div',Num,Denom}) ->
    N = eval(Env,Num),
    D = eval(Env,Denom),
    case D of
        0 -> throw(div_by_zero);
        _NZ -> N div D
    end.

wrap(Env,Exp) ->
    try eval (Env,Exp) of
        Res -> {ok, Res}
    catch
        throw:div_by_zero -> {error,div_by_zero}
    end.

% lookup
% this version assumes it doesn't fail …
% … but could use exceptions to deal with failure (exercise!)

lookup(_X, []) -> throw(element_not_found);
lookup(X, [{X,N}|_]) -> N;
lookup(X, [_|Rest]) -> lookup(X,Rest).

try_lookup(X, Arr) ->
  try lookup(X, Arr) of
    Found -> {ok, Found}
  catch
    throw:element_not_found -> {error, element_not_found}
  end.
