-module(first).
-export([double/1,area/3,square/1,treble/1]).

multi(X,Y) -> 
  X*Y.

double(X) ->
  multi(X,2).

square(X) ->
  multi(X, X).

treble(X) ->
  multi(multi(X,X),X).

area(A,B,C) ->
  S = (A+B+C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).
