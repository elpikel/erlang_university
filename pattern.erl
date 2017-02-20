-module(pattern).
-export ([maxThree/3,howManyEqual/3]).

maxThree(A,B,C) when is_integer(A) andalso is_integer(B) andalso is_integer(C) ->
    max(max(A,B),C).

howManyEqual(A,A,A) -> 3;
howManyEqual(A,A,_) -> 2;
howManyEqual(A,_,A) -> 2;
howManyEqual(_,A,A) -> 2;
howManyEqual(_,_,_) -> 0.
