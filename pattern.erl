-module(pattern).
-export ([maxThree/3,howManyEqual/3]).

maxThree(A,B,C) when A >= B, A >= C ->
  A;
maxThree(A,B,C) when B >= A, B >= C ->
  B;
maxThree(A,B,C) when C >= A, C >= B ->
  C.

howManyEqual(A,A,A) ->
  3;
howManyEqual(A,A,_) ->
  2;
howManyEqual(A,_,A) ->
  2;
howManyEqual(_,A,A) ->
  2;
howManyEqual(_,_,_) ->
  0.
