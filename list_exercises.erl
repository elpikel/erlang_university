-module(list_exercises).
-export([double/1,evens/1]).

double([]) -> [];
double([X|Xs]) -> double(Xs, [X*2]).

double([X|Xs], D) -> double(Xs, [X*2|D]);
double([], D) -> D.

evens([]) -> [];
evens([_Odd,Even|Rest]) -> evens(Rest, [Even]).

evens([], Evens) -> Evens;
evens([_Odd,Even|Rest], Evens) -> evens(Rest, [Even|Evens]);
evens([Odd], Evens) -> Evens.
