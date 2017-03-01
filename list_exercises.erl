-module(list_exercises).
-export([double/1,evens/1,takeAcc/2,take/2]).

double([]) -> [];
double([X|Xs]) -> double(Xs, [X*2]).

double([X|Xs], D) -> double(Xs, [X*2|D]);
double([], D) -> D.

evens([]) -> [];
evens([_Odd,Even|Rest]) -> evens(Rest, [Even]).

evens([], Evens) -> Evens;
evens([_Odd,Even|Rest], Evens) -> evens(Rest, [Even|Evens]);
evens([_Odd], Evens) -> Evens.

take(0, _Xs) -> [];
take(_N, []) -> [];
take(N,[X|Xs]) -> [X|take(N-1, Xs)].

takeAcc(HowMany, [X|Xs]) -> take(HowMany - 1, Xs, [X]).

take(0, _X, Taken) -> lists:reverse(Taken, []);
take(_HowMany, [], Taken) -> lists:reverse(Taken, []);
take(HowMany, [X|Xs], Taken) -> take(HowMany - 1, Xs, [X|Taken]).

