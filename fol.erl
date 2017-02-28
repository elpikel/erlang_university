-module(fol).
-export([maximum/1, product/1]).

product([X|Xs]) -> product([X|Xs], 1).
product([], ProductAcc) -> ProductAcc;
product([X|Xs], ProductAcc) -> product(Xs, ProductAcc * X).

maximum([X|Xs]) -> maximum(Xs, X).
maximum([X|Xs], Max) -> maximum(Xs, max(Max, X));
maximum([], Max) -> Max.