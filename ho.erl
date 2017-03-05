-module(ho).
-export([doubleAll/1,evens/1,product/1]).

doubleAll(X) -> lists:map(fun double/1, X).
double(X) -> 2 * X.

evens(X) -> lists:filter(fun is_even/1, X).
is_even(X) -> X rem 2 == 0.

product(X) -> lists:foldl(fun(Item, Product) -> Item * Product end, 1, X).
