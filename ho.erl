-module(ho).
-export([doubleAll/1,evens/1,product/1,zip/2]).

doubleAll(X) -> lists:map(fun double/1, X).
double(X) -> 2 * X.

evens(X) -> lists:filter(fun is_even/1, X).
is_even(X) -> X rem 2 == 0.

product(X) -> lists:foldl(fun(Item, Product) -> Item * Product end, 1, X).

%ho:zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]
zip(Xs, Ys) -> zip_with(fun(X,Y) -> X+Y end, Xs, Ys).

%zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]
zip_with(_Zip, [],[]) -> [];
zip_with(_Zip, [], _List) -> [];
zip_with(_Zip, _List, []) -> [];
zip_with(Zip, [X|Xs], [Y|Ys]) -> [Zip(X,Y)|zip_with(Zip, Xs, Ys)].
