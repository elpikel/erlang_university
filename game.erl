-module(game).
-export([tournament/2]).

beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.

result(A,B) ->
  case {beat(A), lose(A)} of
    {B, _} -> -1;
    {_, B} -> 1;
    _ -> 0
  end.

% game:tournament([rock,rock,paper,paper],[rock,paper,scissors,rock]).
tournament(Hands1,Hands2) ->
  lists:sum(lists:zipwith(fun result/2,  Hands1, Hands2)).
