-module(rps).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2]).

%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

play_two(_,_,PlaysL,PlaysR,0) ->
   tournament(PlaysL,PlaysR);
play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   play_two(
    StrategyL,
    StrategyR,
    [StrategyL(PlaysL)|PlaysL],
    [StrategyR(PlaysR)|PlaysR],
    N - 1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form

expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) ->
  io:format("rock vs rock = draw~n"),
  draw;
result(rock,paper) ->
  io:format("rock vs paper = lose~n"),
  lose;
result(rock,scissors) ->
  io:format("rock vs scissors = win~n"),
  win;
result(paper,rock) ->
  io:format("paper vs rock = win~n"),
  win;
result(paper,paper) ->
  io:format("paper vs paper = draw~n"),
  draw;
result(paper,scissors) ->
  io:format("paper vs scissors = lose~n"),
  lose;
result(scissors,rock) ->
  io:format("scissors vs rock = lose~n"),
  lose;
result(scissors,paper) ->
  io:format("scissors vs paper = win~n"),
  win;
result(scissors,scissors) ->
  io:format("scissors vs scissors = draw~n"),
  draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

%
% strategies.
%
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.

no_repeat([]) ->
    rand([]);
no_repeat([X|_]) ->
    pick(X).

pick(rock) ->
  lists:nth(rand:uniform(2), [scissors,paper]);
pick(scissors) ->
  lists:nth(rand:uniform(2), [rock,paper]);
pick(paper) ->
  lists:nth(rand:uniform(2), [rock,scissors]).

const(Play) ->
    Play.

cycle([]) -> scissors;
cycle([X|_Xs]) ->
  CycleOrder = [scissors,paper,rock],
  lists:nth(index_of(CycleOrder, X) + 1, [scissors,paper,rock,scissors]).

index_of(List, Elem) -> index_of(List, Elem, 1).

index_of([Elem|_Xs], Elem, Index) ->
  Index;
index_of([_X|Xs], Elem, Index) ->
  index_of(Xs, Elem, Index + 1).


rand(_) ->
    enum(rand:uniform(3) - 1).
