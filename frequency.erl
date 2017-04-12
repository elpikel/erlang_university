%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0,start/0,allocate/1,deallocate/2,exit/1]).

%% Start frequency server with frequency as a name
start() ->
  Pid = spawn_link(frequency, init, []),
  register(frequency, Pid),
  Pid.

%% These are the start functions used to create and
%% initialize the server.

init() ->
  %process_flag(trap_exit, true),
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% Public contract of Frequency server

%% Allocates frequency if it is not already taken
allocate(Pid) ->
  frequency ! {request, Pid, allocate}.

%% Deallocates frequency if it is already allocated
deallocate(Pid, Freq) ->
  frequency ! {request, Pid, {deallocate, Freq}}.

exit(Pid) ->
  frequency ! {'EXIT', Pid, "just want to"}.

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq, Pid),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  case is_allocated(Pid, Allocated) of
    true  ->
      link(Pid),
      {{Free, Allocated}, {cannot_alocate_more_than_one}};
    false -> {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq, Pid) ->
  case is_allocated(Pid, Allocated) of
    true  ->
      unlink(Pid),
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free],  NewAllocated};
    false ->
      {Free, Allocated}
  end.

exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid, 2, Allocated) of
    {value, {Freq, Pid}} ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free],NewAllocated};
    false ->
      {Free, Allocated}
  end.

is_allocated(Pid, Allocated) ->
  lists:any(fun({_FreqToCheck, PidToCheck}) -> Pid == PidToCheck end, Allocated).
