-module(simple_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

%% starts supervisor with given Name
%% Children is list with tuples: {Module, Function, Arguments}
%% used to starting child. Child process must spawn_link in order
%% to connect trap exit works. Child process should return Pid.
start_link(Name, Children) ->
  Pid = spawn_link(simple_supervisor, init, Children),
  register(Name, Pid),
  ok.

init(Children) ->
  process_flag(trap_exit, true),
  loop(start_children([Children])).

start_children([]) -> [];
start_children([{Module, Function, Arguments} | Children]) ->
  case (catch apply(Module,Function,Arguments)) of
    {ok, Pid} ->
      [{Pid, {Module,Function,Arguments}}|start_children(Children)];
    _ ->
      start_children(Children)
  end.

%% restarts given process and updated Children
restart_child(Pid, Children) ->
  {value, {Pid, {Module,Function,Arguments}}} = lists:keysearch(Pid, 1, Children),
  {ok, NewPid} = apply(Module,Function,Arguments),
  [{NewPid, {Module,Function,Arguments}}|lists:keydelete(Pid,1,Children)].

%% restarts child that sends exit signal
loop(Children) ->
  receive
    {'EXIT', Pid, _Reason} ->
      NewChildren = restart_child(Pid, Children),
      loop(NewChildren);
    {stop, From}  ->
      From ! {reply, terminate(Children)}
  end.

%% stops all Children
stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

%% sends kill signal to all Children
terminate([]) -> ok;
terminate([{Pid, _} | Children]) ->
  exit(Pid, kill),
  terminate(Children).
