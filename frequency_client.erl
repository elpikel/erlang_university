-module(frequency_client).
-export([start/0,allocate/1,deallocate/2,stop/1]).
-export([init/0]).

start() ->
  spawn(frequency_client, init, []).

init() ->
  process_flag(trap_exit, true),
  loop().

allocate(Pid) ->
  frequency:allocate(Pid).

deallocate(Pid, Freq) ->
  frequency:deallocate(Pid, Freq).

stop(Pid) ->
  frequency:exit(Pid).

get_response() ->
  receive
    {reply, Reply} ->
      Reply
  end.

loop() ->
  receive
    {'EXIT', Pid, _Reason} ->
      io:format("server exited"),
      frequency:start(),
      loop();
    {reply, Reply} ->
      Reply,
      loop()
  end.
