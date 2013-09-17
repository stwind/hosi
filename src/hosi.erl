-module(hosi).

-export([start/0]).
-export([stop/0]).

-export([new/1]).
-export([done/1]).
-export([snapshot/1]).

-export([ctime/3]).

%% ===================================================================
%% Public
%% ===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

new(Id) ->
    hosi_worker_sup:new_worker(Id).

ctime(Pid, Label, Fun) when is_function(Fun), is_pid(Pid) ->
    {Micros, Result} = timer:tc(Fun),
    hosi_worker:ctime(Pid, Label, {Micros, micros}),
    Result;
ctime(Pid, Label, {Time, Unit}) when is_pid(Pid) ->
    hosi_worker:worker_ctime(Pid, Label, {Time, Unit});
ctime(_, _, Fun) when is_function(Fun) ->
    Fun();
ctime(_, _, _) ->
    ok.

snapshot(Pid) ->
    hosi_worker:snapshot(Pid).

done(Pid) ->
    hosi_worker:stop(Pid).
