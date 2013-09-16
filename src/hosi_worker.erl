-module(hosi_worker).

-export([start_link/1]).
-export([ctime/3]).
-export([snapshot/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          id = <<>> :: binary(),
          metrics = dict:new() :: dict(),
          start_time = os:timestamp() :: erlang:timestamp()
         }).

-record(ctimer, {count = 0 :: non_neg_integer(),
                 time = 0 :: non_neg_integer()}).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

ctime(Pid, Label, {Time, Unit}) ->
    gen_server:cast(Pid, {ctime_time, Label, {Time, Unit}}).

snapshot(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, snapshot).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Id]) ->
    {ok, #state{ id = Id }}.

handle_call(snapshot, _From, 
            #state{start_time=StartTime, metrics=Metrics} = State) ->
    Elapsed = timer:now_diff(os:timestamp(), StartTime) div 1000,
    {reply, make_log_tuples(Elapsed, Metrics), State};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({ctime_time, Label, {Time, Unit}}, 
            #state{ metrics=Metrics } = State) ->
    CTimer = fetch_ctimer(Label, Metrics),
    CTimer1 = update_ctimer(CTimer, {Time, Unit}),
    {noreply, State#state{metrics = store_ctimer(Label, CTimer1, Metrics)}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

fetch_ctimer(Label, Metrics) ->
    case dict:find(Label, Metrics) of
        error ->
            %% Create a new ctimer. It is the caller's responsibility to make sure it ends
            %% up saved in the Metrics dict.
            #ctimer{};
        {ok, #ctimer{} = CTimer} ->
            %% match on record type so we crash if user mismatches labels and type.
            CTimer
    end.

update_ctimer(#ctimer{}=CTimer, {AddTime, micros}) ->
    update_ctimer(CTimer, {AddTime div 1000, ms});
update_ctimer(#ctimer{count = Count, time = Time}, {AddTime, ms}) ->
    #ctimer{count = Count + 1, time = Time + AddTime}.

store_ctimer(Label, #ctimer{}=CTimer, Metrics) ->
    dict:store(Label, CTimer, Metrics).

make_log_tuples(Elapsed, Metrics) ->
    Ans = dict:fold(fun(Label, #ctimer{}=CTimer, Acc) ->
                            [A, B] = ctimer_to_list(Label, CTimer),
                            [A, B | Acc];
                       (_, _, Acc) -> Acc end, [], Metrics),
    [{<<"elapsed">>, Elapsed}| Ans].

%% Turn a #ctimer{} into a proplist appropriate for logging
ctimer_to_list(Label, #ctimer{count = Count, time = Time}) when is_binary(Label) ->
    [{<<Label/binary, "_time">>, Time}, {<<Label/binary, "_count">>, Count}].
