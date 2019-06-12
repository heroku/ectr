%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Copyright (c) 2014 Heroku <nem@erlang.geek.nz>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(ectr_srv).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([start_link/4]).

%% Admin functions
-export([report/1
        ,alter_interval/2
        ]).

-include("ectr_log.hrl").

-record(report, {pid :: pid(),
                 started_at = os:timestamp() :: erlang:timestamp()}).

-record(state, {name :: atom(),
                report :: ectr_report:report(),
                interval = timer:seconds(1) :: pos_integer(),
                tref :: undefined | reference(),
                gc :: ectr_gc:gc(),
                report_job :: #report{} | 'undefined'
               }).

-define(REPORT_MSG, report).

%% API functions
-spec start_link(Name::atom(), Report::ectr_report:report(),
                 Interval::pos_integer(), GC::ectr_gc:gc()) ->
                        {ok, pid()} | {error, any()} | ignore.
start_link(Name, Report, Interval, GC)
  when is_atom(Name),
       is_integer(Interval), Interval > 0 ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [Name, Report, Interval, GC], []).

report(Name) ->
    gen_server:call(Name, report, timer:seconds(30)).

alter_interval(Name, Interval)
    when is_integer(Interval), Interval > 0 ->
    gen_server:call(Name, {alter_interval, Interval}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, Report, Interval, GC]) ->
    process_flag(trap_exit, true),
    _Tid = init_table(Name),
    State = #state{name = Name,
                   report = Report,
                   interval = Interval,
                   gc = ectr_gc:init_table(GC)},
    {ok, set_timer(State)}.

handle_call(report, _From, State = #state{report_job = R = #report{}}) ->
    {reply, {report_running, R}, State};

handle_call(report, _From, State = #state{report_job = undefined}) ->
    NewState = run_report(cancel_timer(State)),
    {reply, ok, set_timer(NewState)};

handle_call({alter_interval, NewInterval}, _From,
            State) when is_integer(NewInterval),
                        NewInterval > 0 ->
    {reply, ok, State#state{interval = NewInterval}};

handle_call(Msg, From, State) ->
    ?INFO("at=unexpected_call msg=~p from=~p",
          [Msg, From]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?INFO("at=unexpected_cast msg=~p",
          [Msg]),
    {noreply, State}.

%% Report timer fires with report still running:
handle_info({timeout, TRef, ?REPORT_MSG},
            State = #state{tref = TRef,
                           report_job = #report{}}) ->
    {noreply, set_timer(State#state{tref=undefined})};

%% Report timer fired - time to kick off a report.
handle_info({timeout, TRef, ?REPORT_MSG},
            State = #state{tref = TRef,
                           report_job = undefined}) ->
    NewState = run_report(State#state{tref=undefined}),
    {noreply, set_timer(NewState)};

handle_info({'EXIT', Pid, Reason},
            State = #state{interval = IntervalMS,
                           report_job = #report{pid = Pid,
                                                started_at = Start}}) ->
    Elapsed = timer:now_diff(os:timestamp(), Start),
    IntervalUS = skew_tolerance() * IntervalMS * 1000,
    case Reason of
        normal when Elapsed =< IntervalUS ->
            ok;
        normal ->
            ?INFO("at=report_completion result=success "
                  "elapsed=~p error=report_ran_too_slowly started_at=~p",
                  [Elapsed, unix_ts(Start)]);
        Else ->
            ?WARN("at=report_completion result=error "
                  "error=~p elapsed=~p started_at=~p",
                  [Else, Elapsed, unix_ts(Start)])
    end,
    {noreply, State#state{report_job = undefined}};

handle_info(Info, State) ->
    ?INFO("at=unexpected_info msg=~p",
          [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

init_table(Name) ->
    ets:new(Name, [public, named_table, set,
                   {write_concurrency, true},
                   {read_concurrency, true}]).

%% @todo Move reporting to child process.
run_report(State = #state{name = Tab,
                          report = Report,
                          gc = GC,
                          report_job = undefined }) ->
    TS = os:timestamp(),
    case ectr_report:start_link(Report, TS, Tab, GC) of
        {ok, Pid} ->
            State#state{report_job = #report{pid = Pid,
                                             started_at = TS} };
        Else ->
            ?WARN("at=report_failure reason=~p", [Else]),
            State
    end.

set_timer(State = #state{tref=undefined,
                         interval = MS}) ->
    TRef = erlang:start_timer(MS, self(), ?REPORT_MSG),
    State#state{tref = TRef}.

cancel_timer(State = #state{tref=TRef}) when is_reference(TRef) ->
    case erlang:cancel_timer(TRef) of
        false ->
            %% Flush msg q.
            receive
                {timeout, TRef, _} -> ok
            after 0 -> ok
            end;
         _Time -> ok
    end,
    State#state{tref = undefined}.

unix_ts({Mega, S, _Micros}) ->
    Mega * 1000000 + S.

skew_tolerance() ->
    case application:get_env(ectr, skew_tolerance) of
        {ok, Value} when is_integer(Value) ->
            Value;
        _ ->
            5
    end.
