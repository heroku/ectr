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
-export([start_link/3]).

%% Admin functions
-export([report/1]).

-include("ectr_log.hrl").

-record(state, {name :: atom(),
                report_fn :: ectr:report_fn(),
                interval = timer:seconds(1) :: pos_integer(),
                tref :: reference(),
                gc_tid :: ets:tab()
               }).

-define(REPORT_MSG, report).

%% API functions
start_link(Name, ReportFn, Interval)
  when is_atom(Name),
       is_function(ReportFn, 2) orelse tuple_size(ReportFn) =:= 2,
       is_integer(Interval), Interval > 0 ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [Name, ReportFn, Interval], []).

report(Name) ->
    gen_server:call(Name, report, timer:seconds(30)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, ReportFn, Interval]) ->
    _Tid = init_table(Name),
    State = #state{name = Name,
                   report_fn = ReportFn,
                   interval = Interval,
                   gc_tid = ectr_gc:init_table(Name)},
    {ok, set_timer(State)}.

handle_call(report, _From, State) ->
    run_report(cancel_timer(State)),
    {reply, ok, set_timer(State)};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TRef, ?REPORT_MSG},
            State = #state{tref = TRef}) ->
    run_report(State#state{tref=undefined}),
    {noreply, set_timer(State)};

handle_info(_Info, State) ->
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
run_report(#state{name = Name,
                  report_fn = Fn,
                  gc_tid = GCTid}) ->
    TS = os:timestamp(),
    ?INFO("at=report_begin name=~p", [Name]),
    try ectr_report:by_snapshot(TS, Name, Fn, GCTid) of
        _ -> ok
    catch
        C:E ->
            ?WARN("at=report_failed class=~p error=~p stack=~1000P",
                  [C, E, erlang:get_stacktrace()]),
            ok
    end,
    ?INFO("at=report_end name=~p elapsed=~p",
          [Name, timer:now_diff(TS, os:timestamp())]),
    ok.

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
