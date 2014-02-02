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
-export([start_link/3, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% API functions
-export([incr/2, incr/3]).

%% Admin functions
-export([report/1]).

-include("ectr_log.hrl").

-record(state, {name :: atom(),
                report_fn :: ectr:report_fn(),
                interval = timer:seconds(1) :: pos_integer(),
                tref :: reference()
               }).

-define(REPORT_MSG, report).

%% API functions
start_link(Name, ReportFn, Interval)
  when is_atom(Name),
       is_function(ReportFn, 2) orelse tuple_size(ReportFn) =:= 2,
       is_integer(Interval), Interval > 0 ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [Name, ReportFn, Interval], []).

incr(Name, Key) ->
    incr(Name, Key, 1).

incr(Name, Key, Incr)
  when is_atom(Name), is_integer(Incr) ->
    try ets:update_counter(Name, Key, Incr)
    catch error:badarg -> % Usually this is because the key doesn't exist.
            try ets:insert_new(Name, {Key, Incr})
            catch error:badarg -> % We lost the key creation race, just update.
                    %% Here, the catch allows us to avoid crashing if
                    %% the table really doesn't exist. This means the
                    %% table owning process can be restarted and
                    %% callers will be able to make progress.
                    catch ets:update_counter(Name, Key, Incr)
            end
    end.

report(Name) ->
    gen_server:call(Name, report, timer:seconds(30)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name, ReportFn, Interval]) ->
    ets:new(Name, [public, named_table, set, {write_concurrency, true}]),
    {ok, set_timer(#state{name = Name,
                          report_fn = ReportFn,
                          interval = Interval})}.

handle_call(report, _From, State) ->
    run_report(cancel_timer(State)),
    {reply, ok, set_timer(State)};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TRef, ?REPORT_MSG},
            State = #state{tref = TRef}) ->
    NewState = run_report(State#state{tref=undefined}),
    {noreply, set_timer(NewState)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @todo Move reporting to child process.
run_report(State = #state{name = Name}) ->
    TS = os:timestamp(),
    Stats = ets:tab2list(Name),
    try report(TS, Stats, State) of
        _ -> State
    catch
        C:E ->
            ?WARN("at=report class=~p error=~p stack=~1000P",
                  [C, E, erlang:get_stacktrace()]),
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

report(TS, Stats, #state{name = Name,
                         report_fn = Fn}) ->
    [ begin
          report_stat(Fn, TS, Stat),
          clear(Name, Stat)
      end
      || Stat = {_Key, Count} <- Stats,
         Count > 0], % Don't report 0 stats - no new sightings.
    ok.

report_stat(Fn, Ts, Stat) when is_function(Fn, 2) ->
    Fn(Ts, Stat);
report_stat({Mod, Fun}, Ts, Stat) ->
    Mod:Fun(Ts, Stat).

clear(_Name, {_Key, 0}) -> % Optimization to skip ops we don't need to do.
    ok;
clear(Name, {Key, Ctr}) ->
    ets:update_counter(Name, Key, Ctr * -1).
