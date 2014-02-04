%%%-------------------------------------------------------------------
%% @copyright Geoff Cant (2014)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc ETS Counter Table Library public API
%% @end
%%%-------------------------------------------------------------------

-module(ectr).

%% API
-export([start_link/3
         ,incr/2
         ,incr/3
        ]).

-type counter_key() :: term().
-type stat() :: {Key::term(), Counter::non_neg_integer()}.
-type report_fn() :: fun ((erlang:timestamp(), stat()) -> any()) |
                     {Module::atom(), Fun::atom()}.

-export_types([stat/0
              ,report_fn/0
              ,counter_key/0
              ]).

%%====================================================================
%% API
%%====================================================================

-spec start_link(Name::atom(),
                 report_fn(),
                 IntervalMS::pos_integer()) ->
                        {ok, pid()} |
                        {error, term()}.
start_link(Name,
           ReportFunction,
           IntervalMS) ->
    ectr_srv:start_link(Name,
                        ReportFunction,
                        IntervalMS).

-spec incr(Tid::ets:tid(),
           Key::counter_key()) -> any().
incr(Name, Key) ->
    incr(Name, Key, 1).

-spec incr(Tid::ets:tid(),
           Key::counter_key(),
           Increment::integer()) -> any().

incr(Tid, Key, Incr)
  when is_atom(Tid) orelse is_integer(Tid),
       is_integer(Incr) ->
    try ets:update_counter(Tid, Key, Incr)
    catch error:badarg -> % Usually this is because the key doesn't exist.
            try ets:insert_new(Tid, {Key, Incr})
            catch error:badarg -> % We lost the key creation race, just update.
                    %% Here, the catch allows us to avoid crashing if
                    %% the table really doesn't exist. This means the
                    %% table owning process can be restarted and
                    %% callers will be able to make progress.
                    catch ets:update_counter(Tid, Key, Incr)
            end
    end.
