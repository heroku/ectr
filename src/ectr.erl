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

-spec incr(Name::atom(),
           Key::counter_key()) -> any().
incr(Name, Key) ->
    ectr_srv:incr(Name, Key).

-spec incr(Name::atom(),
           Key::counter_key(),
           Increment::integer()) -> any().
incr(Name, Key, Incr) ->
    ectr_srv:incr(Name, Key, Incr).
