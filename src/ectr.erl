%%%-------------------------------------------------------------------
%% @copyright Geoff Cant (2014)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc ETS Counter Table Library public API
%% @end
%%%-------------------------------------------------------------------

-module(ectr).

%% API
-export([start_link/4
         ,incr/2
         ,incr/3
         ,new_gc/2
        ]).

-type counter_key() :: term().

-export_type([counter_key/0]).

%%====================================================================
%% API
%%====================================================================

-spec start_link(Name::atom(),
                 ectr_report:report(),
                 IntervalMS::pos_integer(),
                 ectr_gc:gc()) ->
                        {ok, pid()} |
                        {error, _} |
                        ignore.
start_link(Name,
           Report,
           IntervalMS,
           GC) ->
    ectr_srv:start_link(Name,
                        Report,
                        IntervalMS,
                        GC).

new_gc(Name, MarkThreshold) ->
    ectr_gc:new(Name, MarkThreshold).

-spec incr(Tab::ets:tab(),
           Key::counter_key()) -> any().
incr(Name, Key) ->
    incr(Name, Key, 1).

-spec incr(Tab::ets:tab(),
           Key::counter_key(),
           Increment::integer()) -> any().

incr(Tab, Key, Incr)
  when is_atom(Tab) orelse is_integer(Tab),
       is_integer(Incr) ->
    try ets:update_counter(Tab, Key, Incr)
    catch error:badarg -> % Usually this is because the key doesn't exist.
            try ets:insert_new(Tab, {Key, Incr})
            catch error:badarg -> % We lost the key creation race, just update.
                    %% Here, the catch allows us to avoid crashing if
                    %% the table really doesn't exist. This means the
                    %% table owning process can be restarted and
                    %% callers will be able to make progress.
                    catch ets:update_counter(Tab, Key, Incr)
            end
    end.
