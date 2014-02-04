%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ETS Counter Table Garbage Collector
%% @end
-module(ectr_gc).

-export([init_table/1
         ,delete_table/1
         ,mark/2
         ,unmark/2
         ,sweep/2
        ]).

-spec init_table(atom()) -> ets:tab().
%% @private
%% @doc
%% Initializes the GC table for a counter table.
%% @end
init_table(Name) when is_atom(Name) ->
    ets:new(Name, [set, public]).

delete_table(Tid) ->
    ets:delete(Tid).

-spec mark(ets:tab(), Key::term()) -> any().
%% @doc
%% Marks a key for future deletion. (Typically used when a counter has
%% a 0 value during a reporting pass).
%% @end
mark(Tid, Key) ->
    ectr:incr(Tid, Key, 1).

-spec unmark(ets:tab(), Key::term()) -> any().
%% @doc
%% Removes any marks on a key. (Typically used when a counter has a
%% non-0 value during a reporting pass)
%% @end
unmark(Tid, Key) ->
    ets:delete(Tid, Key).

-spec sweep(ets:tab(), pos_integer()) -> [ Key::term() ].
%% @doc
%% Returns all keys that have at least MarkThreshold marks on them,
%% deleting their marks afterwards.
%% @end
sweep(Tid, MarkThreshold)
  when is_integer(MarkThreshold), MarkThreshold > 0 ->
    Keys = ets:select(Tid,
                      ets:fun2ms(fun ({K, Ctr})
                                       when Ctr >= MarkThreshold ->
                                         K
                                 end)),
    [ ets:delete(Tid, Key)
      || Key <- Keys ],
    Keys.
