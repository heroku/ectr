%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ETS Counter Table Garbage Collector
%% @end
-module(ectr_gc).

-export([new/2
         ,init_table/1
         ,delete_table/1
         ,mark/2
         ,unmark/2
         ,sweep/1
         ,sweep/2
        ]).

-export([ets_tab/1]).

-include_lib("stdlib/include/ms_transform.hrl").

-record(gc, {name :: atom(),
             mark_threshold :: pos_integer(),
             tab :: ets:tab()}).

-opaque gc() :: #gc{}.

-export_type([gc/0]).

-spec new(Name::atom(), MarkThreshold::pos_integer()) -> gc().
new(Name, MarkThreshold)
  when is_integer(MarkThreshold), MarkThreshold > 0 ->
    #gc{name = Name,
        mark_threshold = MarkThreshold}.

ets_tab(#gc{tab = Tab}) -> Tab.

-spec init_table(gc()) -> gc().
%% @private
%% @doc
%% Initializes the GC table for a counter table.
%% @end
init_table(GC = #gc{name = Name}) when is_atom(Name) ->
    GC#gc{tab = ets:new(Name, [set, public])}.

delete_table(#gc{tab = Tab}) ->
    ets:delete(Tab).

-spec mark(Key::term(), #gc{}) -> any().
%% @doc
%% Marks a key for future deletion. (Typically used when a counter has
%% a 0 value during a reporting pass).
%% @end
mark(Key, #gc{tab = Tab}) ->
    ectr:incr(Tab, Key, 1).

-spec unmark(Key::term(), #gc{}) -> any().
%% @doc
%% Removes any marks on a key. (Typically used when a counter has a
%% non-0 value during a reporting pass)
%% @end
unmark(Key, #gc{tab = Tab}) ->
    ets:delete(Tab, Key).

-spec sweep(#gc{}) -> [ Key::term() ].
%% @doc
%% Returns all keys that have at least MarkThreshold marks on them,
%% deleting their marks afterwards.
%% @end
sweep(#gc{tab = Tab, mark_threshold = MarkThreshold}) ->
    Keys = ets:select(Tab,
                      ets:fun2ms(fun ({K, Ctr})
                                       when Ctr >= MarkThreshold ->
                                         K
                                 end)),
    [ ets:delete(Tab, Key)
      || Key <- Keys ],
    Keys.

-spec sweep(Tab::ets:tab(), gc()) -> any().
sweep(MainTable, GC = #gc{}) ->
    [ ets:delete_object(MainTable, {Key, 0})
      || Key <- sweep(GC) ].
