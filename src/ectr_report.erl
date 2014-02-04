%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ECTR Report execution library
%% @end
-module(ectr_report).

-export([by_fold/4
        ,by_snapshot/4
        ]).

by_fold(TS, Tab, Fn, GCTid) ->
    ets:foldl(fun (Stat, Acc) ->
                      each_stat(Stat, TS, Tab, Fn, GCTid),
                      Acc
              end,
              undefined,
              Tab),
    ok.

by_snapshot(TS, Tab, Fn, GCTid) ->
    Stats = ets:tab2list(Tab),
    [ each_stat(Stat, TS, Tab, Fn, GCTid)
      || Stat <- Stats ],
    ok.

each_stat(Stat, TS, Tab, Fn, GCTid) ->
    report_stat(Fn, TS, Stat),
    clear(Tab, Stat, GCTid).

report_stat(Fn, Ts, Stat) when is_function(Fn, 2) ->
    Fn(Ts, Stat);
report_stat({Mod, Fun}, Ts, Stat) ->
    Mod:Fun(Ts, Stat).

clear(_Tab, {Key, 0}, GCTid) ->
    ectr_gc:mark(GCTid, Key);
clear(Name, {Key, Ctr}, GCTid) ->
    ectr_gc:unmark(GCTid, Key),
    ets:update_counter(Name, Key, Ctr * -1).
