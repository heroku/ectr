%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ECTR Report for-each execution library
%% @end
-module(ectr_each_report).

-export([new/1
        ,run/4
        ]).

-type report_fn() :: {Mod::atom(), Function::atom()} |
                     fun ((erlang:timestamp(),
                                  Key::term(), Count::non_neg_integer()) ->
                                        any()).
-type report() :: {?MODULE, report_fn()}.

-export_type([report/0]).

-spec new(report_fn()) -> report().
new({Mod, Function}) when is_atom(Mod), is_atom(Function) ->
    {?MODULE, {Mod, Function}};
new(Report) when is_function(Report, 3) ->
    {?MODULE, Report}.

run({?MODULE, Report}, TS, Tab, GC) ->
    ets:foldl(fun (Stat, Acc) ->
                      each_stat(Stat, TS, Report),
                      clear(Tab, Stat, GC),
                      Acc
              end,
              no_state,
              Tab),
    ok.

each_stat({Key, Count}, TS, Fn) when is_function(Fn) ->
    Fn(TS, Key, Count);
each_stat({Key, Count}, TS, {M,F}) ->
    M:F(TS, Key, Count).

clear(_Tab, {Key, 0}, GC) ->
    ectr_gc:mark(Key, GC);
clear(Tab, {Key, Ctr}, GC) ->
    ectr_gc:unmark(Key, GC),
    ets:update_counter(Tab, Key, Ctr * -1).
