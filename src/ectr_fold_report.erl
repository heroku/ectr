%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ECTR Report execution library
%% @end
-module(ectr_fold_report).

-export([new/1
        ,run/4
        ]).

-type report_state() :: term().

-callback report_init(erlang:timestamp()) ->
    report_state().

-callback report_fold(Key::term(), Count::non_neg_integer(), report_state()) ->
    report_state().

-callback report_finish(report_state()) -> any().

-type report() :: {?MODULE, Mod::atom()}.
-export_type([report/0]).

-spec new(Mod::atom()) -> report().
new(Mod) when is_atom(Mod) ->
    {?MODULE, Mod}.

-spec run(report(), TS::erlang:timestamp(), Tab::ets:tab(), GC::ectr_gc:gc()) ->
                 any().
run({?MODULE, Mod}, TS, Tab, GC) ->
    Final = ets:foldl(fun (Stat, Acc) ->
                              fold_stat(Stat, Mod, Tab, GC, Acc),
                              Acc
                      end,
                      Mod:report_init(TS),
                      Tab),
    Mod:report_finish(Final),
    ok.

fold_stat({Key, Count} = Stat, Tab, Mod, GC, Report) ->
    UpdatedReport = Mod:report_fold(Key, Count, Report),
    clear(Tab, Stat, GC),
    UpdatedReport.

clear(_Tab, {Key, 0}, GC) ->
    ectr_gc:mark(Key, GC);
clear(Name, {Key, Ctr}, GC) ->
    ectr_gc:unmark(Key, GC),
    ets:update_counter(Name, Key, Ctr * -1).
