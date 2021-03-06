%%%-------------------------------------------------------------------
%% @copyright Geoff Cant (2014)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc CommonTest test suite for ectr_gc
%% @end
%%%-------------------------------------------------------------------

-module(ectr_gc_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom.

all() ->
    [mark_unmark,
     mark_sweep].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    Config.

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(CaseName, Config) ->
    [{gc, ectr_gc:init_table(ectr_gc:new(CaseName, 1))} | Config].

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    ectr_gc:delete_table(?config(gc, Config)),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

mark_unmark(Config) ->
    GC = ?config(gc, Config),
    GCTab = ectr_gc:ets_tab(GC),
    Key = somekey,
    [] = ets:lookup(GCTab, Key),
    ectr_gc:mark(Key, GC),
    [{Key, 1}] = ets:lookup(GCTab, Key),
    ectr_gc:mark(Key, GC),
    [{Key, 2}] = ets:lookup(GCTab, Key),
    ectr_gc:unmark(Key, GC),
    [] = ets:lookup(GCTab, Key),
    ok.

mark_sweep(Config) ->
    GC = ?config(gc, Config),
    GCTab = ectr_gc:ets_tab(GC),
    Key = somekey,
    [] = ets:lookup(GCTab, Key),
    ectr_gc:mark(Key, GC),
    [{Key, 1}] = ets:lookup(GCTab, Key),
    [Key] = ectr_gc:sweep(GC),
    []= ets:lookup(GCTab, Key),
    ok.
