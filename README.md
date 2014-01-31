ectr
=====

An Erlang ETS Counter Table library with a periodic reporting server.

Build
-----

    $ rebar get-deps compile

Run
---

    $ erl -pa ebin -env ERL_LIBS deps -s ectr
    > ectr_srv:start_link(my_counters,
                          fun (TS, {Key, Counter}) ->
                              error_logger:info_msg("time=~p key=~p count=~p",
                                                    [TS, Key, Counter])
                          end,
                          timer:seconds(5)).
    > ectr_srv:incr(my_counters, coffees_today, 1).
