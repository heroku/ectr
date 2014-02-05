ectr
=====

An Erlang ETS Counter Table library with a periodic reporting server.

Build
-----

    $ rebar get-deps compile

Run
---

    $ erl -pa ebin -env ERL_LIBS deps -s ectr
    
    ReportFn = fun (TS, Key, Counter) ->
                       Time = calendar:now_to_datetime(TS),
                       error_logger:info_msg("time=~p key=~p count=~p",
                                             [Time, Key, Counter])
               end.
    ectr_srv:start_link(my_counters,
                        ectr:new_each_report(ReportFn),
                        timer:seconds(5),
                        ectr:new_gc(my_counters_gc, 1)).
    ectr:incr(my_counters, coffees_today, 1).
