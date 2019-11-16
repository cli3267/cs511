-module(ex8).
-compile(export_all).


counter_loop(S) ->
    receive
        {bump} ->
            counter_loop(S+1);
        {read, From, Ref} ->
            From!{self(), Ref, S},
            counter_loop(S);
        {stop} ->
            ok
    end.

turnstile1(0,_C) ->
    ok; %C!{stop}

turnstile1(N,C) when N > 0 ->
    C!{bump},
    turnstile1(N-1, C).

turnstile2(0,_C) ->
    ok;

turnstile2(M,C) when M > 0->
    C!{bump},
    turnstile2(M-1, C).

startT(N,M) ->
    C = spawn(?MODULE, counter_loop, [0]), % spawns the counter
    spawn(?MODULE, turnstile1, [N,C]), % spawns one turnstile
    spawn(?MODULE, turnstile2, [M,C]). % spawns other turnstile
