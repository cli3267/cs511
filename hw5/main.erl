-module(main).
-compile(export_all).
-author("Christina Li and Brenden Brusberg").

setup_loop(N, Num_watchers) ->
    setup_loop(N, Num_watchers, 0).

setup_loop(N, Num_watchers, Count) ->
    case Num_watchers of
	Num when Num > 1 ->
	    N_Start = Count * 10,
	    N_End = min(N-1, (Count*10)+9),
	    spawn(watcher, watcher, [N_Start, N_End]),
	    setup_loop(N, Num_watchers-1, Count+1);
	Num ->
	    N_Start = Count * 10,
	    N_End = min(N-1, (Count*10)+9),
	    spawn(watcher, watcher, [N_Start, N_End])
    end.

start () ->
    {ok , [ N ]} = io:fread("enter  number  of sensors > ", "~d"),
    if N =< 1 ->
	    io:fwrite("setup: range  must be at  least  2~n", []);
       true  ->
	    Num_watchers = 1 + (N div 10),
	    setup_loop(N, Num_watchers)
    end.
