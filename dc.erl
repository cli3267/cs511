-module(dc).
-compile(export_all).

dryCleaner (Clean,Dirty) -> 
    receive 
        {dropOffOverall} -> 
            dryCleaner (Clean , Dirty + 1 ); 
        { pickUpOverall , From , Ref } -> 
            if Clean > 0 -> 
                dryCleaner ( Clean - 1 , Dirty );
                true ->
                    receive
                        { clean , From , Ref } -> 
                            dryCleaner ( Clean, Dirty )
                    end
            end;
        { clean , From , Ref } -> 
            dryCleaner ( Clean + 1 , Dirty ) 
    end.


employee(DC) -> %drop off overall, then pick up a clean one
    DC!{dropOffOverall},
    DC!{pickUpOverall, self(), DC}.

dryCleanMachine(DC) -> %dry clean item, sleep for 1000 mili seconds and repeat
    timer:sleep(1000),
    DC ! {clean, self(), DC},
    dryCleanMachine(DC).

start(W, M) ->
    DC = spawn(?MODULE, dryCleaner, [0, 0]),
    [spawn(?MODULE, employee, [DC]) || _ <- list:seq(1, W)],
    [spawn(?MODULE, dryCleanMachine, [DC])
     || _ <- lists:seq(1, M)].
