-module(dc).

-compile(export_all).

sem(0) -> receive {release, _From} -> sem(1) end;
sem(P) when P > 0 ->
    receive
      {acquire, From} -> From ! {self(), ok}, sem(P - 1);
      {release, _From} -> sem(P + 1)
    end.

dryCleaner ( Clean , Dirty ) -> 
    receive 
        { dropOff , From , Ref } -> 
            dryCleaner ( Clean , Dirty + 1 ); 
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
    DC!{dropOff, self(), DC},
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
