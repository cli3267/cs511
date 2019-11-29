-module(watcher).
-compile(export_all).
-author("Christina Li and Brenden Brusberg").

watcher(S) ->
    io:fwrite("Watcher: ~w, Initial List of Sensors: ~w~n", [self(), S]),
    receive 
	{"DOWN", _, process, Pid2, {ID, Reason}} ->
	    io:fwrite("Watcher: ~w, Termination Reason: ~w, Sensor: ~w~n", [self(), Reason, ID]),
	    RemovedSensor = lists:delete({ID, Pid2}, S),
	    { Pid, _ } = spawn_monitor(sensor, sensor, [ID, self()]),
	    UpdatedSensor = lists:append([{ID, Pid}], RemovedSensor),
	    io:fwrite("Watcher: ~w, Upated Sensors: ~w~n", [self(), UpdatedSensor]),
	    watcher(UpdatedSensor);
	{ID, Measurement} ->
	    io:fwrite("Sensor: ~w, Measurement: ~w~n", [ID, Measurement]),
	    watcher(S)
    end.

watcher(N_Start, N_End) ->
    io:fwrite("PID ~w~n", [self()]),
    S = lists:apply(fun(ID)->
			    {Pid, _} = spawn_monitor(sensor, sensor, [ID, self()]),
			    {ID, Pid} end
		   , lists:seq(N_Start,N_End)),
    io:fwrite("WATCHER PID:~w, SENSOR LIST[{id,pid}]: ~w~n", [self(), S]), 
    watcher(S).    
    
    
			  
