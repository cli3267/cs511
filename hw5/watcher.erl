-module(watcher).
-compile(export_all).
-author("Christina Li and Brenden Brusberg").

watcher(S) ->
    receive 
	{'DOWN', _, process, Pid, anomalous_reading} ->
	    io:fwrite("Watcher: ~w, Termination Reason: ~w, Sensor: ~w~n", [self(), anomalous_reading, Pid]),
	    %TODO DELETE ID PID TUPPLE IN LIST THEN GET ID TO START NEW MONITORED SENSOR WITH AND ADD IT BACK TO THE LIST
	    %RemovedSensor = lists:delete({ID, Pid}, S),
	    %{ Pid, _ } = spawn_monitor(sensor, sensor, [ID, self()]),
	    %UpdatedSensor = lists:append([{ID, Pid}], RemovedSensor),
	    %io:fwrite("Watcher: ~w, Upated Sensors: ~w~n", [self(), UpdatedSensor]),
	    %watcher(UpdatedSensor);
	    watcher(S);
	{ID, Measurement} ->
	    io:fwrite("Sensor: ~w, Measurement: ~w~n", [ID, Measurement]),
	    watcher(S)
    end.

watcher(N_Start, N_End) ->
    S = lists:map(fun(ID)->
			    {Pid, _} = spawn_monitor(sensor, sensor, [ID, self()]),
			    {ID, Pid} end
		   , lists:seq(N_Start,N_End)),
    io:fwrite("Watcher: ~w, Initial List of Sensors: ~w~n", [self(), S]),
    watcher(S).    
    
    
			  
