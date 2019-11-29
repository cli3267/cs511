-module(watcher).
-compile(export_all).
-author("Christina Li and Brenden Brusberg").

watcher(S) ->
  receive 
	  {'DOWN', _, process, Pid, anomalous_reading} ->
	    %TODO DELETE ID PID TUPPLE IN LIST THEN GET ID TO START NEW MONITORED SENSOR WITH AND ADD IT BACK TO THE LIST
      {Sid, _} = lists:keyfind(Pid, 2, S),
	    io:fwrite("Watcher: ~w, Termination Reason: ~w, Sensor: ~w~n", [self(), anomalous_reading, Sid]),
      RemovedSensor = lists:delete({Sid, Pid}, S),
      {Pid2, _} = spawn_monitor(sensor, sensor, [Sid, self()]),
      io:fwrite("Updated Pid: ~w for Sensor ~w ~n", [Pid2, Sid]),
      UpdatedSensor = lists:append([{Sid, Pid2}], RemovedSensor),
      io:fwrite("Watcher: ~w, Upated Sensors: ~w~n", [self(), UpdatedSensor]),
	    watcher(UpdatedSensor);
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
    
    
			  
