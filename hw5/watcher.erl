-module(watcher).
-compile(export_all).
-author("Christina Li and Brenden Brusberg").

watcher(S) ->
  receive 
    {"DOWN", _, process, Pid2, Reason} ->
        {SensorID, _} = lists:keyfind(Pid2, N, S), %%% idk what N should be 
        io:fwrite("Watcher: ~w, Termination Reason: ~w", [self(), Reason]),
        RemovedSensor = lists:delete({SensorID, Pid2}, S),
        { Pid, _ } = spawn_monitor(sensor, sensor, [SensorID, self()]),
        UpdatedSensor = lists:append([{SensorID, Pid}], RemovedSensor),
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
    
    
			  
