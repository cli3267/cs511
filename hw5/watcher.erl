-module(watcher).
-compile(compile_all).
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


