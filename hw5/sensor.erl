-module(sensor).
-compile(export_all).
-author("Christina Li and Brenden Brusberg").

sensor(ID, Watcher) ->
    Measurement = rand:uniform(11),
    if(Measurement == 11) ->
	    exit(anomalous_reading);
      true ->
	    Watcher!{self(),{ID, Measurement}}
    end,
    Sleep_time = rand:uniform(10),
    timer:sleep(Sleep_time),
    sensor(ID,Watcher).

