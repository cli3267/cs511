-module(b).
-compile(export_all).

barrier(0, N, L) ->
    [From!{self(), ok} || From <- L],
    barrier(N,N,[]);

barrier(M, N, L) when M > 0 ->
    receive
        {From, reached} ->
            barrier(M-1, N, [From | L])
    end.

pass_barrier(B) ->
    B!{self(), reached},
    receive
        {B, ok} ->
            ok
    end.

client(B, Letter, Number) ->
    io:format("~p ~s ~n", [self(), Letter]),
    pass_barrier(B),
    io:format("~p ~w ~n", [self(), Number]).

start(N) ->
    B = spawn(?MODULE, barrier, [N, N, []]),
    spawn(?MODULE, client, [B,"a", 1]),
    spawn(?MODULE, client, [B,"b", 2]),
    spawn(?MODULE, client, [B,"c", 3]).

%%% Producer/Consumers
buffer(Size, Producers, Consumers, Capacity) ->
    receive
        {From, startProduce} when Size+Producers < Capacity ->
            {self(), ok},
            buffer(Size, Producers+1, Consumers, Capacity);
         {From, startProduce} when Size-Producers < 0 ->
            {self(), ok},
            buffer(Size, Producers+1, Consumers, Capacity);
    end.

producer(S) ->
    S!{self(), startProduce},
    io:format("Producer ~p: startProducing~n", [self()]),
    receive
        {S, ok} ->            
            io:format("Producer ~p: produced~n", [self()]),
            produce,
            S!{self(), endProduce}
    end.

consumer(S) ->
    S!{self(), startConsume},
    io:format("Consumer ~p: startConsume~n", [self()]),
    receive
        {S, ok} ->
            io:format("Consumer ~p: consumed~n", [self()]),
            consume,
            S!{self(), endConsume}
    end.

start(NP, NC, Size) ->
    B = spawn(?MODULE, buffer, [0, Size]),
    [ producer(B) || _ <- lists:seq(1, NP)],
    [ consumer(B) || _ <- lists: seq(1, NC)].