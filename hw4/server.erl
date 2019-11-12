-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
		io:format("~n~nNewState:~n~p~n~n", [NewState]),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
%% ChatName, ClientPID, Ref, State original arguments but I changed them around based on my client call
do_join(State, Ref, ClientPID, ChatName) ->
    ChatRms = maps:keys(State#serv_st.chatrooms),
	case lists:member(ChatName, ChatRms) of
		true -> %%if the ChatName already exists
			ExistChatRm = maps:get(ChatName, State#serv_st.chatrooms),
			UpdateChatRms = ExistChatRm,
			UpdateRegistration = maps:update(),
		false -> %% if the ChatName does not exist
			NewChatRm = spawn(chatroom, start_chatroom, [ChatName]),
			UpdateChatRms = maps:put(ChatName, NewChatRm, State#serv_st.chatrooms), %updates chatrooms
			UpdateRegistration = maps:put(ChatName, [ClientPID], Registrations) %puts the client into registrations
	end,

	%send message to the chatroom to let the client join the chatroom
	ClientPID!{self(), Ref,register, ClientPID, ClientNick},

	#serv_st{
		nicks = State#serv_st.nicks,
		registrations = UpdateRegistration, %change
		chatrooms = UpdateChatRms
	}

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    io:format("server:do_leave(...): IMPLEMENT ME~n"),
    State.

%% -record(serv_st, {nicks, registrations, chatrooms}).
%%registrations: a map from a chatroom’s name (string) as the key to a list of the client processes’ PIDs of clients registered in that chatroom.
%%chatrooms: a map from a chatroom’s name (string) as the key to the chatroom’s corresponding PID as the value.
%% executes new nickname protocol from server perspective
%% breaking when trying to change the nickname the second time
do_new_nick(State, Ref, ClientPID, NewNick) ->
	Nicknames = maps:values(State#serv_st.nicks),
	io:format("Nicknames: ~p~nNewNick: ~p~n",[Nicknames, NewNick]),
	case lists:member(NewNick, Nicknames) of
		true ->
			io:format("Nickname: ~p~n", [NewNick]),
			ClientPID!{self(), Ref, err_nick_used},
			State;
		false ->
			% might need for other chatrooms
			%ChatRm = maps:filter(fun(_Nm, Clients) -> lists:member(ClientPID, Clients) end, State#serv_st.registrations),
			%ChatRmPID = maps:filter(fun(Nm, _PID) -> list: member(Nm, maps:keys(ChatRm)) end, ChatRm),
			%NewNicks = maps:map(fun(_Nm, PID) -> PID!{self(), Ref, update_nick, ClientPID, NewNick} end, ChatRmPID),
			NewNicks = maps:update(ClientPID, NewNick, State#serv_st.nicks),
			ClientPID!{self(), Ref, ok_nick},
			io:format("NewNicks = ~p~n", [NewNicks]),
			#serv_st{
				nicks = NewNicks, 
				registrations = State#serv_st.registrations,
				chatrooms = State#serv_st.chatrooms
			}
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
    State.
