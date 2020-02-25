-module(channel).
-export([start/2, handle/2]).

-record(channel_st,
{
    channel_name,
    connected_users
}).

start(ChannelName, User) -> 
    genserver:start(list_to_atom(ChannelName), #channel_st{ channel_name = ChannelName, connected_users=[User]}, fun handle/2).

% JOIN
handle(State = #channel_st{connected_users = Users, channel_name = ChannelName}, {join, NewUser}) -> 
    ServerExists = lists:member(list_to_atom(ChannelName), registered()),
    if ServerExists ->
        IsAlreadyInChannel = lists:member(NewUser, Users),
        if IsAlreadyInChannel -> 
            {reply, {error,user_already_joined, "User already in chat"}, State};
        true -> 
            {reply, join, #channel_st{connected_users = [NewUser | Users]}}
        end;
    true ->
        {reply, {error, server_not_reached, "COOL MESSAGE HELP"}, State}
    end;

% LEAVE 
handle(State = #channel_st{connected_users = Users}, {leave, LeavingUser}) -> 
    IsInChannel = lists:member(LeavingUser, Users),
    if IsInChannel -> 
        ResultingList = lists:delete(LeavingUser, Users),
        {reply, leave, #channel_st{connected_users = ResultingList}};
    true -> 
        {reply, {error, user_not_joined, "User did not join"}, State}
    end;


handle(State = #channel_st{connected_users = Users}, {message_send, Channel, SenderNick, SenderPid, Msg}) ->
    case lists:member(SenderPid,Users) of
        % Spawns a process for each client in the channel that sends a
        % message_receive to genserver for each client.
        true -> spawn(fun()->lists:foreach(
        fun(Pid) ->
            % No need to message yourself
            if Pid == SenderPid -> skip;
            true -> genserver:request(Pid, {message_receive, Channel, SenderNick, Msg})
            end
        end,
        Users) end),
        {reply, ok, State};
        false -> {reply, failed, State}
    end;


% MESSAGES
%handle(State = #channel_st{connected_users = Users}, {message_send, Channel, Sender, Msg}) ->
    
  % SenderInChannel = lists:member(Sender, Users),
   %io:put_chars("Hello from the other sideeeeeeee"),
 %  if SenderInChannel ->
  %      io:fwrite("~p~n", [Users]),
  %      Succeded = spawn(fun() -> broadcast(Users ,Msg, Sender) end ),
        %if Succeded -> 
     %       {reply, message_send, State};
         %true -> 
         %   {reply, error, State}
        %end;
 %  true ->
 %      {reply, error, "U no in channel ecksde"}
 %  end; 
        
% None of the above
handle(State, _) -> {reply, error, State}.

broadcast([User | Users], Msg, Sender) ->
    io:fwrite("~p~n~p~n", [User, Users]),
    genserver:request(User, {message_receive, #channel_st.channel_name, "Sender", Msg}),
    broadcast(Users, Msg, Sender);

broadcast([], _, _) ->
    true. 