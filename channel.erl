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
handle(State = #channel_st{connected_users = Users}, {join, NewUser}) -> 
    IsAlreadyInChannel = lists:member(NewUser, Users),
    if IsAlreadyInChannel -> 
        {reply, error, State};
    true -> 
        NewState = [NewUser | Users],
        {reply, join, #channel_st{connected_users = [NewUser | Users]}}
    end;

% LEAVE 
handle(State = #channel_st{connected_users = Users}, {leave, LeavingUser}) -> 
    IsInChannel = lists:member(LeavingUser, Users),
    if IsInChannel -> 
        ResultingList = lists:delete(LeavingUser, Users),
        {reply, leave, #channel_st{connected_users = ResultingList}};
    true -> 
        {reply, error, State}
    end;

% MESSAGES
handle(State = #channel_st{connected_users = Users}, {message_send, Channel, Sender, Msg}) ->
    
   SenderInChannel = lists:member(Sender, Users),
   %io:put_chars("Hello from the other sideeeeeeee"),
   if SenderInChannel ->
       Succeded = broadcast(Users ,Msg, Sender),
       if Succeded -> 
           %io:fwrite("~n", "HelloSucc"),
           {reply, message_send, State};
        true -> 
            %io:fwrite("~n", "HelloFuck"),
            {reply, error, State}
        end;
   true ->
       {reply, error, "U no in channel ecksde"}
   end; 
        
% None of the above
handle(State, _) -> {reply, error, State}.

broadcast([User | Users], Msg, Sender) ->
    io:put_chars("lmaooo"),
    spawn(fun() -> genserver:request(User, {message_receive, #channel_st.channel_name, Sender, Msg}) end ),
    broadcast(Users, Msg, Sender);

broadcast([], Msg, Sender) ->
    io:put_chars("not so lmaooo"),
    true. 