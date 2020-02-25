-module(channel).
-export([start/2, handle/2]).


% This record maintains the state of a channel, including 
% the name of the channel and the clients who are currently 
% connected to it.
-record(channel_st,
{
    channel_name,
    connected_users
}).

% Starts the channel server instance.
start(ChannelName, User) -> 
    genserver:start(list_to_atom(ChannelName), #channel_st{ channel_name = ChannelName, connected_users=[User]}, fun handle/2).

% ---------------- JOIN -----------------------
% Manages join requests to the channel. 
% It checks if the user is already in the channel.
handle(State = #channel_st{connected_users = Users, channel_name = ChannelName}, {join, NewUser}) -> 
    IsAlreadyInChannel = lists:member(NewUser, Users),
    if IsAlreadyInChannel -> 
        {reply, {error, user_already_joined, "User already in chat"}, State};
    true -> 
        {reply, join, #channel_st{connected_users = [NewUser | Users]}}
    end;


% ---------------- LEAVE -----------------------
% Manages leave requests to the channel.
% Checks if the user trying to leave is in the channel or not.
handle(State = #channel_st{connected_users = Users}, {leave, LeavingUser}) -> 
    IsInChannel = lists:member(LeavingUser, Users),
    if IsInChannel -> 
        ResultingList = lists:delete(LeavingUser, Users),
        {reply, leave, #channel_st{connected_users = ResultingList}};
    true -> 
        {reply, {error, user_not_joined, "User did not join"}, State}
    end;

% --------------- MESSAGES ---------------------
% Handles distributing messages to all connected clients.
% Spawns a new process that sends requests to all connected 
% clients with the sent message.
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
        false -> {reply, {error, user_not_joined, "User is not in channel"}, State}
    end;
        
% ----------------- CATCH ALL >:( ------------------
% Catches all remaining requests and returns an error.
handle(State, _) -> {reply, error, State}.