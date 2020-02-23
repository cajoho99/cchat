-module(channel).
-export([start/2]).

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
% None of the above
handle(State, _) -> {reply, error, State}.

