-module(server).
-export([start/1,stop/1]).

-record(server_st, {
    channels = [],
    nicks = []
}).


% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    genserver:start(ServerAtom, #server_st{}, fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, stop).

%Handle join request
handle(State = #server_st{channels = ExistingChannels, nicks = ExistingNicks}, {join, ChannelToJoin, NickToJoin, Sender}) -> 
    ChannelExists = lists:member(ChannelToJoin, ExistingChannels),
    if ChannelExists -> 
        Result = (catch(genserver:request(list_to_atom(ChannelToJoin), {join, Sender}))),
        case Result of
            error -> {reply, error, State};
            join -> {reply, join, State}
                %NickExists = lists:member(NickToJoin, ExistingNicks),
                %if NickExists -> 
                %    {reply, join, State};
                %true -> 
                %    {reply, join, State#server_st{nicks = [NickToJoin | ExistingNicks]}}
                %end
            end;
    true -> 
        channel:start(ChannelToJoin, Sender),
        {reply, join, State#server_st{channels = [ChannelToJoin | ExistingChannels]}}
    end;

%Handle leave request
handle(State = #server_st{channels = ExistingChannels}, {leave, ChannelToLeave, PidToLeave}) -> 
    ChannelExists = lists:member(ChannelToLeave, ExistingChannels),
    if ChannelExists ->
        Result = (catch (genserver:request(list_to_atom(ChannelToLeave), {leave, PidToLeave}))),
        case Result of 
            error -> {reply, leave, State};
            leave -> {reply, leave, State}
        end;
    true -> 
        {error, does_not_exists, "The channel does not exist"}
    end;

% Renick
handle(State = #server_st{nicks = ExistingNicks}, {nick, Nick}) ->
    NickExists = lists:member(Nick, ExistingNicks),
    if NickExists -> 
        {reply, error, State};
    true -> 
        NewState = #server_st {nicks = [Nick | ExistingNicks]}
    end;

% Catch All >:(
handle(_, _) -> 
    {error, not_implemented, "The function does not exist"}.