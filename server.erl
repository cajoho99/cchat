-module(server).
-export([start/1,stop/1]).

% Server record that holds the channels and the nicks
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
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).

% Handle join request
% Sees if the channel that the client attempts to join exits, if it does it joins that channel but also checks the nick of the client
% If the channel does not exist it creates the channel and then joins the client to it
handle(State = #server_st{channels = ExistingChannels, nicks = ExistingNicks}, {join, ChannelToJoin, NickToJoin, Sender}) -> 
    ChannelExists = lists:member(ChannelToJoin, ExistingChannels),
    if ChannelExists -> 
        Result = (catch(genserver:request(list_to_atom(ChannelToJoin), {join, Sender}))),
        case Result of
            {error, user_already_joined,_} -> {reply, Result, State};
            {'EXIT', _} -> {reply, {error, channel_does_not_respond, "Channel does not respond"}};
            join ->   
                NickExists = lists:member(NickToJoin, ExistingNicks),
                if NickExists -> 
                    {reply, join, State};
                true -> 
                    {reply, join, State#server_st{nicks = [NickToJoin | ExistingNicks]}}
                end
            end;
    true -> 
        channel:start(ChannelToJoin, Sender),
        {reply, join, State#server_st{channels = [ChannelToJoin | ExistingChannels]}}
    end;

% Handle leave request
% It checks if the channel exists and if it does it checks if the client is in the channel
% If the channel doesn't exist or the client was not in the channel it errors out
handle(State = #server_st{channels = ExistingChannels}, {leave, ChannelToLeave, PidToLeave}) -> 
    ChannelExists = lists:member(ChannelToLeave, ExistingChannels),
    if ChannelExists ->
        Result = (catch (genserver:request(list_to_atom(ChannelToLeave), {leave, PidToLeave}))),
        case Result of 
            {error, user_not_joined, _} -> {reply, Result, State};
            leave -> {reply, leave, State};
            {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not respond"}, State}
        end;
    true -> 
        {error, does_not_exists, "The channel does not exist"}
    end;

% Re-nick
% Looks to see if the nick is taken, if it is then it errors out, otherwise it returns a updated state
handle(State = #server_st{nicks = ExistingNicks}, {nick, OldNick, NewNick}) ->
    NickExists = lists:member(NewNick, ExistingNicks),
    if NickExists -> 
        {reply, error, State};
    true -> 
        NewState = #server_st {nicks = [NewNick | lists:delete(OldNick, ExistingNicks)]},
        {reply, nick, NewState}
    end;

% Stops all channels
% Goes through all known channel and stops them one by one
handle(State, stop_channels) ->
  % Iterates through all channels registered to a server and stops them
  lists:foreach(fun(Channel) -> genserver:stop(list_to_atom(Channel)) end, State#server_st.channels),
  {reply,ok,State};

% Catch All >:(
handle(State, Data) ->
    {error, instruction_not_found, ["The instruction does not exist", State, Data]}.