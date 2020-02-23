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
handle(State, Data) -> 
    io:fwrite("~p~n~p~n", [State, Data]),
    {reply, ok, State}.

    
