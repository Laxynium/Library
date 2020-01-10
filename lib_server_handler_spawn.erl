-module(lib_server_handler_spawn).

-export([createHandler/3]).

createHandler(RequestType,From,Data) ->
    case RequestType of 
        echoRequest -> spawn(fun() -> echoHandle(self(),From,Data) end);
        %here go all handler creations
        (_) -> io:format("no existing handler for request type: ~p~n ",[RequestType])
    end,
    ok.

echoHandle(CoreID,From,Reference) ->
    From!{echoReply,Reference}.