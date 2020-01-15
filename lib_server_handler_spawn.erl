-module(lib_server_handler_spawn).
-define(ResponseWaitTime,400).

-record(handler_data,{coreID::pid(),clientID::pid(),request_reference::reference()}).
-type handler_data() :: #handler_data{}.
-record(client_request_data,{request_type::atom(),ref::reference(),data::any()}).
-type client_request_data() :: #client_request_data{}.



-spec createHandler(atom(),pid(),reference(),any()) -> pid().
createHandler(RequestType,RequestData,From,Ref) ->
    HandlerData = #handler_data{coreID = self(),clientID = From, request_reference = Ref},
    case RequestType of 
        echoRequest -> spawn(fun() -> echoHandle(HandlerData,RequestData) end);
        bookRentRequest -> spawn(fun() -> handleBookRentRequest(HandlerData,RequestData) end);
        %here go all handler creations
        (_) -> io:format("no existing handler for request type: ~p~n ",[RequestType])
    end.

%handles connection for echo
-spec echoHandle(handler_data(),reference()) -> ok.
echoHandle(#handler_data{clientID =Client,request_reference = Ref} = HandlerData,_) ->
    Client!{echoReply,noData,Ref,self()},
    dieHandler(HandlerData).

%handles connection for renting
-spec handleBookRentRequest(handler_data(),{core_book:book_id(),lib_user:user_card_id()}) -> ok.
handleBookRentRequest(HandlerData,{BookID,UserID}) ->

    try gen_server:call(Server,{dbQuery,{canUserRent,UserID}}) of
        {dbReply,true} ->
            User = 
            Client!{canRent,noData,}
    catch
        Throw -> 
        

    
    end,
    
    dieHandler(HandlerData).

-spec dieHandler(handler_data()) -> ok.
dieHandler(#handler_data{coreID = Server}) ->
    gen_server:cast(coreID,{handlerDone,self()}),
    ok.


