-module(lib_server_handler_spawn).

-include_lib("core/src/core_book.hrl").
-include_lib("core/src/core_lib_user.hrl").

-define(ClientWaitTime,60000).
-define(ServerWaitTime,500).

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
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    Server = HandlerData#handler_data.coreID,
    case catch(gen_server:call(Server,{dbQuery,{getBookByID,UserID}},?ServerWaitTime)) of
        {dbReply,Book=#book{book_info=BookInfo#book_info{title=Title}}} ->
            case catch(gen_server:call(Server,{dbQuery,{getUserByID,UserID}},?ServerWaitTime)) of
                {dbReply,User#lib_user{name=Username, can_rent=CanRent}} ->
                    if 
                        CanRent ->
                            Client!{CanRent,{Username,Book},Ref,self()};
                        true -> 
                            Client!{canNotRent,{Username,Book},Ref,self()}
                    end;
                _ -> serviceUnavilableResponse(HandlerData)
            end;
        _ -> serviceUnavilableResponse(HandlerData)
    end,
    dieHandler(HandlerData).

confirmRentingBook(HandlerData) ->
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    receive
        {rentingConfirmation,noData,Ref,Client} ->
            
    after 
        ?ClientWaitTime -> serviceTimeoutResponse(HandlerData)
    end.


-spec dieHandler(handler_data()) -> ok.
dieHandler(#handler_data{coreID = Server}) ->
    gen_server:cast(coreID,{handlerDone,self()}),
    ok.

-spec serviceUnavilableResponse(handler_data()) -> ok.
serviceUnavilableResponse(HandlerData) ->
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    Client!{serviceUnavilable,noData,Ref,self()},
    ok.

-spec serviceTimeoutResponse(handler_data()) -> ok.
serviceTimeoutResponse(HandlerData) ->
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    Client!{serviceTimeout,noData,Ref,self()},
    ok.
