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
        bookBorrowRequest -> spawn(fun() -> handleBookBorrowRequest(HandlerData,RequestData) end);
        %here go all handler creations
        (_) -> io:format("no existing handler for request type: ~p~n ",[RequestType])
    end.

%handles connection for echo
-spec echoHandle(handler_data(),reference()) -> ok.
echoHandle(#handler_data{clientID =Client,request_reference = Ref} = HandlerData,_) ->
    Client!{echoReply,noData,Ref,self()},
    dieHandler(HandlerData).

%handles connection for borrowing
-spec handleBookBorrowRequest(handler_data(),{core_book:book_id(),lib_user:user_card_id()}) -> ok.
handleBookBorrowRequest(HandlerData,{BookID,UserID}) ->
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    Server = HandlerData#handler_data.coreID,
    case catch(gen_server:call(Server,{dbQuery,{getBookByID,BookID}},?ServerWaitTime)) of
        {dbReply, none} ->
            Client!{canNotBorrow,lib_server_reasons:noBook(),Ref,self()};
        {dbReply,Book} ->
            case catch(gen_server:call(Server,{dbQuery,{getUserByID,UserID}},?ServerWaitTime)) of
                {dbReply, none} ->
                    Client!{canNotBorrow,lib_server_reasons:noUser(),Ref,self()};
                {dbReply,User} ->
                    CanBorrow = User#lib_user.can_borrow,
                    Username = User#lib_user.name,
                    Title = Book#book.book_info#book_info.title,
                    if 
                        CanBorrow ->
                            Client!{canBorrow,{Username,Book},Ref,self()};
                        true -> 
                            Client!{canNotBorrow,{Username,Book,lib_server_reasons:userCantBorrow()},Ref,self()}
                    end;
                _ -> serviceUnavilableResponse(HandlerData)
            end;
        
        _ -> serviceUnavilableResponse(HandlerData)
    end,
    dieHandler(HandlerData).

-spec confirmBorrowingBook(handler_data(),{core_book:book(),lib_user()}) -> ok.
confirmBorrowingBook(HandlerData,{UserID,BookID}) ->
    Server = HandlerData#handler_data.coreID,
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    receive
        {borrowingConfirmation,noData,Ref,Client} ->
            case catch(gen_server:call(Server,{dbUpdate,{borrowBook,{UserID,BookID}}},?ServerWaitTime)) of
                {dbReply,{updateOk,noData}} ->
                    Client!{bookBorrowed,noData,Ref,self()};
                {dbReply,{canNotUpdate,Reason}} ->
                    Client!{couldNotBorrow,Reason,ref,self()};
                _ ->
                    serviceUnavilableResponse(HandlerData)
            end;
        {borrowiingCancelled,noData,Ref,Client} -> ok
    after 
        ?ClientWaitTime ->
            serviceTimeoutResponse(HandlerData)
    end.

%handles connection for returning book
-spec handleBookReturnRequest(handler_data(),{core_book:book_id(),lib_user:user_card_id()}) -> ok.
handleBookReturnRequest(HandlerData,{UserID,BookID}) ->
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    Server = HandlerData#handler_data.coreID,
    case catch(gen_server:call(Server,{dbQuery,{getBookByID,BookID}},?ServerWaitTime)) of
        {dbReply, none} ->
            Client!{canNotReturn,lib_server_reasons:noBook(),Ref,self()};
        {dbReply,Book} ->
            case catch(gen_server:call(Server,{dbQuery,{getUserByID,UserID}},?ServerWaitTime)) of
                {dbReply, none} ->
                    Client!{canNotBorrow,lib_server_reasons:noUser(),Ref,self()};
                {dbReply,User} ->
                    Username = User#lib_user.name,
                    Title = Book#book.book_info#book_info.title,
                        case catch(gen_server:call(Server,{dbUpdate,{returnBook,{UserID,BookID}}},?ServerWaitTime)) of
                            {dbReply,{updateOk,_}} ->
                                Client!{bookReturned,{Username,Title},Ref,self()};
                            {dbReply,{punishment,PunishmentValue}} ->
                                Client!{bookReturnedAndPunishment,{Username,Title,PunishmentValue},Ref,self()};
                            {dbReply,{canNotUpdate,Reason}} ->
                                Client!{cantReturnBook,{Username,Title}};
                            _ ->
                                serviceUnavilableResponse(HandlerData)
                        end;
                _ -> 
                    serviceUnavilableResponse(HandlerData)
            end;
        _ ->
            serviceUnavilableResponse(HandlerData)
    end,
    dieHandler(HandlerData).

%handles connection for extending book

handleBookExtendRequest(HandlerData,{UserID,BookID}) ->
    Ref = HandlerData#handler_data.request_reference,
    Client = HandlerData#handler_data.clientID,
    Server = HandlerData#handler_data.coreID,
    case catch(gen_server:call(Server,{dbUpdate,{extendBook,{UserID,BookID}}},?ServerWaitTime)) of
        {dbReply,{updateOk,Book}} ->
            Client!{bookExtended,Book,Ref,self()};
        {dbReply,{canNotUpdate,Reason}} ->
            Client!{cantExtedBook,Reason,Ref,self()};
        _ ->
            serviceUnavilableResponse(HandlerData)
    end,
    dieHandler(HandlerData).

%special handler responses
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
