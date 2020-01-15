-module(lib_server_db).
-include_lib("core/src/core_book.hrl").
-include_lib("core/src/core_lib_user.hrl").

-type book_db() :: [book:book()].
-type user_db() :: [core_lib_user:lib_user()].

-record(db_data,{users ::user_db(),books::book_db()}).
-type db_data() :: #db_data{}.

-record(db_query_request,{type::atom(),data::any()}).
-type db_query_request() :: #db_query_request{}.

-export([handleDBquery/2,loadDBFromFile/1]).


-spec loadDBFromFile(string()) -> ok.
loadDBFromFile(Path) ->
    ok.


-spec handleDBquery(db_query_request(),db_data()) -> any().
handleDBquery(#db_query_request{type = QueryType,data = QueryData},DB) ->
    case QueryType of 
        getUserByID -> queryGetUserByID(QueryData,DB);
        getBookByID -> queryGetBookByID(QueryData,DB);

        %% here go all query handles
        _ -> queryError
    end.


-spec handleDBupdate({atom(),any()},db_data()) -> {{atom(),any()},db_data()}
handleDBupdate({UpdateType,Data},DB) ->
    case UpdateType of 
        borrowBook -> updateBorrowBook(Data,DB);
        returnBook -> updateReturnBook(Data,DB);
        _ -> {{updateError,badRequest},DB}
    end.


-spec queryGetUserByID(core_lib_user:user_card_id(),db_data()) -> {ok,core_lib_user:lib_user()} | none.
queryGetUserByID(CardID,#db_data{users = Users})->
    libutil:firstMatch(fun(#lib_user{id=ID}) -> ID == CardID end, Users).


-spec queryGetBookByID(core_book:book_id(),db_data()) -> {ok,core_book:book()} | none.
queryGetBookByID(BookID,#db_data{books = Books}) ->
    libutil:firstMatch(fun(#book{id=ID}) -> ID == BookID end,Books).


-spec updateBorrowBook({user_card_id(),book_id()},db_data()) -> {{updateOk,any()},db_data()} | ({canNotUpdate,any()},db_data().
updateBorrowBook({UserID,BookID}, DB) ->
    Books = DB#db_data.books,
    case queryGetBookByID(BookID,DB) of
    {ok,Book} -> case core_book:borrow(UserID,fun(X) -> X#lib_user.can_borrow end,fun() ->calendar:universal_time() end,Book) of
        {ok, NewBook} -> 
            NewBooks = [NewBook| libutil:deleteFirstMatch(fun(X) -> X#book.id == CardID end,DB#db_data.books)],
            {{updateOk,noData},DB#db_data{books = NewBooks}};
        cannot_borrow -> {{canNotUpdate,lib_server_reasons:userCantBorrow()},DB};
        already_borrowed -> {{canNotUpdate,lib_server_reasons:alreadyBorrowed()},DB}
        end;
    none -> {{canNotUpdate,lib_server_reasons:noBook()},DB}
end.
    

-spec updateReturnBook({user_card_id(),book_id()},db_data()) -> {{updateOk,any()},db_data()} | ({canNotUpdate,any()},db_data().
updateReturnBook({UserID,BookID}, DB) ->
    Books =DB#db_data.books,
    case queryGetBookByID(BookID,DB) of
        {ok,Book} -> case core_book:return(UserID,fun() -> calendar:universal_time() end, Book ) of
            {ok, NewBook} ->
                NewBooks = [NewBook| libutil:deleteFirstMatch(fun(X) -> X#book.id == CardID end,DB#db_data.books)],
                {{updateOk,noData},DB#db_data{books = NewBooks};
            {punishment,Value,NewBook} -> {{punishment,Value},DB#db_data{books = NewBooks}};
            book_not_borrowed -> {{canNotUpdate,lib_server_reasons:notBorrowedByUser()},DB}
        end;
    none -> {{canNotUpdate,lib_server_reasons:noBook()},DB}
end.
    










