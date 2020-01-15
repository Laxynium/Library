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

handleDBupdate({UpdateType,Data},DB) ->
    {ok,DB}.

-spec queryGetUserByID(core_lib_user:user_card_id(),db_data()) -> {ok,core_lib_user:lib_user()} | none.
queryGetUserByID(CardID,#db_data{users = Users})->
    libutil:firstMatch(fun(#lib_user{id=ID}) -> ID == CardID end, Users).

-spec queryGetBookByID(core_book:book_id(),db_data()) -> {ok,core_book:book()} | none.
queryGetBookByID(BookID,#db_data{books = Books}) ->
    libutil:firstMatch(fun(#book{id=ID}) -> ID == BookID end,Books).

updateBorrowBook(BookID,CardID, DB) ->
    Books = DB#db_data.books,
    {ok,Book} = queryGetBookByID(BookID,DB),
    case core_book:borrow(CardID,fun(X) -> core_lib_user:getCanRent(X) end,fun() ->calendar:universal_time() end,Book) of
        {ok, NewBook} -> 
            NewBooks = [NewBook| libutil:deleteFirstMatch(fun(X) -> )]

    NewBooks = 

    ok.









