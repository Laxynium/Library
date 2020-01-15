-module(lib_server_db).

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

handleDBupdate({UpdateType,Data},Db_data) ->
    {ok,Db_data}.

-spec queryGetUserByID(core_lib_user:user_card_id(),db_data()) -> {ok,core_lib_user:lib_user()} | none.
queryGetUserByID(CardID,#db_data{users = Users})->
    libutil:firstMatch(fun(User) -> ID = core_lib_user:getID(User),
                        ID == CardID end,Users).

-spec queryGetBookByID(core_book:book_id(),db_data()) -> {ok,core_book:book()} | none.
queryGetBookByID(BookID,#db_data{books = Books}) ->
    libutil:firstMatch(fun(Book) -> ID = core_book:getID(Book),
                        ID == BookID end,Books).



