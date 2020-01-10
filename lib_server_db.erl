-module(lib_server_db).
-record(book,{rfID,title,rented,rent_until}).
-record(db_data,{activeCardIDs = [],books =[]}).

-export([handleDBquery/2,loadDBFromFile/1]).


loadDBFromFile(Path) ->
    %% TODO
    ok.
handleDBquery({QueryType,Data},Db_data) ->
    case QueryType of 
        isCardIDActive -> queryIsCardIDActive(Data);
        %% here go all query handles
        _ -> queryError
    end.

handleDBupdate({UpdateType,Data},Db_data) ->
    {ok,Db_data}.


queryIsCardIDActive(CardId) ->
    %TODO
    false.