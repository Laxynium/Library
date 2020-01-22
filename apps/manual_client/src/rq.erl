-module(rq).
-compile([export_all]).

getAllBooks() -> 
    gen_server:cast(global:whereis_name(library_server_man),{query,all_books,self(),nodata}),
    receive_cast(2000).
getAllClients() -> 
    gen_server:cast(global:whereis_name(library_server_man),{query,all_clients,self(),nodata}),
    receive_cast(2000).
addBook(Title,Author,Version) ->
    gen_server:cast(global:whereis_name(library_server_man),{command,add_book,self(),{Title,Author,Version}}),
    receive_cast(2000).
query(Data) ->
    gen_server:cast(global:whereis_name(library_server_man),Data),
    receive_cast(2000).


receive_cast(Timeout) ->
    receive
        {'$gen_cast',Data} -> Data;
        Other -> Other
    after
        Timeout -> timeout
    end.



