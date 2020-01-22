-module(rq).
-compile([export_all]).

getAllBooks() -> 
    gen_server:cast(global:whereis_name(library_server_man),{query,all_books,self(),nodata}),
    receive
        Response -> Response
    after 
        2000 -> timeout
    end.
getAllClients() -> 
    gen_server:cast(library_server_man,{query,all_clients,self(),nodata}),
    receive
        Response -> Response
    after 
        2000 -> timeout
    end.



