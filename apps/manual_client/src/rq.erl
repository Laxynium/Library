-module(rq).
-compile([export_all]).

getAllBooks() -> 
    gen_server:cast(global:whereis_name(library_server_man),{query,all_books,self(),nodata}),
    receive_cast(2000).

getSomeUser() ->
    [User|_Rest] = getAllBooks(),
    User.
getAllClients() -> 
    gen_server:cast(global:whereis_name(library_server_man),{query,all_clients,self(),nodata}),
    receive_cast(2000).
addBook(Title,Author,Version) ->
    gen_server:cast(global:whereis_name(library_server_man),{command,add_book,self(),{Title,Author,Version}}),
    receive_cast(2000).
addUser(Name,ID) ->
    gen_server:cast(global:whereis_name(library_server_man),{command,add_client,self(),{ID,Name}}),
    receive_cast(2000).
query(Data) ->
    gen_server:cast(global:whereis_name(library_server_man),Data),
    receive_cast(2000).

loadExampleBooks() ->
    addBook("Lord of the Rings","J.R.R. Tolkien",1),
    addBook("Harry Potter i Czara Ognia","J.K. Rowling",1),
    addBook("Wproadzenie do algotytmów","Thomas H. Cormen",2),
    addBook("Introduction to Automata Theory","John E. Hopcroft",3),
    addBook("example_book_1","example_author_1",1),
    ok.
loadExampleUsers() ->
    addUser("Jan Kowalski","12345"),
    addUser("Katarzyna Nowak","11111"),
    addUser("Michał Góral","22222"),
    ok.

receive_cast(Timeout) ->
    receive
        {'$gen_cast',Data} -> Data;
        Other -> Other
    after
        Timeout -> timeout
    end.



