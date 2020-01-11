-module(book_tests).
-include_lib("eunit/include/eunit.hrl").

borrow_is_successful_when_no_checkouts_before_for_given_book_and_there_are_no_restrictions_on_student_test() ->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    {BookId,_,_} = Book,
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    {Result, BookResult} = book:borrow(StudentA,CanBorrow,Now,Book),
    
    ?assertEqual(ok,Result),
    ?assertEqual({BookId, BookInfo, [{{2020,1,1},{2020,3,31}, StudentA, false,{}}]}, BookResult).

borrow_is_unsuccessful_when_there_are_restrictions_on_student_test() ->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    CanBorrow = fun (_Student) -> false end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    
    Result = book:borrow(StudentA,CanBorrow,Now,Book),

    ?assertEqual(cannot_borrow, Result).

borrow_is_unsuccessful_when_you_want_to_borrow_not_returned_book_test() ->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    {ok,Book2} = book:borrow(StudentA,CanBorrow,Now,Book),

    Result = book:borrow(StudentA,CanBorrow,Now,Book2),

    ?assertEqual(already_borrowed, Result).

borrow_is_successful_when_book_has_been_already_returned_test() ->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    {BookId,_,_} = Book,
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    {ok,Book2} = book:borrow(StudentA,CanBorrow,Now,Book),
    {ok,Book3} = book:return(StudentA,Now,Book2),
    
    {Result,BookResult} = book:borrow(StudentA, CanBorrow, Now, Book3),
    
    ?assertEqual(ok, Result),
    ?assertEqual({BookId, BookInfo, [{{2020,1,1},{2020,3,31}, StudentA, false,{}},{{2020,1,1},{2020,3,31}, StudentA, true,{2020,1,1}}]},BookResult).
