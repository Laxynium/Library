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

borrowed_book_by_student_X_cannot_be_returned_by_studnet_Y_test() ->
    StudentA = {"300111"},
    StudentB = {"300112"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    {ok,Book2} = book:borrow(StudentA, CanBorrow, Now, Book),
    
    Result = book:return(StudentB, Now, Book2),
    
    ?assertEqual(book_not_borrowed, Result).

not_borrowed_book_once_cannot_be_returned_test()->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    Now = fun () -> {{2020,1,1},{12,0,0}} end,    

    Result = book:return(StudentA, Now, Book),

    ?assertEqual(book_not_borrowed, Result).

not_borrowed_book_cannot_be_returned_test()->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    {ok, Book2} = book:borrow(StudentA, CanBorrow, Now, Book),
    {ok, Book3} = book:return(StudentA, Now, Book2),

    Result = book:return(StudentA, Now, Book3),

    ?assertEqual(book_not_borrowed, Result).

punishment_is_calculated_when_book_is_returned_after_the_till_date_test()->
    %Punishment formula is 0.2zl *  number_of_days_after
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    {BookId,_,_} = Book,
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    After = fun () -> {{2020,4,5},{12,0,0}} end,
    {ok, Book2} = book:borrow(StudentA, CanBorrow, Now, Book),

    Result = book:return(StudentA, After, Book2),

    ExpectedBook = {BookId, BookInfo, [{{2020,1,1},{2020,3,31}, StudentA, true,{2020,4,5}}]},
    
    ?assertEqual({punishment, 0.2* 5, ExpectedBook}, Result).

punishment_is_not_calculated_when_book_is_returned_on_time_test()->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    {BookId,_,_} = Book,
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    After = fun () -> {{2020,3,31},{12,0,0}} end,
    {ok, Book2} = book:borrow(StudentA, CanBorrow, Now, Book),

    Result = book:return(StudentA, After, Book2),

    ExpectedBook = {BookId, BookInfo, [{{2020,1,1},{2020,3,31}, StudentA, true,{2020,3,31}}]},
    
    ?assertEqual({ok, ExpectedBook}, Result).

not_borrowed_book_one_cannot_be_extended_test()->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    Later = fun () -> {{2020,3,15},{12,0,0}} end,

    Result = book:extend(StudentA, Later, Book),
    
    ?assertEqual(book_not_borrowed, Result).

not_borrowed_book_cannot_be_extended_test()->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    CanBorrow = fun (_Student) -> true end,    
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    Later = fun () -> {{2020,3,15},{12,0,0}} end,

    {ok, Book2} = book:borrow(StudentA, CanBorrow, Now, Book),
    {ok, Book3} = book:return(StudentA, Now,Book2),
    
    Result = book:extend(StudentA, Later, Book3),
    
    ?assertEqual(book_not_borrowed, Result).

book_borrowed_by_student_X_cannot_be_extended_by_student_Y_test()->
    StudentA = {"300111"},
    StudentB = {"300112"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    Later = fun () -> {{2020,3,15},{12,0,0}} end,
    {ok,Book2} = book:borrow(StudentA, CanBorrow, Now, Book),
    
    Result = book:extend(StudentB, Later, Book2),
    
    ?assertEqual(book_not_borrowed, Result).

book_cannot_be_extened_when_is_returned_after_till_date_plus_delay_period_test()->
    %delay period is 3 days
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    Later = fun () -> {{2020,4,5},{12,0,0}} end,
    {ok,Book2} = book:borrow(StudentA, CanBorrow, Now, Book),

    Result = book:extend(StudentA, Later, Book2),

    ?assertEqual(too_late, Result).

book_is_extended_by_additional_90_days_from_current_date_test()->
    StudentA = {"300111"},
    BookInfo = {"Title","Author",1},
    Book = book:create(BookInfo),
    {BookId,_,_} = Book,
    CanBorrow = fun (_Student) -> true end,
    Now = fun () -> {{2020,1,1},{12,0,0}} end,
    Later = fun () -> {{2020,3,15},{12,0,0}} end,
    {ok,Book2} = book:borrow(StudentA, CanBorrow, Now, Book),

    {Result, BookResult} = book:extend(StudentA, Later, Book2),

    ExpectedBook = {BookId, BookInfo, [{{2020,3,15},{2020,6,13}, StudentA, false,{}}]},
    ?assertEqual(ok, Result),
    ?assertEqual(ExpectedBook, BookResult).