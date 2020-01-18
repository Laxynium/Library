-module(core_book).
-include("core_book.hrl").

-export([create/1,borrow/4, return/3, extend/3, whoBorrowed/2, isBorrowed/1, is_overdue/2]).
-export_type([book_id/0, book_info/0,check_out_info/0, book/0]).

-define(CheckOutPeriotInDays, 90).
-define(PunishmentRate, 0.20).
-define(Delay, 3).

-spec create(book_info()) -> {ok, book()}.
create(BookInfo=#book_info{}) ->
    {ok,#book{id={core_uuid:v4()}, book_info=BookInfo, check_out_info=[]}}.

-spec borrow(lib_user:user_card_id(), fun((lib_user:user_card_id())->boolean()), fun(()->calendar:datetime()), book()) 
    -> {ok, book()} | {fail, cannot_borrow} | {fail, already_borrowed}.
borrow(StudentId, CanBorrow, Now, #book{id=Id,book_info=BookInfo, check_out_info=Checkouts}) ->
    case CanBorrow(StudentId) of 
        false -> 
            {fail, cannot_borrow};
        true ->
            case Checkouts of 
                [#check_out_info{returned=Returned}|_] when not Returned ->
                    {fail, already_borrowed};
                [LastCheckOut=#check_out_info{}|Older] ->
                    {Since,_Time} = Now(),
                    Till = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Since) + ?CheckOutPeriotInDays),
                    {ok, #book{id=Id, book_info=BookInfo, check_out_info=[#check_out_info{since=Since, till=Till, by=StudentId, returned=false, returned_at={}}, LastCheckOut | Older]}};
                [] ->
                    {Since,_Time} = Now(),
                    Till = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Since) + ?CheckOutPeriotInDays),
                    {ok, #book{id=Id, book_info=BookInfo, check_out_info=[#check_out_info{since=Since, till=Till, by=StudentId, returned=false,returned_at={}}]}}
            end
    end.

-spec return(lib_user:user_card_id(), fun(()->calendar:datetime()),book()) 
    -> {ok, book()} | {punishment, float(), book()} | {fail, book_not_borrowed}. % todo replace with decimal type from some library
return(StudentId, Now, #book{id=Id, book_info=BookInfo, check_out_info=CheckOuts}) ->
    case CheckOuts of
        [] ->
            {fail, book_not_borrowed};
        [#check_out_info{by=By,returned=Returned}|_] when (By =/= StudentId) or Returned->
            {fail, book_not_borrowed};
        [#check_out_info{since=Since,till=Till,by=By}|Older] ->
            {CurrentDate, _} = Now(),
            CurrentDateInDays = calendar:date_to_gregorian_days(CurrentDate),
            TillInDays = calendar:date_to_gregorian_days(Till),
            Difference = CurrentDateInDays - TillInDays,
            if 
                Difference >= 1 ->
                    Punishment = Difference * ?PunishmentRate,
                    {punishment, Punishment, #book{id=Id, book_info=BookInfo, check_out_info=[#check_out_info{since=Since, till=Till, by=By, returned=true, returned_at=CurrentDate} | Older]}};
                true -> {ok, #book{id=Id, book_info=BookInfo, check_out_info=[#check_out_info{since=Since, till=Till, by=By, returned=true, returned_at=CurrentDate} | Older]}}
            end
    end.

-spec extend(lib_user:user_card_id(), fun(()->calendar:datetime()), book()) 
-> {ok, book()} | {fail, too_late} | {fail, book_not_borrowed}.
extend(StudentId, Now, #book{id=Id, book_info=BookInfo, check_out_info=CheckOuts}) -> 
    case CheckOuts of 
        [] -> 
            {fail, book_not_borrowed};
        [#check_out_info{by=By,returned=Returned} | _] when (By /= StudentId) or Returned ->
            {fail, book_not_borrowed};
        [#check_out_info{till=Till, by=By} | Older] ->
        {CurrentDate, _} = Now(),
        CurrentDateInDays = calendar:date_to_gregorian_days(CurrentDate),
        TillInDays = calendar:date_to_gregorian_days(Till),
        Difference = CurrentDateInDays - (TillInDays + ?Delay),
        if 
            Difference >= 1 ->
                {fail, too_late};
            true ->
                NewTill = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(CurrentDate) + ?CheckOutPeriotInDays),
                {ok, #book{id=Id, book_info=BookInfo, check_out_info=[#check_out_info{since=CurrentDate, till=NewTill, by=By, returned=false,returned_at={}} | Older]}}
        end
    end.

-spec whoBorrowed(book(),fun(() ->calendar:datetime())) -> {ok,lib_user:user_card_id()} | none.
whoBorrowed(#book{id = ID, check_out_info = CheckOutList},Date) ->
    DateVal = Date(),
    Result = libutil:firstMatch(fun(#check_out_info{since = Since,till = Till}) -> 
        Since_sec = calendar:datetime_to_gregorian_seconds(Since),
        Till_sec = calendar:datetime_to_gregorian_seconds(Till),
        ((Since_sec =< DateVal) and (Till_sec > DateVal)) end ,CheckOutList),
    case Result of
        {ok,By} -> {ok,By};
        none -> none
    end.

-spec isBorrowed(book()) -> boolean().
isBorrowed(Book) ->
    case whoBorrowed(Book,fun() ->calendar:universal_time() end) of
        none -> false;
        _ -> true
    end.

-spec is_overdue(book(), fun(() -> calendar:datetime())) -> boolean().
is_overdue(#book{check_out_info=CheckOuts}, Now)->
    case CheckOuts of
        [#check_out_info{till=Till,returned=false}|_] ->
            is_after(Now(),{Till,{}});
        _ -> false
    end.
    
is_after({ToCheckDate,_Time}, {ReferenceDate,_})->
    ToCheckDateInDays = calendar:date_to_gregorian_days(ToCheckDate),
    ReferenceDateInDays = calendar:date_to_gregorian_days(ReferenceDate),
    ToCheckDateInDays > ReferenceDateInDays.






