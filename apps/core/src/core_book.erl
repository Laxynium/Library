-module(core_book).
-include("core_book.hrl").

-export([create/1,borrow/4, return/3, extend/3, whoBorrowed/2, isBorrowed/1]).
-export_type([book_id/0, book_info/0,check_out_info/0, book/0]).

-define(CheckOutPeriotInDays, 90).
-define(PunishmentRate, 0.20).
-define(Delay, 3).

-type book_id() :: {binary()}.
-type book_info() :: #book_info{}.
-type check_out_info() :: #check_out_info{}.
-type book() :: #book{}.

-spec create(book_info()) -> book().
create(BookInfo) ->
    {{core_uuid:v4()}, BookInfo, []}.

-spec borrow(lib_user:user_card_id(), fun((lib_user:user_card_id())->boolean()), fun(()->calendar:datetime()), book()) -> {ok, book()} | cannot_borrow |already_borrowed.
borrow(StudentId, CanBorrow, Now,{Id,BookInfo, Checkouts}) ->
    case CanBorrow(StudentId) of 
        false -> 
            cannot_borrow;
        true ->
            case Checkouts of 
                [{_,_,_,Returned,_}|_] when not Returned ->
                    already_borrowed;
                [LastCheckOut|Older] -> %TODO refactor it to common function
                    {Since,_Time} = Now(),
                    Till = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Since) + ?CheckOutPeriotInDays),
                    {ok, {Id, BookInfo, [{Since, Till, StudentId, false,{}}, LastCheckOut | Older]}};
                [] ->
                    {Since,_Time} = Now(),
                    Till = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Since) + ?CheckOutPeriotInDays),
                    {ok, {Id, BookInfo, [{Since, Till, StudentId, false,{}}]}}
            end
    end.

-spec return(lib_user:user_card_id(), fun(()->calendar:datetime()),book()) -> {ok, book()} | {punishment, float(), book()} | book_not_borrowed. % todo replace with decimal type from some library
return(StudentId, Now, {Id, BookInfo, CheckOuts}) ->
    case CheckOuts of
        [] ->
            book_not_borrowed;
        [{_,_,By,Returned,_}|_] when (By =/= StudentId) or Returned->
            book_not_borrowed;
        [{Since,Till,By,_,_}|Older] ->
            {CurrentDate, _} = Now(),
            CurrentDateInDays = calendar:date_to_gregorian_days(CurrentDate),
            TillInDays = calendar:date_to_gregorian_days(Till),
            Difference = CurrentDateInDays - TillInDays,
            if 
                Difference >= 1 ->
                    Punishment = Difference * ?PunishmentRate,
                    {punishment, Punishment, {Id, BookInfo, [{Since, Till, By, true, CurrentDate} | Older]}};
                true -> {ok, {Id, BookInfo, [{Since, Till, By, true, CurrentDate} | Older]}}
            end
    end.

-spec extend(lib_user:user_card_id(), fun(()->calendar:datetime()), book()) -> {ok, book()} | too_late | book_not_borrowed.
extend(StudentId, Now, {Id, BookInfo, CheckOuts}) -> 
    case CheckOuts of 
        [] -> 
            book_not_borrowed;
        [{_,_,By,Returned,_} | _] when (By /= StudentId) or Returned ->
            book_not_borrowed;
        [{_Since, Till, By, _, _} | Older] ->
        {CurrentDate, _} = Now(),
        CurrentDateInDays = calendar:date_to_gregorian_days(CurrentDate),
        TillInDays = calendar:date_to_gregorian_days(Till),
        Difference = CurrentDateInDays - (TillInDays + ?Delay),
        if 
            Difference >= 1 ->
                too_late;
            true ->
                NewTill = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(CurrentDate) + ?CheckOutPeriotInDays),
                {ok, {Id, BookInfo, [{CurrentDate, NewTill, By, false,{}} | Older]}}
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







