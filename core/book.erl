-module(book).

-export([create/1,borrow/4, return/3, extend/3]).
-export_type([book_id/0, student_card_id/0, book_info/0,check_out_info/0, book/0]).
-define(CheckOutPeriotInDays, 90).
-define(PunishmentRate, 0.20).
-define(Delay, 3).

-type book_id() :: {binary()}.
-type student_card_id() :: {string()}.

-record (book_info,{
    title::string(), 
    author::string(), 
    version::integer()
}).
-type book_info() :: #book_info{}.

-record(check_out_info, {
    since :: calendar:date(), 
    till :: calendar:date(),
    by :: student_card_id(),
    returned :: boolean(),
    returned_at :: calendar:date() | {}
}).
-type check_out_info() :: #check_out_info{}.

-record (book, {
    id :: book_id(), 
    book_info :: book_info(),
    check_out_info :: [check_out_info()]
}).
-type book() :: #book{}.

-spec create(book_info()) -> book().
create(BookInfo) ->
    {{uuid:v4()}, BookInfo, []}.

-spec borrow(student_card_id(), fun((student_card_id())->boolean()), fun(()->calendar:datetime()), book()) -> {ok, book()} | cannot_borrow |already_borrowed.
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

-spec return(student_card_id(), fun(()->calendar:datetime()),book()) -> {ok, book()} | {punishment, float(), book()} | book_not_borrwed. % todo replace with decimal type from some library
return(StudentId, Now, {Id, BookInfo, CheckOuts}) ->
    case CheckOuts of
        [] ->
            book_not_borrwed;
        [{_,_,By,Returned,_}|_] when (By =/= StudentId) or Returned->
            book_not_borrwed;
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

-spec extend(student_card_id(), fun(()->calendar:datetime()), book()) -> {ok, book()} | too_late | book_not_borrwed.
extend(StudentId, Now, {Id, BookInfo, CheckOuts}) -> 
    case CheckOuts of 
        [] -> 
            book_not_borrwed;
        [{_,_,By,Returned,_} | _] when (By /= StudentId) or Returned ->
            book_not_borrwed;
        [{Since, Till, By, _, _} | Older] ->
        Delay = 3,
        {CurrentDate, _} = Now(),
        CurrentDateInDays = calendar:date_to_gregorian_days(CurrentDate),
        TillInDays = calendar:date_to_gregorian_days(Till),
        Difference = CurrentDateInDays - (TillInDays + Delay),
        if 
            Difference >= 1 ->
                too_late;
            true ->
                Till = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Since) + ?CheckOutPeriotInDays),
                {ok, {Id, BookInfo, [{CurrentDate, Till, By, false,{}} | Older]}}
        end
    end.



