-module(core_lib_user).
-include("core_lib_user.hrl").

-export([create/4,lockBorrowing/1,unlockBorrowing/1]).
-export_type([user_card_id/0,lib_user/0]).

-spec create(string(),user_card_id(), fun(()->calendar:datetime()), fun((user_card_id())->boolean()))
    -> {ok, lib_user()} | {fail, not_unique}.
create(Name, Id, Now, IsUnique)->
    CreationDate = Now(),
    Unique = IsUnique(Id),
    if 
        (not Unique) -> {fail, not_unique};
        true ->
            {ok, #lib_user{name = Name,id = Id,creation_date = CreationDate,can_borrow=true}}
    end.

-spec lockBorrowing(lib_user()) -> {ok, lib_user()}.
lockBorrowing(User) ->
    {ok, User#lib_user{can_borrow = false}}.

-spec unlockBorrowing(lib_user()) -> {ok, lib_user()}.
unlockBorrowing(User) ->
    {ok, User#lib_user{can_borrow = true}}.
