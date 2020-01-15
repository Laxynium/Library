-module(core_lib_user).
-include("core_lib_user.hrl").

-export([create/3,lockBorrowing/1,unlockBorrowing/1]).
-export_type([user_card_id/0,lib_user/0]).

-spec create(string(),user_card_id(),fun(()->calendar:datetime())) -> lib_user().
create(Name,Id,Now) ->
    CreationDate = Now(),
    #lib_user{name = Name,id = Id,creation_date = CreationDate,can_borrow=true}.

-spec lockBorrowing(lib_user()) -> lib_user().
lockBorrowing(User) ->
    User#lib_user{can_borrow = false}.

-spec unlockBorrowing(lib_user()) -> lib_user().
unlockBorrowing(User) ->
    User#lib_user{can_borrow = true}.
