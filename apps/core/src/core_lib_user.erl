-module(core_lib_user).

-export([create/3,lockRenting/1,unlockRenting/1]).
-export_type([user_card_id/0,lib_user/0]).

-type user_card_id() :: string().

-record(lib_user,{
    name::string(),
    id::user_card_id(),
    creation_date::calendar:datetime(),
    can_rent::boolean()}).
-type lib_user() :: #lib_user{}.

-spec create(string(),user_card_id(),fun(()->calendar:datetime())) -> lib_user().
create(Name,Id,Now) ->
    CreationDate = Now(),
    #lib_user{name = Name,id = Id,creation_date = CreationDate,can_rent=true}.

-spec lockRenting(lib_user()) -> lib_user().
lockRenting(User) ->
    User#lib_user{can_rent = false}.

-spec unlockRenting(lib_user()) -> lib_user().
unlockRenting(User) ->
    User#lib_user{can_rent = true}.




