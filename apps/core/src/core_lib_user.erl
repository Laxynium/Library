-module(core_lib_user).
-include("core_lib_user.hrl").

-export([create/3,lockRenting/1,unlockRenting/1,getID/1,getCanRent/1,getCreationDate/1,getName/1]).
-export_type([user_card_id/0,lib_user/0]).

-type user_card_id() :: string().
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

%records can't be exported, just gonna create a lot of getters
%not worth exporting records to external file
-spec getID(lib_user()) -> user_card_id().
getID(#lib_user{id=ID}) -> ID.

-spec getCanRent(lib_user()) -> boolean().
getCanRent(#lib_user{can_rent = CanRent}) -> CanRent.

-spec getCreationDate(lib_user()) -> calendar:datetime().
getCreationDate(#lib_user{creation_date = Date}) -> Date.

-spec getName(lib_user()) -> string().
getName(#lib_user{name = Name}) -> Name.




