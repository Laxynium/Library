-record(lib_user,{
    name::string(),
    id::user_card_id(),
    creation_date::calendar:datetime(),
    can_borrow::boolean()}).

-type user_card_id() :: string().
-type lib_user() :: #lib_user{}.