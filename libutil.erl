-module(libutil).
-export([firstMatch/2,deleteFirstMatch/2]).

-spec firstMatch(fun((T1) -> boolean()),[T1]) -> {ok,T1} | none.
firstMatch(_Condition,[]) -> none;
firstMatch(Condition,[Elem]) -> 
    Valid = Condition(Elem),
    if
        Valid -> {ok,Elem};
        true -> none
    end;
firstMatch(Condition,[H,T]) ->
    Valid = Condition(H),
    if 
        Valid -> {ok,H};
        true -> firstMatch(Condition,T)
    end.

-spec deleteFirstMatch(fun((T1) -> boolean()),[T1]) -> [T1].
deleteFirstMatch(Condition,List)->
    deleteFirstMatch2(Condition,[],List).

-spec deleteFirstMatch2(fun((T1) -> boolean()),[T1],[T1]) -> [T1].
deleteFirstMatch2(Condition,Searched,[]) -> Searched;
deleteFirstMatch2(Condition,Searched,[H|T]) -> 
    Valid = Condition(H),
    if
        Valid -> Searched ++ T;
        true -> deleteFirstMatch2(Condition,Searched ++ [H],T)
    end.






