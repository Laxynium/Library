-module(lib_server_core).
-behaviour(gen_server).
-record(server_state, {db_data::lib_server_db:db_data(),handlers::[pid()]}).
-type server_state() :: #server_state{}.
%% API
-export([ stop/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%gen_serwer robi za bazę danych, trzyma te dane w swoim stanie, funkcje bazy są w module lib_server_db
%gen_serwer na requesty od wszystkich obiektów zewnętrznych tworzy handlery, zapisuje ich PID w stanie
%zapewnianiem usługi zajmują się w 100% handlery
%handlery mogą wywoływać do serwera zapytania bazodanowe
%poza stworzeniem handlera zewnętrzne obiekty mają tylko kontakt z handlerami 


%% gen_server functions
stop(Name) ->
   gen_server:call(Name, stop).

start_link() ->
    DBpath = "todo",
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DBpath], []).

init(DBpath) ->
    {ok, #server_state{db_data = lib_server_db:loadDBFromFile(DBpath),handlers = []}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

%db query reply
handle_call({dbQuery,Data},From,State) ->
    DBrequestResult = lib_server_db:handleDBquery(Data,State#server_state.db_data),
    From!{dbReply,DBrequestResult},
    {noreply,State};

%db update reply
handle_call({dbUpdate,Data},From,State) ->
    {DBupdateResult,updatedDB} = lib_server_db:handleDBupdate(Data,State#server_state.db_data),
    From!{dbReply,DBupdateResult},
    {noreply,State#server_state{db_data = updatedDB}};

%handler creation reply 
handle_call({RequestType,Data,Ref},From,State) ->
    NewHandlerPID  = lib_server_handler_spawn:createHandler(RequestType,Data,From,Ref),
    Handlers = State#server_state.handlers,
    {noreply,State#server_state{handlers = [NewHandlerPID | Handlers]}}.

%handler cleanup
handle_cast({handlerDone,HandlerID}, State) ->
    Handlers = State#server_state.handlers,
    NewHandlers = libutil:deleteFirstMatch(fun(ID) -> ID == HandlerID end,State#server_state.handlers),
   {noreply, State#server_state{handlers = NewHandlers}}.

handle_info(_Msg, State) ->
    % io:format("Unexpected message: ~p~n",[_Msg]),
   {noreply, State}.

terminate(_Reason, State) ->
    lists:foreach(fun(Handler) -> exit(Handler,kill) end,State#server_state.handlers),
    %TODO save DB
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


