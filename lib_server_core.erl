-module(lib_server_core).
-behaviour(gen_server).
-record(serverState, {db_data,activeHandlers}).
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
    gen_server:start_link({local,lib_server}, ?MODULE, [DBpath], []).

init(DBpath) ->
    {ok, #serverState{db_data = lib_server_db:loadDBFromFile(DBpath),activeHandlers = []}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

%echo reply
handle_call(echoRequest, _From, State) ->
    {reply, echoReply, State};

%db query reply
handle_call({dbQuery,Data},From,State) ->
    DBrequestResult = lib_server_db:handleDBquery(Data,State#serverState.db_data),
    %idk czy reply dodaje jakies abstrakcje w odpowiedzi zwrotnej to narazie dałem tu surową wiadomość
    %sprawdzić czy 2 wersja działa poprawnie
    From!{dbReply,DBrequestResult},
    {noreply,State};

%db update reply
handle_call({dbUpdate,Data},From,State) ->
    {DBupdateResult,updatedDB} = lib_server_db:handleDBupdate(Data,State#serverState.db_data),
    From!{dbReply,DBupdateResult},
    {noreply,State#serverState{db_data = updatedDB}};

%handler creation reply 
handle_call({RequestType,Data},From,State) ->
    NewHandlerPID  = lib_server_handler_spawn:createHandler(RequestType,From,Data),
    Handlers = State#serverState.activeHandlers,
    {noreply,State#serverState{activeHandlers = [NewHandlerPID | Handlers]}}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
    io:format("Unexpected message: ~p~n",[_Msg]),
   {noreply, State}.

terminate(_Reason, _State) ->
    %TODO kill all active handlers, save DB

   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


