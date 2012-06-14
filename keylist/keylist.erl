%%% Jednoduche Key-List ulozisko.
%%% Autor: Branislav Hasto
-module(keylist).
-behaviour(gen_server).

-export([start_link/0, set/3, prepend/3, append/3, pop/2, peek/2, delete/2, get/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).	



%%% Client API
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Ulozi zoznam List pod kluc Key.
set(Pid, Key, List) when is_list(List) ->
   gen_server:cast(Pid, {set, Key, List}).

%% Vlozi hodnotu Val na zaciatok zoznamu pod klucom Key.
%% Ak pod Key nebol ulozeny ziadny zoznam, bude tam jednoprvkovy [Val].
prepend(Pid, Key, Val) ->
    gen_server:cast(Pid, {prepend, Key, Val}).

%% Vlozi hodnotu Val na koniec zoznamu pod klucom Key.
%% Ak pod Key nebol ulozeny ziadny zoznam, bude tam jednoprvkovy [Val].
append(Pid, Key, Val) ->
    gen_server:cast(Pid, {append, Key, Val}).

%% Vymaze zoznam pod klucom Key.
delete(Pid, Key) ->
	gen_server:cast(Pid, {delete, Key}).

%% Vrati prvy prvok zoznamu pod klucom Key, prvok zo zoznamu odstrani.
%% Ak je pod Key prazdny zoznam, alebo tam zoznam nie je, vrati {error, empty_list}.
pop(Pid, Key) ->
	gen_server:call(Pid, {pop, Key}).

%% Vrati prvy prvok zoznamu pod klucom Key, prvok v zozname ponecha.
%% Ak je pod Key prazdny zoznam, alebo tam zoznam nie je, vrati {error, empty_list}.
peek(Pid, Key) ->
	gen_server:call(Pid, {peek, Key}).

%% Vrati zoznam pod klucom Key.
%% Ak pod Key nie je ulozeny ziadny zoznam, vrati prazdny zoznam [].
get(Pid, Key) -> 
	gen_server:call(Pid, {get, Key}).



%%% Server functions
init([]) -> {ok, dict:new()}. 


handle_cast({set, Key, List}, Dict) ->
    {noreply, dict:store(Key, List, Dict)};

handle_cast({prepend, Key, Val}, Dict) ->
	List = get_list_at_key_or_empty_list(Key, Dict),
	{noreply, dict:store(Key, [Val|List], Dict)};

handle_cast({append, Key, Val}, Dict) ->
	{noreply, dict:append(Key, Val, Dict)};

handle_cast({delete, Key}, Dict) ->
	{noreply, dict:erase(Key, Dict)}.


handle_call({get, Key}, _From, Dict) ->
	{reply, get_list_at_key_or_empty_list(Key, Dict), Dict};

handle_call({pop, Key}, _From, Dict) ->
	case get_list_at_key_or_empty_list(Key, Dict) of
		[H|T] -> {reply, H, dict:store(Key, T, Dict)};
		[] -> empty_list_reply(Dict)
	end;

handle_call({peek, Key}, _From, Dict) ->
	case get_list_at_key_or_empty_list(Key, Dict) of
		[H|_] -> {reply, H, Dict};
		[] -> empty_list_reply(Dict)
	end.


handle_info(Msg, Dict) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, Dict}.

%% Nie je treba clean-up, ale behaviour gen_server vyzaduje tuto funkciu.
terminate(_Reason, _Dict) -> ok.

%% Ziadne planovane zmeny v kode, ale behaviour gen_server vyzaduje tuto funkciu.
code_change(_OldVsn, Dict, _Extra) -> {ok, Dict}. 



%%% Private functions

%% Vrati zoznam pod zadanym klucom, alebo prazdny zoznam.
get_list_at_key_or_empty_list(Key, Dict) ->
	case dict:find(Key, Dict) of
		{ok, List} -> List;
		error -> []
	end.

%% Vytvori odpoved servera pre pripad citania prvku (peek, pop) z prazdneho zoznamu.
empty_list_reply(Dict) -> {reply, {error, empty_list}, Dict}.
