%% Autor: Branislav Hasto
-module(my_set).
-export([newset/0, insert/2, delete/2, min/1, prec/2, diff/2, equals/2, card/1, any/2, show/1, foldl/3, product/3]).

%% public API

%% Mnozina je usporiadana dvojica atomu my_set a zoznamu.
%% Mnozina moze obsahovat iba cisla, jej prvky su unikatne a je vzdy zoradena vzostupne.

%% Vytvori prazdnu mnozinu.
newset() -> {my_set, []}.


%% Vlozi cislo do mnoziny. 
%% Vrati mnozinu s pridanym prvkom, alebo nezmenenu mnozinu (ak prvok v mnozine uz bol).
insert({my_set, List}, Ele) when is_list(List), is_number(Ele) -> 
	{my_set, insert_to_list(List, Ele)}.


%% Odstrani cislo z mnoziny.
%% Vrati mnozinu bez zadaneho prvku (ak sa v mnozine nechadzal), alebo nezmenenu mnozinu (ak v nej prvok nebol).
delete({my_set, List}, Ele) when is_list(List), is_number(Ele) -> 
	{my_set, delete_from_list(List, Ele)}.


%% Vrati najnizsie cislo z mnoziny, alebo atom nil, ak je mnozina prazdna.
min({my_set, []}) -> 
	nil;
min({my_set, [H|_]}) -> 
	H.


%% Vrati prvok mnoziny, ktory sa v mnozine nachadza pred zadanym prvkom.
%% Ak sa zadany prvok v mnozine nenachadza, alebo je to prvy prvok mnoziny, vrati atom nil.
prec({my_set, List}, Ele) when is_list(List), is_number(Ele) -> 
	prec_in_list(List, Ele).


%% Vrati mnozinu, ktora obsahuje vsetky prvky rozdielu dvoch zadanych mnozin.
%% Ak su obe zadane mnoziny prazdne, vrati prazdnu mnozinu.
diff({my_set, List1}, {my_set, List2}) when is_list(List1), is_list(List2) -> 
	{my_set, list_diff(List1, List2, [])}.

	
%% Vrati true, ak su zadane mnoziny rovnake, inak false.
%% Ak su obe mnoziny prazdne, vrati true.
equals({my_set, List1}, {my_set, List2}) when is_list(List1), is_list(List2) ->
	list_equals(List1, List2).


%% Vrati kardinalitu mnoziny.
card({my_set, List}) when is_list(List) ->
	list_length(List, 0).


%% Vrati true, ak aspon jeden prvok mnoziny splna zadany predikat, inak false.
any({my_set, List}, Func) when is_list(List), is_function(Func, 1) ->
	any_in_list(List, Func).


%% Vypise vsetky prvky mnoziny v tvare [prvok1, prvok2, ...]
show({my_set, List}) when is_list(List) ->
	show_list(List, true).


%% Vykona operaciu fold zlava na zadanej mnozine s pociatocnou hodnotou akumulatora Acc0.
foldl({my_set, List}, Func, Acc0) when is_list(List), is_function(Func, 2) ->
	list_foldl(List, Func, Acc0).


%% Karteziansky sucin dvoch mnozin, vysledok sucinu urcuje zadana funkcia.
product({my_set, List1}, {my_set, List2}, Func) when is_list(List1), is_list(List2), is_function(Func, 2) ->
	list_product(List1, List2, Func).
	
	

%% private functions 

insert_to_list([], Ele) -> [Ele];
insert_to_list(List = [H|_], H) -> List;
insert_to_list(List = [H|_], Ele) when Ele < H -> [Ele|List];
insert_to_list([H|T], Ele) when Ele > H -> [H | insert_to_list(T, Ele)].


list_reverse(List) -> list_reverse(List, []).
list_reverse([], Acc) -> Acc;
list_reverse([H|T], Acc) -> list_reverse(T, [H|Acc]).


to_set(List) -> to_set(List, []).

to_set([], Acc) -> {my_set, Acc};
to_set([H|T], Acc) -> to_set(T, insert_to_list(Acc, H)).


delete_from_list([], _) -> [];
delete_from_list([H|T], H) -> T;
delete_from_list([H|T], Ele) -> [H | delete_from_list(T, Ele)].


prec_in_list([], _) -> nil;
prec_in_list([H,Ele|_], Ele) -> H;
prec_in_list([_|T], Ele) -> prec_in_list(T, Ele).


list_diff([], [], Acc) -> list_reverse(Acc);
list_diff([], [H|T], Acc) -> list_diff([], T, [H|Acc]);
list_diff([H|T], [], Acc) -> list_diff([], T, [H|Acc]);
list_diff([H|T1], [H|T2], Acc) -> list_diff(T1, T2, Acc);
list_diff([H1|T1], List2 = [H2|_], Acc) when H1 < H2 -> list_diff(T1, List2, [H1|Acc]);
list_diff(List1 = [H1|_], [H2|T2], Acc) when H2 < H1 -> list_diff(List1, T2, [H2|Acc]).

	
list_equals([], []) -> true;
list_equals([H|T], [H|T]) -> true;
list_equals(_, _) -> false.


list_length([], Acc) -> Acc;
list_length([_|T], Acc) -> list_length(T, Acc + 1).


list_foldl([], _, Acc) -> Acc;
list_foldl([H|T], Func, Acc) -> list_foldl(T, Func, Func(H, Acc)).


list_product(List1, List2, Func) -> to_set([Func(X, Y) || X <- List1, Y <- List2]).


any_in_list([], _) -> false;
any_in_list([H|T], Func) ->
	case Func(H) of
		true -> true;
		false -> any_in_list(T, Func)
	end.


show_list([], _) -> io:format("[]~n");
show_list([H|[]], Start) ->
	case Start of
		true -> io:format("[~w]~n", [H]);
		false -> io:format(", ~w]~n", [H])
	end;
show_list([H|T], Start) ->
	case Start of
		true -> io:format("[~w", [H]), show_list(T, false);
		false -> io:format(", ~w", [H]), show_list(T, false)
	end.

