:- module(menu_tr, _, [assertions, regtypes]). % [ menu_term_expansion/3 ] ).

:- use_module(library(menu/pattern_filler)).
:- use_module(library(aggregates), [findall/3]).
:- include(library(menu/menu_op)).

menu_term_expansion(0, _, _) :-
	!.
menu_term_expansion(':-'(guard(Head),Body), [NewCl], _M) :-
	!,
	transform_guard(Head, Body, NewCl).
menu_term_expansion(A,  _, _) :-
	functor(A, ':-', _),
	!,
	fail.
menu_term_expansion(end_of_file, A, M) :-
	!,
%	display('-----------------------'),nl,
%	show_all_menus(M),
%	display('.--------------------------.\n'),
	get_all_menus_and_defs(M, A).
%	A \== [end_of_file],
%	display(A),nl.

%	display('-----------------------'),nl,
%	show_all_menus(M),
%	!,
%	fail.
menu_term_expansion(A,  MP, M) :-
	functor(A, ',', _),
	!,
%	display(A),nl,
	get_menu_pattern(Pattern),
	complete_with_pattern(A, Pattern, B),
%	display(B),nl,
	convert_to_menu_opt(B, MP),
	assertz_fact(m(M, B)).
% 	displayq(A),nl,
% 	displayq(B),
% 	nl,
% 	nl.

% ---------------------------------------------------------------------------

% m(Module, Pattern)
:- data m/2.

%% menu, title # flag - option : pre_action :: post_action <- guard.

get_menu_pattern(Pattern) :-
	get_pattern((menu, title# flag-option :true ::true <-true), Pattern).

%% convert_to_menu_opt(','(lt,#('Perform Type Eval',:(type_eval,-(::(option,'<-'(true,ana_g1)),true)))), A).

convert_to_menu_opt(MP, M) :-
 	get_pattern((Menu, Title # Flag- _Option : P_A :: PO_A <-G), P),
 	get_pattern( MP                                            , P),
	M = menu_opt(Menu, Flag, Title, G,  P_A, PO_A).

show_all_menus(M) :-
	show_all_def_values(M),
	current_fact(m(M, A)),
	convert_to_menu_opt(A, MenuP),
	display(MenuP), nl,
	fail.
show_all_menus(_).

show_all_def_values(M) :-
	current_fact(m(M, A)),
 	get_pattern((Menu, _ # Flag- Option : _ :: _ <-_), A),
	display(menu_default(Menu, Flag, Option)), nl,
	fail.
show_all_def_values(_).

get_all_menus_and_defs(M, ALL) :-
	findall(A, retract_fact(m(M, A)), List),
	List \== [],
%	get_all_menus(List, ALL, Tail),
	ALL=Tail,
	get_all_def_values(List, Tail).

get_all_menus([ M  | Ms  ], [ MP | MMs ], Tail) :-
        convert_to_menu_opt(M, MP),
	get_all_menus(Ms, MMs, Tail).
get_all_menus([ ], T, T).
	
get_all_def_values([M|Ms], [D|Ds]) :-
 	get_pattern((Menu, _Title# Flag- Option : _ :: _ <-_), P),
 	get_pattern(M                                        , P),
	functor(Menu, MenuF, _),
	(MenuF = '~' -> MenuF2 = ('~'(functor1(Menu))) ; MenuF2 = MenuF),
	D = menu_default(MenuF2, Flag, Option),
	get_all_def_values(Ms, Ds).
get_all_def_values([], [end_of_file]).

transform_guard(Head, Body, (Head :- NBody)) :-
	arg(1, Head, X),
	transform_body((uni_type(X,Z),Body), Z, NBody).

transform_body((I->T;E), X, (NI->NT;NE)) :- !,
	transform_body(I, X, NI),
	transform_body(T, X, NT),
	transform_body(E, X, NE).
transform_body((A;B), X, (NA;NB)) :- !,
	transform_body(A, X, NA),
	transform_body(B, X, NB).
transform_body((A,B), X, (NA,NB)) :- !,
	transform_body(A, X, NA),
	transform_body(B, X, NB).
transform_body(A==B          , X,  eq(X, A, B)) :- !.
transform_body(A\==B         , X, neq(X, A, B)) :- !.
transform_body(member(A, B), _, vmember(A, B)) :- !.
transform_body(A             , _, A).

