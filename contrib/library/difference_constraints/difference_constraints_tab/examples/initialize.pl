:- use_module(library(difference_constraints/difference_constraints_rt_ll)).

initialize_state(lcs,L) :- !,
	init_var_state_pair(L,lcs).

initialize_state(S,L) :-
	init_var_state(L,S).

init_var_state_pair([S],S) :- !.
init_var_state_pair([S,S],S) :- !.
init_var_state_pair([S|R],S) :-
	init_var_state_once(R,S).
init_var_state_pair([S2|R],S) :-
	not_cs(S2),
	init_var_state_pair(R,S).

init_var_state_once([S],S) :- !.
init_var_state_once([S|R],S) :- not_cs_list(R).
init_var_state_once([S2|R],S) :-
	not_cs(S2),
	init_var_state_once(R,S).

not_cs(l0).
not_cs(l1).
not_cs(l2).
not_cs(lcs).

not_cs_list([]).
not_cs_list([H|R]) :- 
	not_cs(H),
	not_cs_list(R).

init_var_state([],_).
init_var_state([S|R],S) :-
	init_var_state(R,S).

initialize_time_LB([]).
initialize_time_LB([H|R]) :-
  	H #>= 0,
	initialize_time_LB(R).

initialize_time_LB_equal([]).
initialize_time_LB_equal([H|R]) :-
  	H #>= 0,
	add_diff_var_equal(H,R),
	initialize_time_LB_equal(R).

add_diff_var_equal(_,[]).
add_diff_var_equal(V,[H|R]) :-
  	V #= H,
	add_diff_var_equal(V,R).

initialize_time_equal(N,T) :-
	length(T,N),
	init_var_equal(T).

init_var_equal([_]) :- !.
init_var_equal([H,H]) :- !.
init_var_equal([H,H|R]) :-
	init_var_equal([H|R]).

initialize_time(N,T) :-
	length(T,N),
	init_var(T).

init_var([]).
init_var([H|R]) :-
  	difference_constraints_var(H),
	init_var(R).

initialize_reset(N,R) :-
	length(R,N),
	init_reset(R,1).

init_reset([],_).
init_reset([Id|R],Id) :-
	Id1 is Id + 1,
	init_reset(R,Id1).

