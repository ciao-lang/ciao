:- use_module(library(fd/fd_rt)).
:- use_module(library(fd/fd_bounds)).

% For meta-calls
A .=. B :- '$parse'(A .=. B).
A .>. B :- '$parse'(A .>. B).
A .<. B :- '$parse'(A .<. B).
A .>=. B :- '$parse'(A .>=. B).
A .=<. B :- '$parse'(A .=<. B).
A .<>. B :- '$parse'(A .<>. B).

parse(E):- 
	'$parse'(E).

'$parse'(E) :-
	functor(E, F, 2),
	split(E, A1, A2),
	parse_arg(A1, V1),
	parse_arg(A2, V2),
	process_e(F, V1, V2).
parse_arg(E, V) :-
	(functor(E, F, 2) ->
	 split(E, A1, A2),
	 parse_arg(A1, V1),
	 parse_arg(A2, V2),
	 process_a(F, V1, V2, V)
	;
	 V = E).

split(E, A1, A2) :- arg(1, E, A1), arg(2, E, A2).

process_a(+, V1, V2, V) :- 
	'$add'(V1, V2, V).
process_a(-, V1, V2, V) :- 
	'$subs'(V1, V2, V).
process_a(*, V1, V2, V) :-
	'$mult'(V1, V2, V).
process_a(/, V1, V2, V) :-
	'$mult'(V, V2, V1).


process_e(.=.,V1, V2):- V1 = V2.
process_e(.>., V1, V2):- '$.>.'(V1, V2).
process_e(.<., V1, V2):- '$.<.'(V1, V2).
process_e(.=<., V1, V2):- '$.=<.'(V1, V2).
process_e(.>=., V1, V2):- '$.>=.'(V1, V2).
process_e(.<>., V1, V2):- '$.<>.'(V1, V2).

do_in([],_).
do_in(ListOfVars, Domain) :-
	ListOfVars = [X|Xs],
	X in Domain,
	Xs in Domain.

ListOfVars in Domain :- 
	nonvar(ListOfVars), 
	list(ListOfVars), !,
	do_in(ListOfVars, Domain).

X in M .. N :- 
	(var(M); var(N)), !,
	'$.=<.'(M, N),
	X in min(M) .. max(N).
X in M .. N :- 
	'$in'(X, [[M|N]]).
X in M .. N .&. R :- 
	get_bounds(R, Bounds),
	'$in'(X, [[M|N]|Bounds]).
X in dom(Y) :- '$in_dom'(X, Y) .

get_bounds(M..N, [[M|N]]). 
get_bounds(M..N .&. R, [[M|N]| Bounds]) :-
	get_bounds(R, Bounds).

'$.<.'(X, Y) :-
	var(X), var(Y), !,
	absolute_bounds(U,L), 
	[X, Y] in L .. U,
	Y in min_plus_c(X, 1) .. U,
	X in L .. max_sub_c(Y, 1) .

'$.<.'(X, C) :-
	var(X), number(C), !,
	C1 is C - 1,
	absolute_lower_bound(L), 
	X in L .. C1 .

'$.<.'(C, Y) :-
	number(C), var(Y), !,
	C1 is C + 1,
	absolute_lower_bound(U), 
	Y in C1 .. U .

'$.<.'(C1, C2) :-
	number(C1), number(C2),
	C1 < C2.

'$.=<.'(X, Y) :-
	var(X), var(Y), !,
	absolute_bounds(U,L), 
	[X, Y] in L .. U,
	Y in min(X) .. U,
	X in L .. max(Y) .

'$.=<.'(X, C) :-
	var(X), number(C), !,
	absolute_lower_bound(L), 
	X in L .. C .

'$.=<.'(C, Y) :-
	number(C), var(Y), !,
	absolute_lower_bound(U), 
	Y in C .. U .

'$.=<.'(C1, C2) :-
	number(C1), number(C2),
	C1 =< C2 .

'$.>.'(X, Y) :-
	var(X), var(Y), !,
	absolute_bounds(U,L), 
	[X, Y] in L .. U,
	X in min_plus_c(Y, 1) .. U,
	Y in L .. max_sub_c(X, 1) .

'$.>.'(X, C) :-
	var(X), number(C), !, 
	absolute_upper_bound(U),
	C1 is C + 1,
	X in C1 .. U .

'$.>.'(C, Y) :-
	number(C), var(Y), !, 
	absolute_upper_bound(L),
	C1 is C - 1,
	Y in L .. C1 .

'$.>.'(C1, C2) :-
	number(C1), number(C2), 
	C1 > C2.

'$.>=.'(X, Y) :-
	var(X), var(Y), !,
	absolute_bounds(U,L), 
	[X, Y] in L .. U,
	X in min(Y) .. U,
	Y in L .. max(X) .

'$.>=.'(X, C) :-
	var(X), number(C), !, 
	absolute_upper_bound(U),
	X in C .. U .

'$.>=.'(C, Y) :-
	number(C), var(Y), !, 
	absolute_lower_bound(L), 
	Y in L .. C .

'$.>=.'(C1, C2) :-
	number(C1), number(C2), 
	C1 >= C2.

'$.<>.'(X, Y) :-
	var(X), var(Y), !,
	absolute_bounds(U,L), 
	[X, Y] in L .. U,	
	differ_on_instantiation(X, Y).
'$.<>.'(X, C) :-
	var(X), number(C), !,
	absolute_bounds(U,L), 
	[X, Z] in L .. U,
	differ_on_instantiation(X, Z),
	Z = C.
'$.<>.'(C, Y) :-
	number(C), var(Y), !,
	absolute_bounds(U,L), 
	[Y, Z] in L .. U,
	differ_on_instantiation(Y, Z),
	Z = C.
'$.<>.'(C1, C2) :-
	number(C1), number(C2), !,
	\+ C1 == C2.

'$mult'(C1, C2, C):-
	number(C1), 
	number(C2), !,
	C is C1 * C2.

'$mult'(Y, Z, X):-
	var(X), var(Y), var(Z), !,
	absolute_bounds(U,L), 
 	[X, Y, Z] in L .. U,
	X in min_mult_min(Y, Z) .. max_mult_max(Y, Z),
	Y in min_div_max(X, Z) .. max_div_min(X, Z),
	Z in min_div_max(X, Y) .. max_div_min(X, Y).

'$mult'(C, Y, X):-
	number(C), !,
	\+ (C = 0),
 	var(Y), 
	absolute_bounds(U,L), 
	[X, Y] in L .. U,
	C1 is 1/C,
	(C > 0 ->
	 X in min_mult_c(Y, C) .. max_mult_c(Y, C),
	 Y in min_mult_c(X, C1) .. max_mult_c(X, C1)
	;
	 CAbs is abs(C),
	 '$mult'(CAbs, Y, X1),
	 '$subs'(0, X1, X)).

'$mult'(Y, IC, X):-
	'$mult'(IC, Y, X).

'$add'(X, Y, Z) :-
	var(X), var(Y), var(Z), !,
	absolute_bounds(U,L), 
	[X, Y, Z] in L .. U,
	X in min_sub_max(Z, Y) .. max_sub_min(Z, Y),
	Y in min_sub_max(Z, X) .. max_sub_min(Z, X),
	Z in min_plus_min(X, Y) .. max_plus_max(X, Y).
	
'$add'(Y, C, X) :-
	(number(C), var(Y)), !,
	absolute_bounds(U,L), 
	[X, Y] in L .. U,
	X in min_plus_c(Y, C) .. max_plus_c(Y, C),
	Y in min_sub_c(X, C) .. max_sub_c(X, C) .
'$add'(C, Y, X) :-
	(number(C), var(Y)), !, 
	absolute_bounds(U,L),
	[X, Y] in L .. U,
	X in min_plus_c(Y, C) .. max_plus_c(Y, C),
	Y in min_sub_c(X, C) .. max_sub_c(X, C) .
'$add'(C1, C2, X) :-
	X is C1 + C2.

'$subs'(X, Y, Z) :-
	var(X), var(Y), var(Z),!,
	'$add'(Z, Y, X).

'$subs'(Y, C, X) :-
	(number(C), var(Y)), !, 
	absolute_bounds(U,L),
	[X, Y] in L .. U,
	X in min_sub_c(Y, C) .. max_sub_c(Y, C),
	Y in min_plus_c(X, C) .. max_plus_c(X, C) .

'$subs'(C, Y, X) :-
	(number(C), var(Y)), !, 
	 '$add'(X, Y, Z),
	 Z = C.

'$subs'(C1, C2, X) :-
	X is C1 - C2.

all_different([X|Xs]) :-
	all_different_aux(Xs, X),
	all_different(Xs).
all_different([]) .

all_different_aux([X|Xs], V) :-
	'$.<>.'(V, X), 
	all_different_aux(Xs, V).
all_different_aux([],_) .
