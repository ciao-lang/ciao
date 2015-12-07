:- module(block_tr, [sentence_tr/3, clause_tr/3, conj2list/2], []).

:- use_module(library(aggregates)).
:- use_module(library(sort)).

conj2list(A, [A]):- var(A), !.
conj2list((A,B), [A|L]) :- !, conj2list(B, L).
conj2list(A, [A]).

:- data declaration/5.

% Fail if no '-' are found in the input list
normalize_declaration([], [], X):- nonvar(X).
normalize_declaration([H1|T1], [H2|T2], S):-
	(
	    H1 == '-' -> H2 = -, S = '-'
	;
	    H2 = '?'
	), 
	normalize_declaration(T1, T2, S).


treat_declarations([], _Mod, []).
treat_declarations([Decl1|T1], Mod, [F2/A|T2]):-
	functor(Decl1, F1, A),
	Decl1 =.. [_|L1], 
	atom_concat('$block_', F1, F2),
	(
	    normalize_declaration(L1, L2, _) ->
	    (
		retract_fact(declaration(F1, F2, A, ListDecls1, Mod)) ->
		sort:sort([L2|ListDecls1], ListDecls2),
		assertz_fact(declaration(F1, F2, A, ListDecls2, Mod))
	    ;
		assertz_fact(declaration(F1, F2, A, [L2], Mod))
	    ), 
	    treat_declarations(T1, Mod, T2)
	;
	    true
	).


generate_condition2([], [], []).
generate_condition2([H1|T1], [A|T2], L1):-
	(
	    H1 = '-' -> L1 = [A|L2]
	;
	    L1 = L2
	),
	generate_condition2(T1, T2, L2).

generate_condition1([], _Args, []).
generate_condition1([H1|T1], Args, [H2|T2]):-
	generate_condition2(H1, Args, H2),
	generate_condition1(T1, Args, T2).


generate_clause( ('$no_tr'(Head) :- '$block'(Cond, Goal)),
	          F1, F2, A, ListDecls, _Mod):-
	functor(Head, F1, A),
	Head =.. [_|Args],
	Goal =.. [F2|Args], 
	generate_condition1(ListDecls, Args, Cond).

generate_clauses([], [end_of_file], _Mod).
generate_clauses([t(F1, F2, A, ListDecls)|T1], [Clause|T2], Mod):-
	generate_clause(Clause, F1, F2, A, ListDecls, Mod), 
	generate_clauses(T1, T2, Mod).

sentence_tr(0, [0], Mod):-!,
	retractall_fact(declaration(_, _, _, _, Mod)).
sentence_tr((:-block(Arg)), [(:-impl_defined(L3))], Mod):-!,
	conj2list(Arg, L1),
	treat_declarations(L1, Mod, L2),
	sort:sort(L2, L3).
sentence_tr(end_of_file, Clauses, Mod):-!, 
	findall(t(F1, F2, A, ListDecls),
                declaration(F1, F2, A, ListDecls, Mod), L),
	generate_clauses(L, Clauses, Mod).
sentence_tr(C, [C], _Mod). 

clause_tr(clause('$no_tr'(H), B), clause(H, B), _Mod):-!.
clause_tr(clause(H1, B), clause(H2, B), Mod):-
	(
	    functor(H1, F1, A), 
	    declaration(F1, F2, A, _, Mod) ->
	    H1 =.. [_|Args],  
	    H2 =.. [F2|Args]
	;
	    H1 = H2
	).
	    
