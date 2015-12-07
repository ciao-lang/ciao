:- module(bundlehooks_tr, [defdep/3], [assertions]).

:- data hook_decl/3.

% by default the separator is '.' but could be ''
% defdep(0,_,M) :-
% 	asserta_fact(dependency_separator('.',M)).
defdep(end_of_file, Clauses, M) :-
	get_multifile_preds(Clauses, M).
%
defdep('$builder_hook'(ItemCmd), Clauses, Mod) :- !,
	defdep(('$builder_hook'(ItemCmd) :- true), Clauses, Mod).
defdep(('$builder_hook'(ItemCmd) :- Body), Clauses, Mod) :- 
	nonvar(ItemCmd),
	!,
	( ItemCmd = Item:Cmd -> true
	; Item = '', Cmd = ItemCmd
	),
	gen_clause_target(Cmd, Item, Body, Clauses, Mod).
defdep((:- def_third_party(Name, Props)), Clauses, Mod) :- !,
	gen_third_party(Name, Props, Clauses, Mod).

gen_clause_target(Target, Item, Body, Clauses, Mod) :-
	functor(Target, N, A),
	functor(Target0, N, A),
	Clauses0 = [( '$bundlehook_do'(Target, Item) :- Body )],
	( hook_decl(Target0, Item, Mod) ->
	    Clauses1 = Clauses0
	;
	    Clauses1 = [('$bundlehook_decl'(Target0, Item) :- true)|Clauses0],
	    assertz_fact(hook_decl(Target0, Item, Mod))
	),
	Clauses = Clauses1.

get_multifile_preds(Clauses, M) :-
	( hook_decl(_, _, M) ->
	    Clauses = [( m_bundlehook_do(M, Item, A) :-
		    M:'$bundlehook_do'(A, Item) )|Clauses1]
	;
	    Clauses = Clauses1
	),
	( hook_decl(_, _, M) ->
	    retractall_fact(hook_decl(_, _, M)),
	    Clauses1 = [( m_bundlehook_decl(M, Item, A) :-
		    M:'$bundlehook_decl'(A, Item) )|Clauses2]
	;
	    Clauses1 = Clauses2
	),
	Clauses2 = [end_of_file].

% TODO: improve (share with other code, generalize, errors, etc.)
gen_third_party(Name, Props, Clauses, Mod) :-
	Props2 = [name|Props],
	gen_third_party_(Props2, Name, Clauses, Mod).

gen_third_party_([], _, [], _).
gen_third_party_([P|Ps], Name, Clauses, Mod) :-
	P =.. [N|Args],
	atom_concat('m_third_party_', N, N2),
	P2 =.. [N2, Name|Args],
	Clauses = [P2|Clauses0],
	gen_third_party_(Ps, Name, Clauses0, Mod).
