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
defdep((:- bundle_flag(Name, Opts)), Clauses, Mod) :- !,
	get_bundle_name(Mod, M),
	check_bundle_flag_name(Name, M),
	Clauses = [('$bundleconfig_entry'(Name, M, Opts))].
% defdep(Sent, [], _Mod) :- \+ allowed_sent(Sent), !,
%  	message(warning, ['Invalid sentence for bundle hooks: ', Sent]).

% allowed_sent((:- Decl)) :- nonvar(Decl), !,
% 	allowed_decl(Decl).
% allowed_sent((?- _)) :- fail.
% allowed_sent(_). % TODO: not all rules should be allowed
% 
% allowed_decl(doc(_,_)).
% allowed_decl(discontiguous(_)).
% allowed_decl(multifile(_)).
% allowed_decl(include(M)) :- allowed_import(M).
% allowed_decl(use_module(M)) :- allowed_import(M).
% allowed_decl(use_module(M,_)) :- allowed_import(M).
% 
% % TODO: be more precise
% allowed_import(.(_)). % (for local imports) % TODO: temporary?
% allowed_import(library(_)).
% allowed_import(ciaobld(_)).

% ---------------------------------------------------------------------------

% E.g., from 'foo.hools.pl' to 'foo'
get_bundle_name(M1, M) :-
	( atom_concat(M, '.hooks', M1) -> true
	; M = M1
	).

check_bundle_flag_name(Name, _M) :-
	valid_bundle_flag_name(Name), !.
check_bundle_flag_name(Name, M) :-
 	message(error, ['Invalid bundle flag name \'', Name, '\' at \'', M, '\' (it must match [a-z][0-9a-z_]*)']).

% matches [a-z][0-9a-z_]* (so that the name can be exported to other tools)
% TODO: an alternative is to ask for a specific name in such cases
valid_bundle_flag_name(Name) :-
	atom_codes(Name, NameC),
	valid_codes(NameC).

valid_codes([X|Xs]) :- alpha(X), valid_codes_(Xs).

valid_codes_([]).
valid_codes_([X|Xs]) :- ualphanum(X), valid_codes_(Xs).

alpha(X) :- X >= 0'a, X =< 0'z.

ualphanum(0'_) :- !.
ualphanum(X) :- X >= 0'a, X =< 0'z.
ualphanum(X) :- X >= 0'0, X =< 0'9.

% ---------------------------------------------------------------------------

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
	%
	get_bundle_name(M, Bundle),
	Clauses2 = [
          (m_bundle_config_call(Bundle,G) :- call(G)),
	  (m_bundle_config_entry(Bundle,Name,Props) :- '$bundleconfig_entry'(Name,Bundle,Props))
          |Clauses3],
	%
	Clauses3 = [end_of_file].

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
