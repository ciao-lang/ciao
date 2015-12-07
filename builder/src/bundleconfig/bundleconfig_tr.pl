:- module(bundleconfig_tr, [sent/3], [assertions]).

:- data hook_decl/2.

sent(end_of_file, Clauses, M) :-
	get_multifile_preds(Clauses, M).
sent((:- bundle_flag(Name, Opts)), Clauses, Mod) :- !,
	get_bundle_name(Mod, M),
	check_bundle_flag_name(Name, M),
	Clauses = [('$bundleconfig_entry'(Name, M, Opts))].
sent(Sent, [], _Mod) :- \+ allowed_sent(Sent), !,
 	message(warning, ['Invalid sentence for bundle configuration: ', Sent]).

allowed_sent((:- Decl)) :- nonvar(Decl), !,
	allowed_decl(Decl).
allowed_sent((?- _)) :- fail.
allowed_sent(_). % TODO: not all rules should be allowed

allowed_decl(doc(_,_)).
allowed_decl(discontiguous(_)).
allowed_decl(multifile(_)).
allowed_decl(include(M)) :- allowed_import(M).
allowed_decl(use_module(M)) :- allowed_import(M).
allowed_decl(use_module(M,_)) :- allowed_import(M).

% TODO: be more precise
allowed_import(.(_)). % (for local imports) % TODO: temporary?
allowed_import(library(_)).
allowed_import(ciaobld(_)).

get_bundle_name(M1, M) :-
	( atom_concat(M, '.config', M1) -> true
	; M = M1
	).

get_multifile_preds(Clauses, M1) :-
	get_bundle_name(M1, M),
	Clauses = [
          (m_bundle_config_call(M,G) :- call(G)),
	  (m_bundle_config_entry(M,Name,Props) :- '$bundleconfig_entry'(Name,M,Props))
          |Clauses1],
	Clauses1 = [end_of_file].

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

