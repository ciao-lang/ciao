:- module(yap_compat_tr, [], []).
:- include(library(dialect/yap_compat_ops)).

% Yap compatibility mode for Ciao
% ---------------------------------------------------------------------------
%
% - Support for (:- meta_predicate A,B,C).
% - Supports as/2 module specs in use_module and reexport.
%
% TODO:
%   - Look at diffs.txt, it contains some changes that were not possible 
%     to do automatically. 
%     (missing exports, and pred(N) in meta_predicate)
%   - Transform it into a package
%
% Jose F. Morales
% Thu Feb 12 13:48:26 CET 2009

:- use_module(library(aggregates), [findall/3]).

% wrap_pred(FromM, FromN, A, ToN, Visibility, M): Predicate FromM:FromF/A is wrapped in module M as ToN/A
%   Visibility = 'public' if it is reexported
%   Visiblity = 'private' if it is not reexported
:- data wrap_pred/6.

% yap_compat_sentence(S0, S, Module): translates S0 into S
:- export(yap_compat_sentence/3).

cleanup :-
	retractall_fact(wrap_pred(_,_,_,_,_,_)).

% yap_compat_sentence(0, _, Mod) :- !, % no need for initialization
yap_compat_sentence(end_of_file, Clauses, Mod) :- !,
	add_wrap_preds(Mod, Clauses, [end_of_file]),
	cleanup.
yap_compat_sentence((?- _), _, _) :- !, fail.
yap_compat_sentence((:- Decl), R, Mod) :- !,
        decl(Decl, R, Mod).

% modspec_to_mod(+X, -Z)
modspec_to_mod(X, Z) :- functor(X, _, 1), !,
	arg(1, X, Y), modspec_to_mod(Y, Z).
modspec_to_mod(X, X).

% Treat declarations
decl(use_module(FromMod, List), R, Mod) :- valid_list(List), !,
	collect_as(List, FromMod, private, Mod, List2),
%	display(nr(List, List2)), nl,
	R = (:- use_module(FromMod, List2)).
decl(reexport(FromMod, List), R, Mod) :- valid_list(List), !,
	collect_as(List, FromMod, public, Mod, List2),
	R = (:- reexport(FromMod, List2)).
decl(meta_predicate(Preds), R, _Mod) :- nonvar(Preds), Preds = (_, _), !,
	flat_meta_predicate(Preds, R, []).

flat_meta_predicate(A, _R, _R0) :- var(A), !, fail.
flat_meta_predicate((A, B), R, R0) :- !,
	flat_meta_predicate(A, R, R1),
	flat_meta_predicate(B, R1, R0).
flat_meta_predicate(A, R, R0) :-
	R = [(:- meta_predicate(A))|R0].

% valid_list(+List)
valid_list(X) :- var(X), !, fail.
valid_list([]).
valid_list([_|Xs]) :- valid_list(Xs).

% collect_as(+Specs0, +Visibility, +Mod, -Specs)
collect_as([], _, _, _, []). 
collect_as([Spec|Specs0], FromM, Visibility, Mod, Specs) :-
	as_spec(Spec, FromN, A, ToN), !,
	assertz_fact(wrap_pred(FromM, FromN, A, ToN, Visibility, Mod)),
	collect_as(Specs0, FromM, Visibility, Mod, Specs). 
collect_as([Spec|Specs0], FromM, Visibility, Mod, [Spec|Specs]) :-
	collect_as(Specs0, FromM, Visibility, Mod, Specs). 

% as_spec(+Spec, -FromN, -FromA, -ToN)
as_spec(Spec, FromN, FromA, ToN) :-
	nonvar(Spec),
	Spec = (FromNA as ToN),
	nonvar(FromNA),
	FromNA = FromN/FromA.

% add_wrap_preds(+Mod, +Clauses, +Clauses0)
add_wrap_preds(Mod, Clauses, Clauses0) :-
	findall(wrap_pred(FromM, FromN, A, ToN, Visibility),
	        wrap_pred(FromM, FromN, A, ToN, Visibility, Mod), Ps),
%	display(ps(Ps)), nl,
	add_wrap_preds__2(Ps, Mod, Clauses, Clauses0).

add_wrap_preds__2([], _, Clauses, Clauses).
add_wrap_preds__2([P|Ps], Mod, Clauses, Clauses0) :-
%	display(P), nl,
	add_wrap_preds__3(P, Mod, Clauses, Clauses1),
	add_wrap_preds__2(Ps, Mod, Clauses1, Clauses0).

add_wrap_preds__3(wrap_pred(FromM, FromN, A, ToN, Visibility), _Mod, Clauses, Clauses0) :-
	functor(From, FromN, A),
	From =.. [_|Args],
	To =.. [ToN|Args],
	( Visibility = public ->
	    Clauses = [(:- export(ToN/A))|Clauses1]
	; Clauses = Clauses1
	),
	modspec_to_mod(FromM, FromM2),
	Clauses1 = [(:- use_module(FromM, [FromN/A])), (To :- FromM2:From)|Clauses0].
%	display(Clauses), nl.

% ---------------------------------------------------------------------------

%:- export(yap_compat_goal/2).
%yap_compat_goal(..., ...)
