:- module(dcg_tr, [
	dcg_translation/3,
	dcg_translation_goal/3,
	dcg_translate_dcg/5
	], 
	[assertions, nortchecks,
%	 'compiler/callback',
	 isomodes]).

:- use_module(library(terms), [copy_args/3]).

:- doc(title,"Definite Clause Grammars (expansion module)").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Jose F. Morales (support for phrase/{2,3})").

:- doc(module, "This module implements the translation for Definite
   Clause Grammars (DCGs).").

:- include(library(dcg/dcg_ops)).

:- doc(dcg_translation/2,"Performs the code expansion of source
	clauses that use DCGs.").

dcg_translation(T1, T3, M) :-
        dcg_expansion_internal(T1, M, T2),
        T2 = T3.

dcg_expansion_internal((H,List-->B), M, (H1:-B2,B1)) :- !,
	dcg_translate_dcg_atom(H, H1, S0, S1),
	dcg_translate_dcg(List, B2, M, S1, S),
	dcg_translate_dcg(B, B1, M, S0, S).
dcg_expansion_internal((H-->B), M, (H1:-B1)) :-
	dcg_translate_dcg_atom(H, H1, S0, S),
	dcg_translate_dcg(B, B1, M, S0, S).

dcg_translate_dcg(X, Y, M, S0, S) :-
	dcg_translate_dcg_(X, Y0, M, Tail, S0, S),
	(   Tail\==S0 -> S=Tail, dcg_connect(X, Y0, Y)
	;   dcg_connect2(X, Y0, S=Tail, Y)
	).

dcg_connect((_->_), X, X) :- X = (_->_), !.
dcg_connect(_, (P->Q), ((P->Q),true)) :- !.
dcg_connect(_, X, X).

dcg_connect2((_->_), (P->Q0), Eq, (P->Q)) :- !, dcg_and(Q0, Eq, Q).
dcg_connect2(_, (P->Q), Eq, ((P->Q),Eq)) :- !.
dcg_connect2(_, X0, Eq, X) :- dcg_and(X0, Eq, X).

dcg_translate_dcg_(X, Y, _M, S, S0, S) :- var(X), !,
	% Make sure that dcg_phrase is used
	% TODO: The error message does not explain that we have to use
	%       another package
	Y = phrase(X,S0,S).
	% TODO: add_module_check/1 does not work properly, fix
        % add_module_check(ensure_imported(dcg_phrase_rt, phrase, 3)).
dcg_translate_dcg_([], true, _M, S0, S0, _) :- !.
dcg_translate_dcg_([X|Y], XandY, M, Tail, S0, S) :- !,
	dcg_translate_dcg_(Y, Y1, M, Tail, S1, S),
	dcg_and('C'(S0,X,S1), Y1, XandY).
dcg_translate_dcg_(\+X, (X1 -> fail; S=S0), M, S, S0, S) :- !,
	dcg_translate_dcg_(X, X1, M, S1, S0, S1).
dcg_translate_dcg_((X,Y), XandY, M, Tail, S0, S) :- !,
	dcg_translate_dcg_(X, X1, M, S1, S0, S1),
	dcg_translate_dcg_(Y, Y1, M, Tail, S1, S),
	dcg_and(X1, Y1, XandY).
dcg_translate_dcg_((X->Y), (X1->Y1), M, Tail, S0, S) :- !,
	dcg_translate_dcg_(X, X1, M, S1, S0, S1),
	dcg_translate_dcg_(Y, Y1, M, Tail, S1, S).
dcg_translate_dcg_(if(X,Y,Z), if(X1,Y1,Z1), M, S, S0, S) :- !,
	dcg_translate_dcg(X, X1, M, S0, S1),
	dcg_translate_dcg(Y, Y1, M, S1, S),
	dcg_translate_dcg(Z, Z1, M, S1, S).
dcg_translate_dcg_((X;Y), (X1;Y1), M, S, S0, S) :- !,
	dcg_translate_dcg(X, X1, M, S0, S),
	dcg_translate_dcg(Y, Y1, M, S0, S).
dcg_translate_dcg_('|'(X,Y), (X1;Y1), M, S, S0, S) :- !,
	dcg_translate_dcg(X, X1, M, S0, S),
	dcg_translate_dcg(Y, Y1, M, S0, S).
dcg_translate_dcg_(!, !, _M, S0, S0, _) :- !.
dcg_translate_dcg_({G}, call(G), _M, S0, S0, _) :- var(G), !.
dcg_translate_dcg_({G}, G, _M, S0, S0, _) :- !.
% Note:
%   We only allow module qualification of atomic goals in DCGs,
%   to mimic the behaviour of the Ciao module system.
%
%   This introduces an incompatibility with DCG expansion in Prolog
%   systems such as SICStus and SWI Prolog. Those systems propagated
%   module qualification through control structures (e.g. M:(a,b) ===>
%   M:a, M:b). If useful, we could add (e.g., as a parameter) that
%   behaviour to our module system.
%   
dcg_translate_dcg_((Mod:X), MX1, M, S, S0, S) :- nonvar(X), !,
	MX1 = (Mod:X1),
	% TODO: Use is_control(X) to emit a or error warning in case
	%   case that a control predicate is used? (or expect
	%   'M:,'/4).
	dcg_translate_dcg_(X, X1, M, S, S0, S).
dcg_translate_dcg_(X, X1, _M, S, S0, S) :-
	dcg_translate_dcg_atom(X, X1, S0, S).

dcg_and(X, Y, Z) :- X==true, !, Z=Y.
dcg_and(X, Y, Z) :- Y==true, !, Z=X.
dcg_and(X, Y, (X,Y)).

% TODO: Enable to control qualification errors
% is_control(\+ _).
% is_control((_,_)).
% is_control((_->_)).
% is_control((_;_)).
% is_control('|'(_,_)).
% is_control(!).
% is_control(if(_,_,_)).

dcg_translate_dcg_atom(X, X1, S0, S) :-
	functor(X, F, A),
	A1 is A+1,
	A2 is A+2,
	functor(X1, F, A2),
	arg(A1, X1, S0),
	arg(A2, X1, S),
	copy_args(A, X, X1).

% ---------------------------------------------------------------------------
% Translate goals (for phrase/2, phrase/3)

% TODO: missing check for module of phrase (use M for that)
dcg_translation_goal(phrase(X, S), G, M) :- nonvar(X),
	dcg_translate_dcg(X, G, M, S, []).
dcg_translation_goal(phrase(X, S, S0), G, M) :- nonvar(X),
	dcg_translate_dcg(X, G, M, S, S0).
