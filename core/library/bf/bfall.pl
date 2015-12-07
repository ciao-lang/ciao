:- package(bfall).
% (See bf_doc for details about this package)

:- use_module(library(aggregates), [findall/4]).

'$bf'(X, Y, _):- X == Y, !, fail. % No (more) solutions.
'$bf'([u([], Goal)|_], _, Goal).
'$bf'([u(Resolvent, U_Goal)|Us], Urest, Goal):-
	'$expand_resolvent'(Resolvent, U_Goal, Urest, NewUrest),
	'$bf'(Us, NewUrest, Goal).

'$expand_resolvent'([], _, X, X).
'$expand_resolvent'([A|Rest], Goal, Us, Us_):-
        ( '$bfpred'(A) ->
            aggregates:findall(u(Body,Goal), '$bfcl'(A, Body, Rest), Us, Us_)
        ; aggregates:findall(u(Rest,Goal), A, Us, Us_)
        ).

:- discontiguous '$bfcl'/3, '$bfpred'/1.

:- include(library(bf/ops)).

:- load_compilation_module(library(bf/bfalltr)).
:- add_sentence_trans(bfalltr:bfalltr/3, 750). % TODO: Probably not right priority

% TODO: Priorities are not enough to make it work with other
%       translations, such as fsyntax. See "Modular Extensions for
%       Modular (Logic) Languages (LOPSTR'11)" paper for details.
