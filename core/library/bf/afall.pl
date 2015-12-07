:- package(afall).
% (See bf_doc for details about this package)

:- use_module(library(aggregates), [findall/4]).

'$bf'(X, Y, _):- X == Y, !, fail. % No (more) solutions.
'$bf'([u(Resolvent, ResTail, Goal)|_], _, Goal) :- ResTail == Resolvent.
'$bf'([u(Resolvent, ResTail, U_Goal)|Us], Urest, Goal):-
	'$expand_resolvent'(Resolvent, ResTail, U_Goal, Urest, NewUrest),
	'$bf'(Us, NewUrest, Goal).

'$expand_resolvent'(Rest, RestTail, _, X, Y) :-
        Rest == RestTail, !, X = Y.
'$expand_resolvent'([A|Rest], Rest_, Goal, Us, Us_):-
        ( '$bfpred'(A) ->
            aggregates:findall(u(Rest,T,Goal), '$bfcl'(A, Rest_, T), Us, Us_)
        ; aggregates:findall(u(Rest,Rest_,Goal), A, Us, Us_)
        ).

:- discontiguous '$bfcl'/3, '$bfpred'/1.

:- include(library(bf/ops)).

:- load_compilation_module(library(bf/afalltr)).
:- add_sentence_trans(afalltr:afalltr/3, 750). % TODO: Probably not right priority

% TODO: Priorities are not enough to make it work with other
%       translations, such as fsyntax. See "Modular Extensions for
%       Modular (Logic) Languages (LOPSTR'11)" paper for details.
