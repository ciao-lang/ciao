:- push_prolog_flag(unused_pred_warnings, no).

:- use_module(library(aggregates), [findall/4]).

% TODO:
%  - define af_rt as a module and add meta declaration for '$bf' and '$expand_resolvent'
%  - merge with bf_rt.pl

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
:- pop_prolog_flag(unused_pred_warnings).
