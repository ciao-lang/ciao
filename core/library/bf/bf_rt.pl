:- use_module(library(aggregates), [findall/4]).

% TODO:
%  - define bf_rt as a module and add meta declaration for '$bf' and '$expand_resolvent'
%  - merge with af_rt.pl

'$bf'(X, Y, _):- X == Y, !, fail. % No (more) solutions.
'$bf'([u([], Goal)|_], _, Goal).
'$bf'([u(Resolvent, U_Goal)|Us], Urest, Goal):-
	'$expand_resolvent'(Resolvent, U_Goal, Urest, NewUrest),
	'$bf'(Us, NewUrest, Goal).

'$expand_resolvent'([], _, X, X).
'$expand_resolvent'([A|Rest], Goal, Us, Us_):-
        ( 
	    '$bfpred'(A) ->
	    aggregates:findall(u(Body,Goal), '$bfcl'(A, Body, Rest), Us, Us_)
        ;
	    aggregates:findall(u(Rest,Goal), A, Us, Us_)
        ).
