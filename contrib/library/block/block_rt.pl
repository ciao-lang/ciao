:- module(block_rt, ['$block'/2], [assertions]).
:- use_module(library(freeze)).

:- meta_predicate('$block'(?, goal)).

'$block'([], G):-call(G).
'$block'([H|T], G):-
	(
	    vars(H) ->
	    or(H, S), 
	    freeze:freeze(S, '$block'(T, G))
	;
	    '$block'(T, G)
	).

vars([]).
vars([H|T]):- var(H), vars(T).

or([], _).
or([H|T], S):-
	freeze:freeze(H, S=0), 
	or(T, S).




	    
