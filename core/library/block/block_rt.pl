:- module(block_rt, ['$block'/2], [assertions]).

% This is the runtime module for block declarations. It defines the
% '$block'/2 predicate, which is called from wrappers introduced by
% the block translation module.

:- use_module(engine(hiord_rt), [call/1]).
:- use_module(library(freeze)).

:- meta_predicate('$block'(?, goal)).

% Given a list of list of variables:
%   [[A1,A2,...],
%    [B1,B2,...],
%    ...]
% suspend the execution of G while the formula:
%   (var(A1),var(A2),...
%   ;var(B1),var(B2),...
%   ;...)
% holds.
%
% The evaluation of the condition is delayed using freeze/2.

'$block'([], G):-call(G).
'$block'([H|T], G):-
	( vars(H) ->
	    or(H, S), 
	    freeze:freeze(S, '$block'(T, G))
	; '$block'(T, G)
	).

vars([]).
vars([H|T]):- var(H), vars(T).

or([], _).
or([H|T], S):-
	freeze:freeze(H, S=0), 
	or(T, S).




	    
