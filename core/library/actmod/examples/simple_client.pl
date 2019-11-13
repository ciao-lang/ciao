:- module(simple_client, [add_pop/1, population/2, halt_server/0], [actmod]).

% A simple active module client

:- use_module(simple_server, [population/2, shutdown/0], [active, reg_protocol(filebased)]).
% :- use_module(simple_server, []). % (force actmod in same process) % TODO: add option for this
% :- use_module(simple_server, [population/2, shutdown/0]). % (no actmod)

:- use_module(library(aggregates)).

add_pop(S) :- findall(P,population(_,P),L), sumlist(L,S).

sumlist([],0).
sumlist([X|T],S) :- 
    sumlist(T,S1),
    S is X + S1.

halt_server :-
    actmod_cast(simple_server:shutdown).

