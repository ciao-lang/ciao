% A simple active module client
%:- module(simple_client,[add_pop/1, population/2, halt_server/0],[actmods]).
:- use_package(actmods).
:- use_module(library(actmods/filebased_locate)).
:- use_active_module(simple_server, [population/2, shutdown/0]).
%% :- use_module(simple_server, [population/2, shutdown/0]).


:- use_module(library(aggregates)).

add_pop(S) :- findall(P,population(_,P),L), sumlist(L,S).

sumlist([],0).
sumlist([X|T],S) :- 
	sumlist(T,S1),
	S is X + S1.

halt_server :- shutdown .
