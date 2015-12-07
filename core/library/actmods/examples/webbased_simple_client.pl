% A simple active module client connects to server via Web
:- module(webbased_simple_client,[add_pop/1, population/2],[actmods]).

:- use_module(library(actmods/webbased_locate)).
:- use_active_module(simple_server, [population/2]).


:- use_module(library(aggregates)).

add_pop(S) :- findall(P,population(_,P),L), sumlist(L,S).

sumlist([],0).
sumlist([X|T],S) :- 
	sumlist(T,S1),
	S is X + S1.
