:- module(webbased_simple_client, [add_pop/1, population/2], [actmod]).

% A simple active module client that connects to server via Web

:- use_module(simple_server, [population/2], [active, reg_protocol(webbased)]).

:- use_module(library(aggregates)).

add_pop(S) :- findall(P,population(_,P),L), sumlist(L,S).

sumlist([],0).
sumlist([X|T],S) :- 
    sumlist(T,S1),
    S is X + S1.
