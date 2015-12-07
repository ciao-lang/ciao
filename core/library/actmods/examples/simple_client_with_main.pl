% A simple active module client with main
%:- module(simple_client_with_main,[main/0],[actmods]).

:- use_package(actmods).
:- use_module(library(actmods/filebased_locate)).
:- use_active_module(simple_server, [population/2, shutdown/0]).

:- use_module(library(aggregates)).
:- use_module(library(errhandle)).

main :- 
	error_protect(doit).

doit :-
	display('Checking connection to server...'), nl,
	population(_,_),
	display('Server OK.'), nl,
	display('Computing population (info obtained from server)...'), nl,
	add_pop(S),
	display('Total population is: '), display(S), nl,
	shutdown.

add_pop(S) :- findall(P,population(_,P),L), sumlist(L,S).

sumlist([],0).
sumlist([X|T],S) :- 
	sumlist(T,S1),
	S is X + S1.
