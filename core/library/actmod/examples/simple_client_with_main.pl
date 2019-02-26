:- module(simple_client_with_main, [main/1], [actmod]).

% A simple active module client with main/0

:- use_module(simple_server, [population/2, shutdown/0], [active, reg_protocol(filebased)]).

:- use_module(engine(io_basic)).
:- use_module(library(aggregates)).
:- use_module(library(errhandle), [error_protect/2]).

main(_) :- 
	error_protect(doit, abort).

doit :-
	display('Checking connection to server...'), nl,
	( population(_,_) -> display('Server OK.'), nl
	; display('Server failed?!'), nl
	),
	display('Computing population (info obtained from server)...'), nl,
	add_pop(S),
	display('Total population is: '), display(S), nl,
	actmod_cast(simple_server:shutdown).

add_pop(S) :- findall(P,population(_,P),L), sumlist(L,S).

sumlist([],0).
sumlist([X|T],S) :- 
	sumlist(T,S1),
	S is X + S1.
