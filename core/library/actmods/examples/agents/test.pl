
:- use_package(actmods).

:- use_module(library(actmods/webbased_locate)).
:- use_active_module(agent1, [agent1/2]).
:- use_active_module(agent2, [agent2/2]).

main:-
	display(1),
	agent1( agent2, add([2,3,4]) ),
	display(2),
	( agent2( i, shutdown) % fails!
	; true ), !,
	display(3),
	( agent1( i, shutdown)
	; true ), !,
	display(4),
	true.
