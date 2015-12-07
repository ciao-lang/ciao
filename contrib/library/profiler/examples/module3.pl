:- module(module3, _, [profiler]).

:- use_module(module1).
:- use_module(module2).

:- cost_center main3/1.

main3([A, B]) :-
	module1:pp(A),
	module2:pp(B).
