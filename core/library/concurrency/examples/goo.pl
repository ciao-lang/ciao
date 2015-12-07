:- module(goo, [main/0], []).

:- use_module(library(concurrency)).
:- use_module(library(system)).

:- concurrent goal_solution_ready/1.

goo1(X,Y) :-
	Z = 3,
	Y = X,
	Z = Y,
	X = Z.

goo2(X,Y) :-
	Z = 2,
	Y = X,
	Z = Y,
	X = Z.

main :-
	set_prolog_flag(gc, off),
	eng_call(do, create, create, _),
	G1 = '$:'('goo:goo1'(X1,Y1)),
	G2 = '$:'('goo:goo2'(X2,Y2)),
	and(G1,G2),
	display(G1), nl,
	display(G2), nl,
	Res is X1 + X2 + Y1 + Y2,
	display('Result: '), display(Res), nl.

and(G1,G2) :-
	H = goal_info(_Id,G2),
        publish(G2,H),
        G1,
        read(H).

publish(G2,goal_info(Id,G2)) :-
	eng_push_goal(Id,G2),
	display(eng_push_goal(G2)),nl,
	pause(1).

read(goal_info(Id,G2)) :-
	(
	    eng_goal_solution(Id) -> call(G2)
	;
            retract_fact(goal_solution_ready(Id)),
            display(goal_solution_ready(Id)), nl
	).

do :-
	display(eng_find_goal(Id,Goal)),nl,
	eng_find_goal(Id,Goal),
	start_goal(Id,Goal).

start_goal(Id,Goal) :-
	call(Goal),
	assertz_fact(goal_solution_ready(Id)),
	do.

