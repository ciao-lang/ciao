:- use_module(library(tcltk)).
:- use_module(library(lists)).

setup :-
	tk_new([name('SICStus+Tcl/Tk - Queens')], Tcl),
	tcl_eval(Tcl, 'source 8-queens.tcl', _),
         tk_next_event(Tcl, Event),
         (   Event = next -> go(Tcl)
         ;   closedown(Tcl)
         ).

closedown(Tcl) :-
	tcl_delete(Tcl).

go(Tcl) :-
	tcl_eval(Tcl, 'clear_board', _),
	queens(8, Qs),
	show_solution(Tcl,Qs),
	tk_next_event(Tcl, Event),
	(   Event = next -> fail
	;   closedown(Tcl)
	).
go(Tcl) :-
	tcl_eval(Tcl, 'disable_next', _),
	tcl_eval(Tcl, 'clear_board', _),
	tk_next_event(Tcl, _Event),
	closedown(Tcl).


						% 8-queens.pl

queens(N, Qs) :-
	range(1, N, Ns),
	do_queens(Ns, [], Qs).

do_queens(UnplacedQs, SafeQs, Qs) :-
	select(Q, UnplacedQs, UnplacedQs1),
	\+ attack(Q, SafeQs),
	do_queens(UnplacedQs1, [Q|SafeQs], Qs).
do_queens([], Qs, Qs).

attack(X, Xs) :- do_attack(X, 1, Xs).

do_attack(X, N, [Y|_Ys]) :- X is Y + N.
do_attack(X, N, [Y|_Ys]) :- X is Y - N.
do_attack(X, N, [_Y|Ys]) :-
	N1 is N + 1,
	do_attack(X, N1, Ys).

range(M, N, [M|Ns]) :-
	M < N,
	M1 is M + 1,
	range(M1, N, Ns).
range(N, N, [N]).

show_solution(Tcl, L) :-
	reverse(L, LR),
	tcl_eval(Tcl, [show_solution, br(LR)], _),
	tk_do_all_events.

tk_do_all_events :-
	tk_do_one_event, !,
	tk_do_all_events.
tk_do_all_events.
