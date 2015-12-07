:- module(tcl_queens,[main/0, test/0],[assertions,isomodes,regtypes]).

:- use_module(library(tcltk)).
:- use_module(library(lists)).

:-export(queens/2).

queens(N,Qs) :-
        range(1,N,Ns),
        do_queens(Ns,[],Qs).

do_queens(UnplacedQs, SafeQs, Qs) :-
        select(Q,UnplacedQs, UnplacedQs1),
        \+ attack(Q,SafeQs),
        do_queens(UnplacedQs1,[Q|SafeQs],Qs).
do_queens([], Qs, Qs).

attack(X,Xs) :- do_attack(X, 1, Xs).

do_attack(X,N,[Y|_Ys]) :- X is Y + N.
do_attack(X,N,[Y|_Ys]) :- X is Y - N.
do_attack(X,N,[_Y|Ys]) :- 
        N1 is N + 1,
        do_attack(X,N1,Ys).

range(M,N,[M|Ns]) :-
        M < N,
        M1 is M + 1,
        range(M1,N,Ns).

range(N, N,[N]).

main:- test.

test :- 
        tk_new([name('Ciao+TclTk - Queens')],I),
%       tcl_new(I),
        tcl_eval(I,'source 8-queens.tcl',_),
%       tcl_event(I,[prolog_event,dq(write(next))],_),
%       tcl_event(I,[prolog_event,dq(write(next))],_),
%       tcl_event(I,[prolog_event,dq(write(next))],_),
%       tcl_event(I,[prolog_event,dq(write(quit))],H),
%       display(H),
        tk_next_event(I,Event),
%       display(Event),
%       display('En el primero*****'),nl,
%       display(Event),nl,
        ( Event = next -> go(I)
          ; closedown(I)
        ).

closedown(X) :- tcl_delete(X).


go(I) :- 
%       display('Queens'),nl,
        tcl_eval(I,'clear_board',_),
        queens(8,Qs),
        display(Qs),nl,
        show_solution(I,Qs),
        tk_next_event(I,Event),
%       display('En el segundo*****'),display(Event),nl,
        ( Event = next -> fail
          ; closedown(I)
        ).
go(I) :- 
        tcl_eval(I,'disable_next',_),
        tcl_eval(I,'clear_board',_),
        tk_next_event(I,_Event),
        closedown(I).

show_solution(I,L) :-
        reverse(L,LR),
        tcl_eval(I,[show_solution,br(LR)],_).
