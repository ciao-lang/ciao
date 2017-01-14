:- module(money_and_user,[test/0,do/0],[andorra]).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(write), [write/1]).



:- determinate(test,true).

test:-
	solve(A,B,C,D,E,F,G,H),
        print_out(A,B,C,D,E,F,G,H).

do :-
        solve(A,B,C,D,E,F,G,H).

:- determinate(difflist(A),nonvar(A)).

difflist([A|B]) :-
        not_el(A,B),
        difflist(B).
difflist([]).

:- determinate(digi(A),nonvar(A)).

digi(9).
digi(8).
digi(7).
digi(6).
digi(5).
digi(4).
digi(3).
digi(2).
digi(1).
digi(0).


go(fn(A,B,C,D,E,F,G,H)) :-
        solve(A,B,C,D,E,F,G,H),
        print_out(A,B,C,D,E,F,G,H).


:- determinate( not_el(_X,A), nonvar(A) ).

not_el(A,[]).
not_el(A,[B|C]) :-
        A\==B,
        not_el(A,C).

:- determinate( carry(A,B,C), ( nonvar(A), nonvar(B), nonvar(C) ) ).

carry(1,1,1).
carry(1,1,0).
carry(1,0,1).
carry(1,0,0).
carry(0,1,1).
carry(0,1,0).
carry(0,0,1).
carry(0,0,0).

:- determinate(generate(_A,_B,_C,_D,_E,_F,_G,_H),true).

generate(A,B,C,D,E,F,G,H) :-
        digi(E),
        digi(A),
        digi(F),
        digi(B),
        digi(C),
        digi(G),
        digi(D),
        digi(H),
        difflist([A,B,C,D,E,F,G,H]).

:- determinate(print_out(_A,_B,_C,_D,_E,_F,_G,_H),true).

print_out(A,B,C,D,E,F,G,H) :-
        write('    SEND'),
        nl,
        write(' +  MORE'),
        nl,
        write(-----------),
        nl,
        write('   MONEY'),
        nl,
        nl,
        write('The solution is:'),
        nl,
        nl,
        write('   '),
        write(A),
        write(B),
        write(C),
        write(D),
        nl,
        write(' + '),
        write(E),
        write(F),
        write(G),
        write(B),
        nl,
        write(-----------),
        nl,
        write('  '),
        write(E),
        write(F),
        write(C),
        write(B),
        write(H),
        nl.

:- determinate(solve(_A,_B,_C,_D,_E,_F,_G,_H),true).

solve(A,B,C,D,E,F,G,H) :-
        carry(I,J,K),
        E=1,
        generate(A,B,C,D,E,F,G,H),
        A>0,
        L is D+B,
        M is H+10*I,
        L=M,
        N is I+C+G,
        O is B+10*J,
        N=O,
        P is J+B+F,
        Q is C+10*K,
        P=Q,
        R is K+A+E,
        S is F+10*E,
        R=S.

%%%%%%%%%%%%%%%%%%%%%

ourmain:-
	statistics(runtime,_),
	ourdo,
	statistics(runtime,[_,T1]),
        write(T1).

%:- determinate(ourdo,true).

ourdo:- solve(_A,_B,_C,_D,_E,_F,_G,_H), fail.
ourdo.
