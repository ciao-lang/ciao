:- module(_, _).

:- use_module(library(freeze)).

% Example from B Prolog

/* sendmoney */
go:-
    go_(Vars, T),
    write(Vars), nl, 
    write('execution time is '),write(T), write(milliseconds).

go_(Vars, T):-
    statistics(runtime,_),
    range(0,9,D),
    vars_constraints(Vars),
    label(Vars,D),
    statistics(runtime,[_,T]).


vars_constraints(Vars):-
    Vars=[S,E,N,D,M,O,R,Y],
    neq(S,0),
    neq(M,0),
    eq(R1,M),
    add(R2,S,M,R1,O),
    add(R3,E,O,R2,N),
    add(R4,N,R,R3,E),
    add(0,D,E,R4,Y),
    Carry = [0,1],
    member(R1,Carry),
    member(R2,Carry),
    member(R3,Carry),
    member(R4,Carry).

%member(X,[X|_]).
%member(X,[_|Xs]):-member(X,Xs).

label([],_).
label([V|Vs],D):-
    myselect(D,Rest,V),
    label(Vs,Rest).

range(N,N,[N]) :- !.
range(M,N,[M|Ns]) :-
	M < N,
	M1 is M+1,
	range(M1,N,Ns).

myselect([X|Xs],Xs,X).
myselect([Y|Ys],[Y|Zs],X) :- myselect(Ys,Zs,X).

neq(X,Y):-freeze(X,X=\=Y).

eq(X,Y):-freeze(X,freeze(Y,X=:=Y)).

add(C,X,Y,NewC,Z):-freeze(C,freeze(X,freeze(Y,freeze(NewC,freeze(Z,C+X+Y=:=10*NewC+Z))))).

