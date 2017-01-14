%% Cryptarithmetic puzzle  (? Ref. CLP paper, Proc. 4th ICLP)
%% version for andorra1.7 (i.e. after added parser and new built-ins)

:- module(crypt_and_user,[go/0,test/0],[andorra]).
:- use_module(library(write), [write/1]).



:- determinate( test, true ).

test:-
        solve(A,B,C,D,E,F,G,H),
%	wakeup(_1,_3), to include in the result!!!
        print_out(A,B,C,D,E,F,G,H).

:- determinate( go, true ).

go:- solve(S,E,N,D,M,O,R,Y).

:- determinate( go2(_), true ).

go2(fn(S,E,N,D,M,O,R,Y)):- solve(S,E,N,D,M,O,R,Y),
			print_out(S,E,N,D,M,O,R,Y).

:- determinate( solve(_S,_E,_N,_D,_M,_O,_R,_Y), true ).

solve(S,E,N,D,M,O,R,Y):- 
	M = 1,			%% M must be 1
	carry(C1,C2,C3),
	ganerate(S,E,N,D,M,O,R,Y),
	S > 0,			%% S can't be 0
	const(0,D,E,Y,C1),  	%% D+E = Y+10*C1
	const(C1,N,R,E,C2),	%% C1+N+R = E+10*C2
	const(C2,E,O,N,C3),	%% C2+E+O = N+10*C3
	const(C3,S,M,O,M).	%% C3+S+M = O+10*M


:- determinate(  const(A,B,C,D,E), (
	ground(E), ground(D), ground(B), ground(C) ;
	ground(D), ground(E), ground(A), ground(C) ;
	ground(A), ground(E), ground(D), ground(B) ;
	ground(B), ground(E), ground(A), ground(C) ;
	ground(C), ground(D), ground(A), ground(B) ) ).

const(A,B,C,D,E):-		%% if only A is unbound
	A is 10 * E + D - B - C,!.
const(A,B,C,D,E):-		%% if only B is unbound
	B is 10 * E + D - A - C,!.
const(A,B,C,D,E):-		%% if only C is unbound
	C is 10 * E + D - A - B,!.
const(A,B,C,D,E):-		%% if only D is unbound
	D is C - 10 * E + A + B,!.
const(A,B,C,D,E):-		%% if only E is unbound
	E is (C - D + A + B) / 10.
	
:- determinate( ganerate(_S,_E,_N,_D,_M,_O,_R,_Y), true ).

ganerate(S,E,N,D,M,O,R,Y):- 
	digi(S,1), 
	digi(E,2), 
	digi(N,3), 
	digi(D,4), 
	digi(M,5),
	digi(O,6), 
	digi(R,7), 
	digi(Y,8), 
	difflist([S,E,N,D,M,O,R,Y]).

:- determinate( difflist(A), nonvar(A) ).

difflist([]).
difflist([X|T]):- not_member(X,T), difflist(T).

:- determinate( not_member(_A,B), nonvar(B) ).

not_member( X, [] ).
not_member( X, [Y|L] ):- not_same(X,Y), not_member( X, L ).

:- determinate( not_same(_A,_B), true ).

not_same(X,Y):- X\==Y.

:- determinate( digi(A,_B), nonvar(A) ).

%% should be digi/1, the second argument is only for debugging
digi(9,X).
digi(8,X).
digi(7,X).
digi(6,X).
digi(5,X).
digi(4,X).
digi(3,X).
digi(2,X).
digi(1,X).
digi(0,X).

:- determinate( carry(A,B,C), ( nonvar(A),nonvar(B),nonvar(C) ) ).

carry(1,1,1).
carry(1,1,0).
carry(1,0,1).
carry(1,0,0).
carry(0,1,1).
carry(0,1,0).
carry(0,0,1).
carry(0,0,0).

:- determinate( print_out(_S,_E,_N,_D,_M,_O,_R,_Y), true ).

print_out(S,E,N,D,M,O,R,Y):- 
	write('    SEND'), nl,
     	write(' +  MORE'), nl,
	write('-----------'), nl,
	write('   MONEY'), nl, nl,
	write('The solution is:'), nl, nl,
	write('   '), write(S),write(E),write(N),write(D), nl,
	write(' + '), write(M),write(O),write(R),write(E), nl,
	write('-----------'), nl,
	write('  '),
	write(M),write(O),write(N),write(E),write(Y), nl.
