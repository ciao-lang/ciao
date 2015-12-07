%:- use_package(debug).
:- use_package(fd).
%:- use_package('/home/clip/Systems/ciao/library/fd').
:- use_module(library(lists)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).
:- use_module(library(write)).
:- use_module(library(aggregates)).
:- use_module(library(system)).
%:- use_module(library(concurrency)).

main0(X,Y) :-
	X .>. 5,
	Y .<. 7,
	X + Y .=. 12,
	labeling([X,Y]).

main1(A,B,C,D,E,F):- 
	10 in -1..10,
	C in 5..12,
	A in 7..15,
	B in C..A,
  	B .>=. A,
  	C .>=. B,
  	D in 7..13,
  	E in 3..8,
 	E in dom(D),
 	F in min(D)..max(B),
  	F .=. E + 1,
	labeling([A,B,C,D,E,F]).

main2(A, B) :-
	B in 13 .. 18,
	A in 12..16,
	B in min(A) .. max(B),
	A in min(B) .. B.

main3(B,A):- 
	B in 7..14,
	A in 11..16,
	B .>=. A,
	A in 12..13,
	labeling([A,B]).

main4(A,B) :-
	A in 15 .. 18,
	B in 13 .. 24,
	B .=. A - 4,
%	A = 18.
	labeling([A,B]).
% 	findall(solution(A,B), labeling([A,B]), L),
% 	display(L).

main5(A,B) :-
	B in 13 .. 18,
	A in 12 .. B ,
	labeling([A,B]).

main6(A, B, C) :-
	B in 13 .. 18,
	A in 12..16,
	C in 14..20 .&. 25..30,
	B .>=. C,
 	B in min(A) .. max(A),
 	A in max(B) .. max(B).

main7(A, V) :-
	A in 5 .. 7 .&. 10 .. 100,
	V in 6..12 .&. 16..19,
	X in 14 .. 200,
	X .=. 2*V,
%	A in dom(V).
	A = V.
%	V = 10.

main8(A, B) :-
	A in 18 .. 20 .&. 23 .. 28 .&. 29 .. 37,
	B in 19 .. 21 .&. 27 .. 32, 
 	A + 1 .=. B,
	labeling([A,B]).

main9(A, B, C) :-
	A in 17 .. 19 .&. 20 .. 22,
	B in 19 .. 21 .&. 23..24,
  	C in A .. B,
   	A .=<. B,
	labeling([A,B,C]).

main10(A, B, C) :-
	A in 0 .. 100,
	B in 0 .. 1/0,
	C in 0 .. 100,
	A .=<. B,
	B .=<. C, 
	C .=<. A.
% 	A = 1 .

main11(A, B, C) :-
	A in 0 .. 100,
	B in 0 .. 100,
	C in 0 .. 100,
	C .=. A + B,
	labeling([A,B,C]).

main12(A):-
	A in 0 .. 10 .&. 20 .. 24,
	A .=. A + 5. % Of course, this fails.

main13(A, B, C):-
	A in 5 .. 10,
	B in 3 .. 8, 
	C in 0 .. 20, 
	A .=<. B,
	C .=. A + B,
	A .>. 6,
	labeling([A, B, C]).
%	labeling(A).

main14(A, B, C) :-
	A in 0..9,
%	B in 0..19,
%	B in -1/0 .. 1/0,
	C in 0 .. 1/0,
	B .=. 10 * A,
%	mult(B, 10, A). %, A = 1.
 	A + B .=. C,
	labeling([A,B,C]).
%	C = 11.
%	A .>=. C.

main15(A, B, C, D) :-
	A in 0..9,
	B in 0..9,
	C in 0 .. 1/0,
	D in 0 .. 14,
	C .>=. D,
	A + B .=. 8. % C ,
%  	C = 8.

main16(A,B,C,D) :-
	A in 0 .. 16,
	B in 0 .. 12,
	C in 7 .. 12 .&. 15 .. 17,
	D in 0 .. 100,
	different_fd([A,B,C,D]),
	A .=. 2*B,
	A .>. B,
	D .=. 3 * A,
	C .=. A + B,
 	labeling([A,B,C,D]).

main17(A, B, C) :-
	A in 0 .. 10,
	B in 0 .. 10,
	C in 30 .. 50,
	A .>. 5,
 	A .<>. B,
 	A .<>. C.

main18(X, Y):-
	[X, Y] in 0..2 .&. 7..9,
        X .=. Y, 
	X = 8.

main19(A, B):- 
	A in 11..16,
%	B in 7..14,
	B .>=. A.

main20(X, Y) :-
	X in 0 .. 1,
	Y in 1 .. 2,
        X .=. Y.

main21(X, Y) :-
	X in 0 .. 1,
	Y in 1 .. 2,
        X .<>. Y,
	force_fail(X).

force_fail(X) :-
	X = 1,
	fail.
force_fail(_).

main22(A, B, C) :-
	[A,B] in 0 .. 7,
	 C in 2 .. 20,
	 A .=. 2 * B,
	 C .=. 3 * A,
	 labeling([A,B,C]).	 
	 

smm([S, E, N, D, M, O, R, Y]) :-	
	statistics(walltime,_),
%	display('0'), nl,
	do_smm(S, E, N, D, M, O, R, Y),
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

do_smm(S, E, N, D, M, O, R, Y) :-
	SMM = [S, E, N, D, M, O, R, Y],
	SMM in 0 .. 9,
	different_fd(SMM),
%	display(1), nl,
	M .>=. 1,
	S .>=. 1,
	S1 .=. 1000 * S,
	E1 .=. 100 * E,
 	N1 .=. 10 * N,
 	M1 .=. 1000 * M,
 	O1 .=. 100 * O,
 	O2 .=. 1000 * O,
	R1 .=. 10 * R,
 	M2 .=. 10000 * M,
	N2 .=. 100 * N,
	E2 .=. 10 * E,

%	display(2), nl,

	S1E1 .=. S1 + E1,
	N1D .=. N1 + D,
	M1O1 .=. M1 + O1,
	R1E .=. R1 + E,
	S1E1N1D .=. S1E1 + N1D,
 	M1O1R1E .=. M1O1 + R1E,
 	SUM1 .=. S1E1N1D + M1O1R1E,

%	display(3), nl,

 	M2O2 .=. M2 + O2,
 	N2E2 .=. N2 + E2,
 	M2O2N2E2 .=. M2O2 + N2E2,

%	display(4), nl,

        SUM1 .=. M2O2N2E2 + Y,

%	display(5), nl,

%	display('Vamos a entrar en el apasionante mundo del labeling...'), nl,
%	S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8, Y = 2.
%	display('1'), nl,
%	statistics(runtime,_),
	labeling([S, E, N, D, M, O, R, Y]).
%	statistics(runtime,[_, Time]),
%	format("Used ~d milliseconds~n", Time).
%	labeling([E, N, D, Y, M, S, O, R]).
%	all_different([S, E, N, D, M, O, R, Y]).

dgr_fd(DGR) :-	
	statistics(walltime,_),
	do_dgr_fd(DGR),
	statistics(walltime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

do_dgr_fd(DGR) :-	
        DGR = [D,G,R,O,E,N,B,A,L,T],
%	[Sum1, Sum2, Sum3] in 0 .. 999999,
        DGR in 0..9,
	different_fd(DGR),
	sum(D, O, N, A, L, D, Sum1),
%  	display(Sum1), nl,
%  	Sum1 = 526485,
   	sum(G, E, R, A, L, D, Sum2),
%  	display(Sum2), nl,
% 	Sum2 = 197485,
  	sum(R, O, B, E, R, T, Sum3),
%  	display(Sum3), nl,
	Sum3 .=. Sum1 + Sum2,
%	sum(Sum3, Sum1, Sum2),
%	Sum3 = 723970,
%	different_fd(DGR),	
	display('Vamos a entrar en el apasionante mundo del labeling...'), nl,
	labeling(DGR).

sum(A, B, C, D, E, F, S):-
        A .>=. 1,
%	mult(A6, 100000, A),
	A6 .=. 100000 * A,
%	mult(A5, 10000, B),
        A5 .=. 10000 * B,
%	mult(A4, 1000, C),
	A4 .=. 1000 * C,
%	mult(A3, 100, D),
	A3 .=. 100 * D,
%	mult(A2, 10, E),
	A2 .=. 10 * E,
	R1 .=. A2 + F,
%	sum(R1, A2, F),
	R2 .=. A3 + R1,
%	sum(R2, A3, R1),
	R3 .=. A4 + R2,
%	sum(R3, A4, R2),
	R4 .=. A5 + R3,
%	sum(R4, A5, R3),
	S  .=. A6 + R4.
%	sum(S, A6, R4).

do_dgr_fd2(DGR) :-	
        DGR = [D,G,R,O,E,N,B,A,L,T],
        DGR in 0..9,
	different_fd(DGR),
	D .>. 0, 
	G .>. 0, 
	R .>. 0, 
	D1 .=. 100000 * D,
%	mult(D1, 100000, D),
	O1 .=. 10000 * O,
	N1 .=. 1000 * N,
	A1 .=. 100 * A,
	L1 .=. 10 * L,
	D1O1 .=. D1 + O1,
	D1O1N1 .=. D1O1 + N1,
	L1D .=. L1 + D,
	A1L1D .=. A1 + L1D,
	D1O1N1A1L1D .=. D1O1N1 + A1L1D,

	G1 .=. 100000 * G,
	E1 .=. 10000 * E,
	R1 .=. 1000 * R,
	G1E1 .=. G1 + E1,
	G1E1R1 .=. G1E1 + R1,
	G1E1R1A1L1D .=. G1E1R1 + A1L1D,

	R2 .=. 100000 * R,
	B1 .=. 1000 * B,
	E2 .=. 100 * E,
	R3 .=. 10 * R,
	R2O1 .=. R2 + O1,
	B1E2 .=. B1 + E2,
	R3T .=. R3 + T,
	R2O1B1E2 .=. R2O1 + B1E2,	
	R2O1B1E2R3T .=. R2O1B1E2 + R3T,

	R2O1B1E2R3T .=. D1O1N1A1L1D + G1E1R1A1L1D,

   	labeling(DGR).
%   	all_different(DGR).

sum_test(L):-
	L1 = [A, B, C, D, E, F], 
	L1 in 0..9,
	append(L1, [S], L),
       S in 0..1000000,
	different_fd(L),
        A .>=. 1,
%	mult(A6, 100000, A),
	A6 .=. 100000 * A,
%	mult(A5, 10000, B),
        A5 .=. 10000 * B,
%	mult(A4, 1000, C),
	A4 .=. 1000 * C,
%	mult(A3, 100, D),
	A3 .=. 100 * D,
%	mult(A2, 10, E),
	A2 .=. 10 * E,
	R1 .=. A2 + F,
%	sum(R1, A2, F),
	R2 .=. A3 + R1,
%	sum(R2, A3, R1),
	R3 .=. A4 + R2,
%	sum(R3, A4, R2),
	R4 .=. A5 + R3,
%	sum(R4, A5, R3),
	S  .=. A6 + R4,
%	sum(S, A6, R4), 
	labeling(L).

mult_test(L):-
	L = [A,A2,A3,A4,A5,A6],
	different_fd(L),
        A in 0 .. 9,
	A2 .=. 10 * A,
	A3 .=. 100 * A2,
 	A4 .=. 1000 * A3,
        A5 .=. 10000 * A4,
 	A6 .=. 100000 * A5,
	labeling(L).


different_fd([]) .
different_fd([X|Xs]) :-
	different_fd_aux(X, Xs),
	different_fd(Xs).

different_fd_aux(_,[]) .
different_fd_aux(V, [X|Xs]) :-
	V .<>. X,
	different_fd_aux(V, Xs).

% all_different([V|Vs]) :-
% 	Vs = [V1|_],
% 	V .<>. V1, 
% 	all_different(Vs) .
% all_different([_]) .

% all_different([]) .
% all_different([V|Vs]) :-
% % 	list_of_different_numbers(Vs, V),	
% 	nocontainsx(Vs, V),	
%  	all_different(Vs) .

list_of_different_numbers([],_).
list_of_different_numbers([X|Xs], V) :-
	
	A is X/V,
	display(A), nl,
	((A == 1.0; A == 0.Nan) ->
	 fail
	;
	 list_of_different_numbers(Xs, V)).

queens(N, Qs) :-
	statistics(runtime,_),
	do_queens(N, Qs),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).

do_queens(N, Qs):- 
	constrain_values(N, N, Qs),
	different_fd(Qs),!,
	labeling(Qs).
%	all_different(Qs).

constrain_values(0, _N, []).
constrain_values(N, Range, [X|Xs]):-
        N > 0, 
        X in 1 .. Range,
        N1 is N - 1,
        constrain_values(N1, Range, Xs),
        no_attack(Xs, X, 1).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
	Nb1 is Nb + 1,
	no_attack(Ys, Queen, Nb1),
	YNb .=. Y + Nb,
	YNNb .=. Y - Nb,
	Queen .<>. YNb,
	Queen .<>. YNNb.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_queens(N, M, Qs) :-
	statistics(runtime,_),
	findnsols(M, do_queens(N, Qs),do_queens(N, Qs),L),
	display(L), nl,
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_queens(N, Qs) :-
	statistics(runtime,_),
	do_allqueens(N, Qs),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", Time).
    
do_allqueens(N, Qs):-
	do_queens(N, Qs),
	display(Qs), nl,
	fail.
do_allqueens(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Alpha                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_alpha:-    
    statistics(runtime,_),
    top_alpha,
    statistics(runtime,[_,Y]), 
    display('time : '), display(Y), nl.

top_alpha:-
    alpha(LD),
    display(LD), nl.

alpha(LD):-
        LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],
        LD in 1..26,	

        different_fd(LD),

        sum_alpha([B,A,L,L,E,T], 45),
        sum_alpha([C,E,L,L,O], 43),
        sum_alpha([C,O,N,C,E,R,T], 74),
        sum_alpha([F,L,U,T,E], 30),
        sum_alpha([F,U,G,U,E], 50),
        sum_alpha([G,L,E,E], 66),
	 sum_alpha([J,A,Z,Z], 58),
	 sum_alpha([L,Y,R,E], 47),
	 sum_alpha([O,B,O,E], 53),
        sum_alpha([O,P,E,R,A], 65),
        sum_alpha([P,O,L,K,A], 59),
        sum_alpha([Q,U,A,R,T,E,T], 50),
        sum_alpha([S,A,X,O,P,H,O,N,E], 134),
        sum_alpha([S,C,A,L,E], 51),
        sum_alpha([S,O,L,O], 37),
        sum_alpha([S,O,N,G], 61),
        sum_alpha([S,O,P,R,A,N,O], 82),
        sum_alpha([T,H,E,M,E], 72),
        sum_alpha([V,I,O,L,I,N], 100),
        sum_alpha([W,A,L,T,Z], 34),

        labeling(LD).

test_alpha :-
	List=[A,B,C,E,F,G,L,N,O,R,T,U],
	List in 1..26, 
	different_fd(List),
	sum_alpha([B,A,L,L,E,T], 45), 
	sum_alpha([C,E,L,L,O], 43),
	sum_alpha([C,O,N,C,E,R,T], 74),
	sum_alpha([F,L,U,T,E], 30),
	sum_alpha([F,U,G,U,E], 50),
	labeling(List).

sum_alpha([],_).
sum_alpha([N], N).
sum_alpha([A,B|R], N) :-
	C .=. A + B,
	sum_alpha([C|R], N).	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Concurrent labeling                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main23(X,Y) :-
	[X,Y] in 0..2, 
	X .>. Y, 
	%X = 2.
	th_labeling(all, [X,Y], [], L),
	display(L).
	

main24(A, B) :-
	A in 18 .. 20 .&. 23 .. 28 .&. 29 .. 37,
	B in 19 .. 21 .&. 27 .. 32, 
 	A + 1 .=. B,
	th_labeling(all, [A,B], [], L),
	display(L).

main25(A, B, C) :-
	A in 17 .. 19 .&. 20 .. 22,
	B in 19 .. 21 .&. 23..24,
  	C in A .. B,
   	A .=<. B,
	th_labeling(all, [A,B,C], [], L),
	display(L).

main26(A, B, C):-
	A in 5 .. 10,
	B in 3 .. 8, 
	C in 0 .. 20, 
	A .=<. B,
	C .=. A + B,
	A .>. 6,
	th_labeling(all, [A, B, C], [], L),
	display(L).

main27(A,B,C,D) :-
	A in 0 .. 16,
	B in 0 .. 12,
	C in 7 .. 12 .&. 15 .. 17,
	D in 0 .. 100,
	A .=. 2*B,
	D .=. 3 * A,
	C .=. A + B,
	labeling([A,B,C,D]).
% 	th_labeling(first, [A,B,C,D], [], L),
%	display(L).

th_sum_test(L):-
	L = [A,B,C,D,E],
%	A1 in -1/0 .. 1/0,
       L in 0..10,
       L1 = [A1|L],
       different_fd(L),
       display('before*'),nl,
       A1 .=. 10 * A,
%       mult(A1, 10, A),
       display('after*'),nl,
       A1 .>. 0,
       A .=. B + C,
       D .=. A + E,
       labeling(L1).
%        th_labeling(first, L, [A1], Sols),
%        display(Sols).

th_mult_test(L):-
	L = [A,A2,A3,A4,A5,A6],
	different_fd(L),
	A in 0 .. 9,
	A2 .=. 10 * A,
	A3 .=. 100 * A2,
 	A4 .=. 1000 * A3,
	A5 .=. 10000 * A4,
 	A6 .=. 100000 * A5,
       th_labeling(first, L, Sols),
       display(Sols).

th_sum_mult_test(L1):-
	L1 = [A, B, C, D, E, F],
       L1 in 0..9,
       L = [S|L1],
       different_fd(L),
       A .>=. 1,
       A6 .=. 100000 * A,
       A5 .=. 10000 * B,
       A4 .=. 1000 * C,
       A3 .=. 100 * D,
       A2 .=. 10 *  E,
       R1 .=. A2 + F,
       R2 .=. A3 + R1,
       R3 .=. A4 + R2,
       R4 .=. A5 + R3,
       S .=. A6 + R4,
       th_labeling(first, L1, [A6, A5, A4, A3, A2, R1, R2, R3, R4], Sols),
       display(Sols).

elaborated1(X,Y):-
	[X,Y] in 0..100, 
	 Z .=. X + Y, 
	 U .=. 3*X, 
	% equ(Z,U), 
        Z .>. U,
	labeling([X,Y]).


negtest(X,Y) :-
	[X,Y] in -5 .. -3 .&. -1..10,
 	X-Y .=. -3*X,
 	labeling([X,Y]).

nlmult(X,Y,Z) :-
	[X,Y,Z] in -2..2,
	 X.=.Y*Z,
	 labeling([X,Y,Z]).

nldiv(X,Y,Z) :-
	[X,Y,Z] in -2..2,
	 X.=.Y/Z,
	 labeling([X,Y,Z]).
