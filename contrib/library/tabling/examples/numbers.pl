 :- module(numbers, 
         [
             lex/3
         ],[]).
 
 :- set_prolog_flag(multi_arity_warnings, off).
 
 :- use_module(library(prolog_sys)).
 :- use_module(library(write)).

:- include(tabling_type).

:- table find_xp/3.

lex(N, E, L):-
	find_xp(N, E, L).


 % find(+X, ?E, +L): the expression E uses all the numbers in the list
 % L and its numerical value  is X. By now, X is restricted to be integer.


find_xp(X, X, [X]).
find_xp(X, sqrt(T), [T]):- sqrt(T,X), X * X =:= T.
find_xp(X, E, L) :- 
	asim_subset(L, La, Lb),
	find_xp(Xa, A, La),
	find_xp(Xb, B, Lb),
	oper_comm(Xa, Xb, A, B, X, E).

find_xp(X, E, L) :- 
	asim_subset(L, La, Lb),
	find_xp(Xa, A, La),
	find_xp(Xb, B, Lb),
	oper_nocomm(Xa, Xb, A, B, X, E).

operate(A, B, C):- eval(A + B,C), integer(C).
operate(A, B, C):- eval(A - B,C), integer(C).
operate(A, B, C):- eval(B - A,C), integer(C).
operate(A, B, C):- eval(A * B,C), integer(C).
operate(A, B, C):- B =\= 0, eval(A / B,C), integer(C).
operate(A, B, C):- A =\= 0, eval(B / A,C), integer(C).


 % directed_search(+X, ?E, +L): the expression E uses al the numbers in
 % the list L and its numerical value is X. Integer descompositions are
 % sought for. This is quite tricky, because the final
 % search knows about it to reduce the search space.

 % search(?X, ?Y, +Z): the number X can be expressed by the expression Y
 % using the numbers in the list Z. As a heuristic rule, addition and
 % multiplication are tried before.

search(X, X, [X]).                  % Green.
search(X, E, L):-
	asim_subset(L, La, Lb),
	find_xp(Xa, A, La),
	find_xp(Xb, B, Lb),
	oper_comm(Xa, Xb, A, B, X, E).
search(X, E, L):-
	asim_subset(L, La, Lb),
	find_xp(Xa, A, La),
	find_xp(Xb, B, Lb),
	oper_nocomm(Xa, Xb, A, B, X, E).

eval(X,X) :- number(X).
eval(X + Y,Z) :- eval(X,X1), eval(Y,Y1), Z is X1 + Y1.
eval(X - Y,Z) :- eval(X,X1), eval(Y,Y1), Z is X1 - Y1.
eval(X * Y,Z) :- eval(X,X1), eval(Y,Y1), Z is X1 * Y1.
eval(X / Y,Z) :- eval(X,X1), eval(Y,Y1), Z is X1 / Y1.
eval(X // Y,Z) :- eval(X,X1), eval(Y,Y1), integer(X1), integer(Y1), Z is X1 // Y1.
eval(sqrt(X),Z) :- eval(X,X2), sqrt(X2,Z), Z * Z =:= X2.

oper_comm(Xa, Xb, A, B, X, A + B):- perform(Xa + Xb, X).
oper_comm(Xa, Xb, A, B, X, A * B):- perform(Xa * Xb, X).

oper_nocomm(Xa, Xb, A, B, X, A - B):- perform(Xa - Xb, X).
oper_nocomm(Xa, Xb, A, B, X, B - A):- perform(Xb - Xa, X).
oper_nocomm(Xa, Xb, A, B, X, A / B):- perform(Xa / Xb, X).
oper_nocomm(Xa, Xb, A, B, X, B / A):- perform(Xb / Xa, X).


 % perform(+Op, ?Res): Op is a rational or an operation between two
 % rationals, and Res is the result (rational) of that operation.

perform(Op, Res):-
	rewrite(Op, Op1),
	reduce(Op1, Res).


 % rewrite(?X, ?Y): X is a rational or an expression involving TWO
 % rationals and Y is the same expression rewrote so that the main
 % functor (if any) is '/'/2.

rewrite(A / B + C / D, (A * D + B * C) / (B * D)).
rewrite(A / B + C, (A + C * B) / B ):- atomic(C).
rewrite(C + A / B, (A + C * B) / B):- atomic(C).
rewrite(A + B, A + B):- atomic(A), atomic(B).
rewrite(A / B - C / D,  (A * D - B * C) / (B * D)).
rewrite(A / B - C, (A - B * C) / B):- atomic(C).
rewrite(C - A / B, (C * B - A) / B):- atomic(C).
rewrite(A - B, A - B):- atomic(A), atomic(B).
rewrite((A / B) * (C / D), (A * C) / (B * D)).
rewrite(A * (B / C), (A * B) / C):- atomic(A).
rewrite((A / B) * C, (A * C) / B):- atomic(C).
rewrite(A * B, A * B):- atomic(A), atomic(B).
rewrite((A / B) / (C / D), (A * D) / (B * C)).
rewrite(A / (B / C), (A * C) / B):- atomic(A).
rewrite((A / B) / C, A / (B * C)):- atomic(C).
rewrite(A / B, A / B):- atomic(A), atomic(B).
rewrite(A, A):- atomic(A).


 %

reduce(_ / Y, _):- 
        0 =:= Y, !, fail.
reduce(X / Y, Z):- !,
	(0 =:= X mod Y ->
	    eval(X // Y,Z);
            fail
	).
reduce(X, Y):- eval(X,Y).


fac_add(X, Y, Z):-
	eval(X // 2,X2),
	eval(X - X2,X1),
	fac_add(X2, X1, Y, Z).
fac_add(X, Y, X1, Y1):- swap(X, Y, X1, Y1).
fac_add(X, Y, X1, Y1):-
	X > 0,
	eval(X - 1,Xa),
	eval(Y + 1,Ya),
	fac_add(Xa, Ya, X1, Y1).


 % fac_mul(X, Y, Z): X = Y * Z, X, Y and Z natural numbers.

fac_mul(X, Y, Z):-
	sqrt(X, X2),
	fac_mul(X, X2, Y1, Z1),
	swap(Y1, Z1, Y, Z).
fac_mul(X, Xd, Xd, D):-
	Xd =\= 0,
	0 =:= X mod Xd,
	eval(X // Xd,D).
fac_mul(X, Xd, Y, Z):-
	Xd > 0,
	eval(Xd - 1,Xd1),
	fac_mul(X, Xd1, Y, Z).


 % Horrible integer square root algorithm.

sqrt(1, S):- !, S = 1.
sqrt(X, Y):-
	sqrt(X, X, 0, Y).
sqrt(X, Max, Min, Y):-
	Med is (Max + Min) // 2,
	sqrt(X, Max, Med, Min, Y).
sqrt(_, __, Med,  Min, Y):-
	Med - Min < 1, !, Y = Min.
sqrt(X, Max, Med, Min, Y):-
	(
	    Med * Med =< X ->
	    sqrt(X, Max, Med, Y);
	    sqrt(X, Med, Min, Y)
	).

 % Only different numbers are swapped.

swap(X, Y, X, Y).
swap(X, Y, Y, X):- X =\= Y.


 % subset(X, Y, Z): Y and Z are subsets of X. Relative order is preserved.

subset([], [], []).
subset([X|Xs], Ys, [X|Zs]):- subset(Xs, Ys, Zs).
subset([X|Xs], [X|Ys], Zs):- subset(Xs, Ys, Zs).


 % nep_subset(X, Y, Z): Y and Z are non-empty subsets of X

nep_subset(X, Y, Z):-
	Y = [_|_],
	Z = [_|_],
	subset(X, Y, Z).


 % asim_subset(X, Y, Z): Y and Z are non-empty subsets of X and a
 % solution given for Y on backtracking will never be returned for Z

asim_subset([X|Xs], [X|Ys], Zs):-
	Zs = [_|_],
	subset(Xs, Ys, Zs).


 % select(X, Y, Z): the element X is caught from the list Y to give
 % the list Z.

select(X, [X|Y], Y).
select(X, [_|Y], Z):-
	select(X, Y, Z).

 
 %

 %% member(X, [X|_]).
 %% member(X, [_|L]):- member(X, L).


 % Definition of a natural number.

natural(0).
natural(X):-
	natural(Y),
	eval(Y + 1,X).
