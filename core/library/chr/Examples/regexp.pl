:- module(regexp, [test/1, test/0, sim/2, expr/2]).

:- use_package(chr).          % For Ciao
%:- use_module(library(chr)).   % For SWI

:- op(200, yf, *).
:- op(200, yf, +).
:- op(700, xfx, ~).

:- chr_constraint f/2.
:- chr_constraint f/5.
:- chr_constraint union/3.
:- chr_constraint or/3.
:- chr_constraint (~)/2.

sim(A, B) :- A ~ B.

K ~ L \ K ~ L <=> true.

f([], R) <=> R = (0, [], []).
f([K|L], R) <=> R = (T, A, B),
    f(K, (Kt, Ka, Kb)), f(L, (Lt, La, Lb)), 
    or(Kt, Lt, T), union(Ka, La, A), union(Kb, Lb, B).
f(a, R) <=> R = (0, [[]*], []).
f(b, R) <=> R = (0, [], [[]*]).
f(K*, R) <=> R = (1, [(Ka,[K*])], [(Kb,[K*])]), 
    f(K, (_, Ka, Kb)). 
f((K, L), R) <=>  f(K, (Kt, Ka, Kb)), f(Kt, Ka, Kb, L, R).
f(0, Ka, Kb, L, R) <=> R = (0, [(Ka,L)], [(Kb,L)]).
f(1, Ka, Kb, L, R) <=> R = (T, A, B), f(L, (T, La, Lb)), 
     union([(Ka, L)], La, A), union([(Kb, L)], Lb, B).

f(K+ , R) <=>
     R = (T, [Ka, (Ka,K+)], [Kb, (Kb,K+)]), f(K, (T, Ka, Kb)).
f(eps, R) <=>  R = (1, [], []).

union(L, [], R) <=> L = R.
union([], L, R) <=> L = R.
union([L|M], [K|N], R) <=> L @< K | union([L|M], N, S), R = [K|S].
union([K|M], [K|N], R) <=> union(M, N, S), R = [K|S].
union([L|M], [K|N], R) <=> L @> K | union(M, [K|N], S), R = [L|S].

or(1, A, R) <=> R = 1.
or(A, 1, R) <=> R = 1.
or(0, 0, R) <=> R = 0.

K~L ==> nonvar(K), nonvar(L) |
     f(K, (T, Ka, Kb)), f(L, (T, La, Lb)), Ka ~ La, Kb ~ Lb.

expr(1, ((b*,a)*,(a,b*))*).
expr(2, [[]*, (a, [a,b]*), ([a,b]*,(a,(a,[a,b]*)))]).

test(X):-
	statistics(runtime, _), 
	expr(1, E1), expr(2, E2),  E1 ~ E2,
	statistics(runtime, [_, X]).

test:-
  test(A), write(A), !, fail.
test.
