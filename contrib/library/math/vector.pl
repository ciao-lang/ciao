:- module(_,
	    [
		vector_norm_inf/2,
		vector_multiply/3,
		vector_constant_multiply/3,
		vector_constant_division/3,
		vector_eq/2,
		vector_sum/2,
		vector_square_sum/2,
		vector_addition/3,
		vector_substraction/3,
		vector_multiply_components/3,
		vector_divide_components/3,
		vector_is_zero/1,
		vector_sqrt/2,
		vector_project/4,
		vector_project_list/4,
		vector_division/3,
		vector_constant_multiply_addition/4
	    ],
	    [assertions, nativeprops, unittestdecls, hiord,
		library(math/math_clp)]).

:- load_test_package(library(math/math_clp)).

:- use_module(library(apply)).
:- use_module(library(hiordlib)).

:- push_prolog_flag(multi_arity_warnings, off).

:- doc(author, "Edison Mera").
:- doc(module, "This module contains methods for processing of vectors.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The folowing methods abstract the way the vector components are obtained   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export(mapvector/2).
:- meta_predicate mapvector(pred(1), ?).
mapvector(P, V) :- maplist(P, V).

:- export(mapvector/3).
:- meta_predicate mapvector(pred(2), ?, ?).
mapvector(P, V1, V2) :- maplist(P, V1, V2).

:- export(mapvector/4).
:- meta_predicate mapvector(pred(3), ?, ?, ?).
mapvector(P, V1, V2, V3) :- maplist(P, V1, V2, V3).

% :- meta_predicate mapvector(pred(4), ?, ?, ?, ?).
% mapvector(P, V1, V2, V3, V4) :- maplist(P, V1, V2, V3, V4).

:- export(mapvscale/4).
:- meta_predicate mapvscale(pred(3), ?, ?, ?).
mapvscale(P, V, E0, E) :- map(V, P, E0, E).

:- export(mapvscale/5).
:- meta_predicate mapvscale(pred(4), ?, ?, ?, ?).
mapvscale(P, V1, V2, E0, E) :- map(V1, V2, P, E0, E).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pop_prolog_flag(multi_arity_warnings).

:- test vector_norm_inf(L, S) : (L = [1, -2.0, 1.5])
	=> succeeds(S .=. 2) + not_fails
# "Norm of a 3 component vector".

:- test vector_norm_inf(L, S) : (L = []) => near(S, 0, 0) + not_fails
# "Empty list have norm zero".

:- pred vector_norm_inf(List, AbsMax) : list(number) * term =>
	list(number) * number # "Unifies @var{AbsMax} with the
	infinite norm, which is the maximum of the absolute value of
	the numbers in the list @var{List}.".

vector_norm_inf(X, A) :-
	mapvscale(max_abs_number, X, 0.0, A).

max_abs_number(X, A0, A1) :- A1 .=. max(abs(X), A0).

:- test vector_addition(A, B, C) : (A = [1, 2, 3], B = [3, 2, 2])
	=> succeeds((C = [C1, C2, C3], C1 .=. 4.0, C2 .=. 4.0, C3 .=. 5.0))
	+ not_fails # "Test addition of vectors".

:- true pred vector_addition(X, Y, Z) : list(number) * list(number) *
	term => list(number) * list(number) * list(number) # "Unifies
   @var{Z} with the sum of the vectors @var{X} and @var{Y}.".

vector_addition(X, Y, Z) :-
	mapvector((_(A, B, C) :- C .=. A + B), X, Y, Z).

vector_multiply_components(X, Y, Z) :-
	mapvector((_(A, B, C) :- C .=. A * B), X, Y, Z).

vector_divide_components(X, Y, Z) :-
	mapvector((_(A, B, C) :- C .=. A / B), X, Y, Z).

vector_is_zero(Vector) :- mapvector(scalar_eq(0.0), Vector).

vector_eq(A, B) :- map(A, scalar_eq, B).


scalar_eq(X, Y) :- X .=. Y.

:- test vector_sum(L, S) : (L = [1, 2, 3.0, 7]) => succeeds(S .=. 13.0) +
	not_fails # "Sum of 5 elements".

:- test vector_sum(L, S) : (L = []) => (S = 0) + not_fails
# "Empty list sums zero.".

:- true pred vector_sum(List, Sum) : list(number) * term => list(number) *
	number # "Unifies @var{Sum} with the total sum of the numbers in
      the list @var{List}.".

vector_sum(X, S) :- mapvscale((_(E, S0, S1) :- S1 .=. S0 + E), X, 0, S).

:- test vector_square_sum(L, S) : (L = [1, 2, 3.0, 7]) => succeeds(S .=. 63.0)
	+ not_fails # "Square sum of 5 elements".

:- test vector_square_sum(L, S) : (L = []) => succeeds(S .=. 0.0) + not_fails
# "Empty list sums zero".

:- true pred vector_square_sum(List, Sum2) : list(number) * term =>
	list(number) * number # "Unifies @var{Sum2} with the total sum of
   the square of the numbers in the list @var{List}.".

vector_square_sum(Vector, SquareSum) :-
	vector_multiply(Vector, Vector, SquareSum).

:- true pred vector_multiply(Vector1, Vector2, Result) : list(number) *
	list(number) * term => list(number) * list(number) * number #
"Unifies @var{Result} with the scalar product between the vectors
   @var{Vector1} and @var{Vector2}.".

vector_multiply(V1, V2, S) :-
	mapvscale((_(A, B, S0, S1) :- S1 .=. S0 + A * B), V1, V2, 0, S).

:- true pred vector_constant_multiply(Vector, Scalar, Result) : number *
	list(number) * term => number * list(number) * list(number) #
"Unifies @var{Result} with the scalar product between @var{Scalar}
   and @var{Vector}.".

vector_constant_multiply(X, R, Y) :-
	mapvector((_(A, B) :- B .=. A * R), X, Y).

:- true pred vector_constant_division(Vector, Scalar, Result) :
	list(number) * number * term => list(number) * number * list(number) #
"Unifies @var{Result} with the scalar product between
   1.0/@var{Scalar} and @var{Vector}.".

vector_constant_division(X, R, Y) :-
	mapvector((_(A, B) :- B .=. A / R), X, Y).

:- true pred vector_substraction(X, Y, Z) : list(number) * list(number) *
	term => list(number) * list(number) * list(number) # "Unifies
   @var{Z} with the rest of the vectors @var{X} and @var{Y}.".

vector_substraction(X, Y, Z) :-
	mapvector((_(A, B, C) :- C .=. A - B), X, Y, Z).

vector_division(X, Y, Z) :-
	mapvector((_(A, B, C) :- C .=. A / B), X, Y, Z).

vector_sqrt(V, S) :-
	mapvector((_(C, E) :- E .=. sqrt(C)), V, S).

:- true pred vector_constant_multiply_addition(Vector, Scalar, Add,
	    Result) : list(number) * number * list(number) * term => number *
	list(number) * list(number) * list(number) # "Unifies @var{Result}
   with the product between @var{Scalar} and @var{Vector}, plus
   @var{Add}.  In other words, @var{Result} = @var{Scalar} *
   @var{Vector} + @var{Add}.".

vector_constant_multiply_addition(Vs, Scalar, As, Rs) :-
	mapvector((_(V, A, R) :- R .=. Scalar * V + A), Vs, As, Rs).

:- test vector_project(A, B, C, D) :
	(
	    B = [a, b, c, d, e],
	    C = [b, d, e],
	    A = [aaa, bbb, ccc, ddd, eee]
	) => (D == [bbb, ddd, eee]) + not_fails.

vector_project(Data, Base, Projection, Out) :-
	mapvscale(( _(D, E, P0-Out0, P1-Out1) :-
		vector_project_elem(D, E, P0, P1, Out0, Out1) ), Data, Base,
	    Projection-Out, [] -[]).

vector_project_elem(Data, E, [E|Projection], Projection, [Data|Out], Out) :- !.
vector_project_elem(_,    _, Projection,     Projection, Out,        Out).

:- test vector_project_list(A, B, C, D) : (
	    B = [a, b, c, d, e],
	    C = [b, d, e],
	    A = [[aa, bb, cc, dd, ee],
		[aaa, bbb, ccc, ddd, eee]]) =>
	(D == [[bb, dd, ee], [bbb, ddd, eee]]) + not_fails.

vector_project_list(Data, Base, Projection, Out) :-
	mapvector(vector_project(Base, Projection), Data, Out).
