:- module(lqsort_function, [test/2], [fsyntax, lazy]).

:- use_module(library(lazy/lazy_lib), _).
:- use_module(library(random)). 

:- fun_eval arith(true).

:- lazy fun_eval qsort(_,~).
qsort(X) := ~qsort_(X, []).

:- fun_eval qsort_(_,~).
qsort_([], Acc)    := Acc.
qsort_([X|T], Acc) := ~qsort_(S, [X|~qsort_(G, Acc)])
                   :- (S, G) = ~partition(T, X).

:- fun_eval partition(_,_,~).
partition([], _)    := ([], []).
partition([X|T], Y) := (S, [X|G]) :- Y < X, !, (S,G) = ~partition(T, Y).
partition([X|T], Y) := ([X|S], G) :- !, (S,G) = ~partition(T, Y).

:- fun_eval gen_list(_,~).
gen_list(0) := [].
gen_list(X) := [~random(1,1000000)|~gen_list(X-1)] :- X > 0.

:- fun_eval test(_,~).
test(X) :=
	~take(X,~qsort(~gen_list(X))).

