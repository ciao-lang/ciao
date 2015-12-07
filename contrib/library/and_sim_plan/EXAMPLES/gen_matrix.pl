:- module(gen_matrix,
        [
            gen_matrix/3
	],
	[]).

:- use_package(fsyntax).
:- use_module(library(random)).

:- fun_eval arith(true).

:- fun_eval gen_matrix/2.
gen_matrix(0,_) := [].
gen_matrix(X,Y) := [~gen_list(Y)|gen_matrix(X-1,Y)] :- X > 0.

:- fun_eval gen_list/1.
gen_list(0) := [].
gen_list(X) := [~random(1,1000)|~gen_list(X-1)] :- X > 0.
