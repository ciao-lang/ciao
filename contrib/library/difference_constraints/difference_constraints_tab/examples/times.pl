:- use_module(library(between)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).

times(N,Q) :-
	NT is N * 1000,
	abolish_all_tables,
        statistics(runtime,[_,_]),
	(call(Q), fail; true),
        statistics(runtime,[_,Tt]),
	get_time(Tt,NT,T,Q),
	format("Execution time ~d milliseconds~n", T).
 
get_time(Tt,NT,Tt,_) :-
	Tt >= NT, !.

get_time(Tt,NT,T,Pred) :-
	N is NT / (Tt+1),
	abolish_all_tables,
        statistics(runtime,[_,_]),
        (
            between(1,N,_),
	    (call(Pred), fail; abolish_all_tables),
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin]),
        (
            between(1,N,_),
	    abolish_all_tables,
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin2]),
        T is (Tfin - Tfin2) / N.
