:- module(fibo, 
	[
	    test/1, 
	    result/1,
 	    fibo/2,
	    spend_time/2
	]).


:- include(tabling_type).


:- table fibo/2.


spend_time(NT,T) :-
	abolish_all_tables,
        statistics(runtime,[_,_]),
	tp, abolish_all_tables,
        statistics(runtime,[_,Tt]),
	N is (NT / Tt) + 1,
        statistics(runtime,[_,_]),
        (
            between(1,N,_),
            tp, abolish_all_tables,
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin]),
        (
            between(1,N,_),
            fail
        ;
            true
        ),
        statistics(runtime,[_,Tfin2]),
        T is (Tfin - Tfin2) / N.

tp :-
	fibo(_,_), fail.
tp.

test(N) :-
	abolish_all_tables,
	fibo(19, N).

fibo(0,0).
fibo(1,1).
fibo(F,N) :-
	F > 1,
	F1 is F - 1,
	F2 is F - 2,
	fibo(F1, N1),
	fibo(F2, N2),
	N is N1 + N2.
	

result(4181).