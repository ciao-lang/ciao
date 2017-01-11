:- module(fibo_large, 
	[
	    test/1, 
	    result/1,
 	    fibo_large/2,
	    spend_time/2
	]).


:- include(tabling_type).


:- table fibo_large/2.


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
	fibo_large(_,_), fail.
tp.

test(N) :-
	abolish_all_tables,
	fibo_large(1000, N).

fibo_large(0,0).
fibo_large(1,1).
fibo_large(F,N) :-
	F > 1,
	F1 is F - 1,
	F2 is F - 2,
	fibo_large(F1, N1),
	fibo_large(F2, N2),
	N is N1 + N2.
	

result(43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875).