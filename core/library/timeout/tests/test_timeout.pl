:- module(test_timeout, _).

% TODO: document -- some seems broken

:- use_module(library(timeout), [call_with_time_limit/3]).

main([]):- test(2, _).

test(2, Time):-
	dichotomy((test(1, T), (T == 0 -> throw(time_limit_exceeded); true)), 3, 0, 15000, Time),
	message(['Time (2) = ', Time]). 

		     
test(1, Time):-
	dichotomy((queens(15, _L)), 5, 0, 100, Time),
	message(['Time (1) = ', Time]). 
				

dichotomy(Call, N, LowerBound, UpperBound, Result):-
	Time is (LowerBound + UpperBound) // 2,
%	message([dichotomy(Call, N, LowerBound, UpperBound, 'Result'), ' ',  Time]),
	(
	    \+ \+ (iterate(N, Time, Call, M), M > (N +1) // 2)  ->
	    (
		Time = UpperBound ->  
		Result = Time 
	    ;
		dichotomy(Call, N, LowerBound, Time, Result)
	    )
	;
	    (
		Time = LowerBound ->  
		Result = Time 
	    ;
		dichotomy(Call, N, Time, UpperBound, Result)
	    )
	).

:- meta_predicate(iterate(+, +, :, +)).

iterate(0, _Time, _Call, 0):-!.
iterate(N, Time, Call, M):-
	N2 is N -1, 
	iterate(N2, Time, Call, M2),
	call_with_time_limit(Time, (\+ \+ (Call), M is M2 + 1) , M = M2). 


%%%%%%%%%%%%%%%%%%%%
% N QUEENS PROBLEM %
%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(multi_arity_warnings,off).

queens(N,Qs):-
	range(1,N,Ns),
	queens(Ns,[],Qs).




queens([],Qs,Qs).

queens(UnplacedQs,SafeQs,Qs):-
	sel(UnplacedQs,UnplacedQs1,Q),
	not_attack(SafeQs,Q),
	queens(UnplacedQs1,[Q|SafeQs],Qs).


not_attack(Xs,X):-
	not_attack(Xs,X,1).
 

not_attack([],_,_).

not_attack([Y|Ys],X,N):-
	X =\= Y+N, 
	X =\= Y-N,
	N1 is N+1,  
	not_attack(Ys,X,N1).




sel([X|Xs],Xs,X).

sel([Y|Ys],[Y|Zs],X):-
	sel(Ys,Zs,X).




range(N,N,[N]):- !.

range(M,N,[M|Ns]):-
	M < N,
	M1 is M+1,
	range(M1,N,Ns).


:- set_prolog_flag(multi_arity_warnings,on).