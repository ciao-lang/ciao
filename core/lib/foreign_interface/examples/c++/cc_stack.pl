:- module(cc_stack, [cc_stack_new/1, cc_stack_size/2, 
                       cc_stack_push/2, cc_stack_pop/1, cc_stack_top/2], 
		      [foreign_interface, assertions]).

:- use_module(library(odd), [undo/1]).

:- true pred ciao_stack_new(go(Stack)) :: address 
	+ (foreign, returns(Stack)).
:- true pred ciao_stack_delete(in(_Stack)) :: address 
	+ foreign.
:- true pred ciao_stack_size(in(_Stack), go(Size)) ::  (address * int)  
	+ (foreign, returns(Size)).
:- true pred ciao_stack_push(in(_Stack), in(_Value)) :: (address * int) 
	+ foreign.
:- true pred ciao_stack_pop(in(_Stack)) ::  address
	+ foreign.
:- true pred ciao_stack_top(in(_Stack), go(Value)) ::  (address * int)  
	+ (foreign, returns(Value)).

cc_stack_new(cc_stack(X)) :-
	ciao_stack_new(X), 
	% stack are deallocated on backtrack.
	undo(ciao_stack_delete(X)).

cc_stack_size(cc_stack(X), Size):-
	ciao_stack_size(X, Size).

cc_stack_push(cc_stack(X), I):-
	ciao_stack_push(X, I).

cc_stack_pop(cc_stack(X)):-
	(
	    ciao_stack_size(X, Size), Size > 0  ->
	    ciao_stack_pop(X)
	;
	    throw(error(empty_cc_stack, cc_stack_pop/1-1))
	).

cc_stack_top(cc_stack(X), Int):-
	(
	    ciao_stack_size(X, Size), Size > 0  ->
	    ciao_stack_top(X, Int)
	;
	    throw(error(empty_cc_stack, cc_stack_top/1-1))
	).

	
:- use_foreign_library('stdc++').
:- use_foreign_source('cc_stack.cc').


