 %% USER MANUAL
 %% - fischer(P,S) returns in S all the reachable states for P processors
 %% - times(N,fisher(P,_)) repeats the execution of fischer (P processors)  
 %%   to get its execution time on average
 %%    N is the minimum time we want to execute times/2.

%% OPTIMIZATIONS
%% Remove entailed answers
%% merging when we have 2^n answers
%% concume_constraint_answer-> instant asignation
%% constraint answer representations (tries?)
%% We spent 2400 on entailment.
%% get_ta_member and put_member are linear!!
%% apply invariant is needed?
%% We spent 2400 on dijkstra
%% Read answer can be just change the state!!
%% More things for sure! 

:- module(fischer,
	[
	    times/2,
  	    f/2
	],[]).

:- use_module(library(dynamic)).
:- dynamic sol/1.
:- dynamic answ/1.

:- use_module(library(write),[print/1]).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints/difference_constraints_tab)).
:- use_package(library(difference_constraints)).

:- use_module(library(lists), [length/2]).

:- include(times).
:- include(initialize).
:- include(member).

:- table reach/1.

 %% f(N) :- 
 %% 	assert(sol(0)),
 %% 	assert(answ(0)),
 %% 	f(N,_S),
 %% 	retract(sol(N1)),
 %% 	N2 is N1 + 1,
 %% 	assert(sol(N2)),	
 %%  	fail.
 %% f(_) :-
 %% 	retract(answ(N1)),
 %% 	nl, display(answers(N1)),nl,
 %% 	retract(sol(N2)),
 %% 	display(soluciones(N2)),nl.
	
f(N,state(LY,K,Y)) :-
	length(LY,N),
	length(Y,N),
	reach(state(LY,K,Y)).

reach(state(LY,0,Y)) :-
	initialize_state(l0,LY),
	initialize_time_LB_equal(Y),
	apply_invariant(LY,Y).
 %%  %%    	max_lb(LB),
 %%  %%    	max_ub(UB),
 %%  %%     difference_constraints_do_canonical,
 %%  %%    	difference_constraints_normalize(Y,LB,UB),
 %%  %%   	print(answer_base(LY,0,Y)),nl,
 %% 	retract(answ(N1)),
 %% 	N2 is N1 + 1,
 %% 	assert(answ(N2)).

reach(state(LY,K2,Y)) :- 
	length(LY,N),
	length(LX,N),
	length(X,N),
	reach(state(LX,K1,X)), 
	each_trans(state(LX,K1,X),state(LY,K2,Y)).
 %%  %%     max_lb(LB),
 %%  %%     max_ub(UB),
 %%  %%     difference_constraints_do_canonical,
 %%  %%     difference_constraints_normalize(Y,LB,UB),
 %% 	retract(answ(N1)),
 %% 	N2 is N1 + 1,
 %% 	assert(answ(N2)).


each_trans(state(LX,KX,X),state(LY,KY,Y)) :-
	get_ta_member(LX,X,LXi,Xi,I),
	trans(state(LXi,I,KX,Xi),state(LYi,I,KY,Yi),Reset),
	difference_constraints_do_canonical,
	(
	    Reset = yes ->
	    take_out_member(I,X,Xdelay),
	    difference_constraints_reset(Xi,Yi,Xdelay),
	    difference_constraints_delay(Xdelay),
	    put_member(I,LYi,Yi,LX,Xdelay,LY,Y)
	;
	    difference_constraints_delay(X),
	    X = Y,
	    put_member(I,LYi,_,LX,_,LY,_)
	),
 %%   	get_clocks(LY,Y,[l0,l1,l2],Y1),
 %% 	print(Y1),nl,
 %%   	full_abstraction(Y1),
 %%  	take_out_member(KY,LY,LYtmp),
 %%  	take_out_member(KY,Y,Ytmp),
 %%  	get_clocks(LYtmp,Ytmp,[l2],Y1),
 %%  	get_clocks(LY,Y,[l2],Y1),
 %%   	make_indep(Y1),
 %%  	get_clocks(LY,Y,[l1],Y2),
 %%   	make_indep(Y2),
 %%   	get_clocks(LY,Y,[l0],Y3),
 %%    	make_indep(Y3),
	apply_invariant(LY,Y).

trans(state(l0,Id,0,_), state(l1,Id,0,X),yes) :- !,
	X #>= 0.

trans(state(l1,Id,_,Xin), state(l2,Id,Id,Xout),yes) :- !,
	Xin #=< 2,
	Xout #>= 0.

trans(state(l2,Id,K,X), state(l0,Id,K,X),no) :-
	K =\= Id, !,
	X #>= 4.	

trans(state(l2,Id,Id,X), state(lcs,Id,Id,X),no) :- !,
	X #>= 4.	

trans(state(lcs,Id,_,X), state(l0,Id,0,X),no) :- !. 

apply_invariant([],[]).
apply_invariant([L|RL],[X|RX]) :-
	invariant(state(L,_,_,X)),
	apply_invariant(RL,RX).

invariant(state(l0,_,_,_)).
invariant(state(l1,_,_,X)) :- X #=< 2.
invariant(state(l2,_,_,_)).
invariant(state(lcs,_,_,_)).

max_lb(4).
max_ub(2).