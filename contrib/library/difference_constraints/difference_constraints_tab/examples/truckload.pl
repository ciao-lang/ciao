 %% USER MANUAL
 %% - truckload(I,W,D,T): truckload problem where I is the number of packets, 
 %%   W is the load, D is the final city and T is the time restrictions.
 %% - t_truckload/4: same as truckload/4 but using tabling.
 %% - call examples:
 %%   truckload(60,100,chicago,T)
 %%   truckload(60,200,chicago,T)
 %%   truckload(60,300,chicago,T)
 %%   t_truckload(60,100,chicago,T)
 %%   t_truckload(60,200,chicago,T)
 %%   t_truckload(60,300,chicago,T)
 %% - times(N,P) repeats the execution of P to get its execution time on average.
 %%   N is the minimum time we want to execute times/2.

:- module(truckload,
	[
	    times/2,
	    t_truckload/4,
	    truckload/4
	],[]).

:- use_module(library(write),[print/1]).

:- use_package(library(tabling)).
:- const_table_module(library(difference_constraints/difference_constraints_tab)).
:- use_package(library(difference_constraints)).

:- include(times).

:- table t_truckload/4.

t_truckload(0,0,_,_).
t_truckload(I,W,D,T) :-
        I > 0,
        I1 is I - 1,
        t_truckload(I1,W,D,T).
t_truckload(I,W,D,T) :-
        I > 0,
        pack(I,Wi,D,T),
        W1 is W - Wi,
        W1 >= 0,
        I1 is I - 1,
        t_truckload(I1,W1,D,T).	

truckload(0,0,_,_).
truckload(I,W,D,T) :-
        I > 0,
        I1 is I - 1,
        truckload(I1,W,D,T).
truckload(I,W,D,T) :-
        I > 0,
        pack(I,Wi,D,T),
        W1 is W - Wi,
        W1 >= 0,
        I1 is I - 1,
        truckload(I1,W1,D,T).

 %% ABSTRACTION
 %% truckload(I,W,D,T) :-
 %%         I > 0,
 %%         pack(I,Wi,D,Ti),
 %%         W1 is W - Wi,
 %%         W1 >= 0,
 %%         I1 is I - 1,
 %%         truckload(I1,W1,D,T), T = Ti.

pack(60,29,chicago,T) :- T #>= 19, T #=< 30.
pack(59,82,chicago,T) :- T #>= 20, T #=< 30.
pack(58,24,chicago,T) :- T #>= 8,  T #=< 13.
pack(57,11,chicago,T) :- T #>= 21, T #=< 23.
pack(56,57,chicago,T) :- T #>= 8,  T #=< 29.
pack(55,30,chicago,T) :- T #>= 14, T #=< 19.
pack(54,71,chicago,T) :- T #>= 11, T #=< 15.
pack(53,31,chicago,T) :- T #>= 10, T #=< 26.
pack(52,34,chicago,T) :- T #>= 19, T #=< 21.
pack(51,73,chicago,T) :- T #>= 0,  T #=< 20.
pack(50,83,chicago,T) :- T #>= 21, T #=< 25.
pack(49,86,chicago,T) :- T #>= 10, T #=< 13.
pack(48,79,chicago,T) :- T #>= 22, T #=< 25.
pack(47,50,chicago,T) :- T #>= 18, T #=< 29.
pack(46,58,chicago,T) :- T #>= 13, T #=< 15.
pack(45,69,chicago,T) :- T #>= 4,  T #=< 15.
pack(44,77,chicago,T) :- T #>= 22, T #=< 29.
pack(43,74,chicago,T) :- T #>= 27, T #=< 31.
pack(42,65,chicago,T) :- T #>= 12, T #=< 25.
pack(41,26,chicago,T) :- T #>= 12, T #=< 27.
pack(40,56,chicago,T) :- T #>= 15, T #=< 17.
pack(39,15,chicago,T) :- T #>= 29, T #=< 31.
pack(38,81,chicago,T) :- T #>= 24, T #=< 27.
pack(37,45,chicago,T) :- T #>= 2,  T #=< 8 .
pack(36,40,chicago,T) :- T #>= 5,  T #=< 22.
pack(35,43,chicago,T) :- T #>= 4,  T #=< 11.
pack(34,22,chicago,T) :- T #>= 23, T #=< 30.
pack(33,60,chicago,T) :- T #>= 4,  T #=< 30.
pack(32,82,chicago,T) :- T #>= 28, T #=< 30.
pack(31,41,chicago,T) :- T #>= 27, T #=< 29.
pack(30,29,chicago,T) :- T #>= 19, T #=< 29.
pack(29,82,chicago,T) :- T #>= 20, T #=< 29.
pack(28,24,chicago,T) :- T #>= 8,  T #=< 12.
pack(27,11,chicago,T) :- T #>= 21, T #=< 22.
pack(26,57,chicago,T) :- T #>= 8,  T #=< 28.
pack(25,30,chicago,T) :- T #>= 14, T #=< 18.
pack(24,71,chicago,T) :- T #>= 11, T #=< 14.
pack(23,31,chicago,T) :- T #>= 10, T #=< 27.
pack(22,34,chicago,T) :- T #>= 19, T #=< 20.
pack(21,73,chicago,T) :- T #>= 0,  T #=< 19.
pack(20,83,chicago,T) :- T #>= 21, T #=< 24.
pack(19,86,chicago,T) :- T #>= 10, T #=< 12.
pack(18,79,chicago,T) :- T #>= 22, T #=< 24.
pack(17,50,chicago,T) :- T #>= 18, T #=< 28.
pack(16,58,chicago,T) :- T #>= 13, T #=< 14.
pack(15,69,chicago,T) :- T #>= 4,  T #=< 14.
pack(14,77,chicago,T) :- T #>= 22, T #=< 28.
pack(13,74,chicago,T) :- T #>= 27, T #=< 30.
pack(12,65,chicago,T) :- T #>= 12, T #=< 24.
pack(11,26,chicago,T) :- T #>= 12, T #=< 26.
pack(10,56,chicago,T) :- T #>= 15, T #=< 16.
pack(9, 15,chicago,T) :- T #>= 29, T #=< 30.
pack(8, 81,chicago,T) :- T #>= 24, T #=< 26.
pack(7, 45,chicago,T) :- T #>= 2,  T #=< 7.
pack(6, 40,chicago,T) :- T #>= 5,  T #=< 21.
pack(5, 43,chicago,T) :- T #>= 4,  T #=< 10.
pack(4, 22,chicago,T) :- T #>= 23, T #=< 29.
pack(3, 60,chicago,T) :- T #>= 4,  T #=< 29.
pack(2, 82,chicago,T) :- T #>= 28, T #=< 29.
pack(1, 41,chicago,T) :- T #>= 27, T #=< 28.
