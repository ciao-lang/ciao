:- module(time_analyzer, [
		performance/3,

		benchmark/6,
		compare_benchmark/7,
		generate_benchmark_list/7,

		benchmark2/6,
		compare_benchmark2/7,
		generate_benchmark_list2/7,

		sub_times/3,
		div_times/2,

		cost/3
	    ], [assertions, regtypes, hiord]).


:- reexport(library(gnuplot)).

:- use_module(library(prolog_sys)).
:- use_module(library(lists)).
:- use_module(library(write)).
%:- use_module(library(system)).

:- use_module(library(hiordlib)).



% WE need this for treatment of pairs in compare_benchmark
:- use_module(engine(internals), [term_to_meta/2, module_concat/3]).


:- doc(title,
	    "Measuring features from predicates (time cost or memory used)").
:- doc(author, "David Trallero Mena").

:- doc(module, "This library has been done for measuring or compare
 execution features (currently only time) of predicates. This module relys
 on gnuplot, an auxiliary module which use the tool @tt{gnuplot}, for
 representing results graphically").

:- meta_predicate exec_pred_ntimes(goal, ?).


repeat(_).
repeat(N) :-
	N > 1,
	N2 is N - 1,
	repeat(N2).


exec_pred_ntimes(Pred, Times) :-
	repeat(Times),
	\+ \+ call(Pred),
	fail.

exec_pred_ntimes(_, _).



% exec_pred_ntimes( _Pred , 0 ) :- !.
%
% exec_pred_ntimes( Pred , Times ) :-
% 	 \+ \+ call( Pred ),
% 	 NTimes is Times - 1 ,
% 	 exec_pred_ntimes( Pred , NTimes ).


:- meta_predicate exec_pred_ntimes(goal, ?, ?).

time_of_exec_ntimes(Pred, Times, Time) :-
	statistics(runtime, [Start|_]),
	exec_pred_ntimes(Pred, Times),
	statistics(runtime, [End|_]),
	Time is (End - Start) /Times.




:- meta_predicate exec_graph_pred(?, goal, ?, ?, ?).

exec_graph_pred(0, _Pred, _Current, _Inc, []) :-
	!.

exec_graph_pred(Times, Pred, Current, Inc, [(Current, Res)|RRes]) :-
	time_of_exec_ntimes(Pred, Current, Res),
%	display( 'Repetitions: ' ) , display( Current ) , nl,
%	display( 'Total Time: ' ) , display( Res ) , nl,nl,
	NCurrent is Current + Inc,
	NTimes is Times - 1,
	exec_graph_pred(NTimes, Pred, NCurrent, Inc, RRes).



% exec_graph_pred( _Pred , Start , End , _Inc , [] ) :- 
% 	Start >= End, 
% 	!.

% exec_graph_pred( Pred , Start , End , Inc ,  [(Start,Res)|RRes] ) :- 
% 	time_of_exec_ntimes( Pred , Start , Res ),
% 	NStart is Start + Inc ,
% 	exec_graph_pred( Pred , NStart , End , Inc , RRes ).


exec_graph_exp_pred(_Pred, Start, End, _Exp, []) :-
	Start >= End.

exec_graph_exp_pred(Pred, Start, End, Exp, [(Start, Times)|RTimes]) :-
	time_of_exec_ntimes(Pred, Start, Times),
	NStart is Start*Exp,
	exec_graph_exp_pred(Pred, NStart, End, Exp, RTimes).


do_nothing. % <- SPECIAL PREDICATE.

:- meta_predicate performance(goal, ?, ?).

:- pred performance(P, M, Times)
	: callable * term * var => callable * term * list(num)

# "performance accepts a goal, @var{P}, as a first argument. The aim
  of this predicate is to call @var{P} several times and meassure some
  feature (in this version, only time, that is reason because no extra
  parameter has been added). @var{M} defines how many times @var{P}
  should be called. Usually, calling the predicate in some succesion
  (10,100,1000) and dividing by the number of times it is executed we
  can obtain the \"execution time\" of the predicate (if we are
  measuring time).

  The result of executions are returned in the list @var{Times}

 The diferent modes are:
@begin{itemize} 
 @item graph( Start , End , Increment ). It defines arithmetic succesion
 starting in Start and ending in End, by increment of Increment. So @var{P}
 is called Start times on the first time, Start+Increment on the second, etc.

 @item graph The same as graph/3 but with default options

 @item graph_exp( Start , End , Exp ). It defines geometric succesion.
Start is multiplied by Exp till it gets End. So @var{P} is called Start times
on the first time, Start*Exp on the second, etc.

 @item graph_exp The same as graph_exp/3 but with default options
@end{itemize}
".

performance(Pred, graph(S, E, I), TimesEnd) :-
	!,
	Times is integer((E - S) /I),
	exec_graph_pred(Times, Pred,       S, I, TimesA),
	exec_graph_pred(Times, do_nothing, S, I, TimesB),
%	display( times( TimesA , TimesB ) ),nl,
	sub_times(TimesA, TimesB, TimesEnd).
%	display( TimesEnd ) , nl.

performance(Pred, graph, Times) :-
	!,
	performance(Pred, graph(0, 10000, 1000), Times).

performance(Pred, graph_exp(Start, End, Exp), TimesEnd) :-
	!,
	exec_graph_exp_pred(Pred,       Start, End, Exp, Times),
	exec_graph_exp_pred(do_nothing, Start, End, Exp, Times2),
	sub_times(Times, Times2, TimesEnd).

performance(Pred, graph_exp, Times) :-
	!,
	performance(Pred, graph_exp(10, 100000, 10), Times).

performance(_, Method, []) :-
	message(error, ['Method ', Method, ' unknown']).

:- regtype pair/1.

pair((A, B)) :-
	num(A),
	num(B).

:- pred sub_times(A, B, C)
	:: list(pair) * list(pair) * list(pair)

# "C is the result of doing A - B, where A, B, C are a list of pairs
  as (Time,_)".

sub_times([],           [],           []) :- !.
sub_times([(A, B1)|R1], [(A, B2)|R2], [(A, B3)|R3]) :-
	B3 is B1 - B2,
	sub_times(R1, R2, R3).

:- pred div_times(A, B)
	:: list(pair) * list(pair)

# "@var{A} is a list of pairs (P1,P2). @var{B} is a list of pairs with
  the form (P1,P2/P1) for each (P1,P2) that belongs to @var{A}".

div_times([],          []) :- !.
div_times([(A, B)|R1], [(A, B1)|R2]) :-
	B1 is B/A,
	div_times(R1, R2).



:- pred cost(A, T, What) :: callable * int * term

# "This pred is thought for measuring constant complexity
   predicates. @var{T} is the expected measured feature. @var{What} is
   reserved for future implementations, just put 'runtime' ".

:- meta_predicate cost(goal, ?, ?).


cost(Pred, T, What) :-
	cost2(Pred, T, 10, 100, 10, What).



:- meta_predicate cost2(goal, ?, ?, ?, ?, ?).

cost2(Pred, T, S, E, I, What) :-
	write('Trying to stabilize with '), write(S),
	write(' to '), write(E),
	write(' by '), write(I), nl,
	performance(Pred, graph(S, E, I), Times),
	(
	    transform_to_time_list(Times, TTimes),
%	    write( TTimes ) , nl ,
	    is_stabilized(TTimes, T)
	->
	    true
	;
	    E1 is 2*E-S,
	    cost2(Pred, T, E, E1, I, What)
	).


is_stabilized(L, T) :-
	append(_, [A, B, C|_], L),
	fabse(A, B),
	fabse(B, C),
	T = C.

fabse(A, B) :-
	A > B,
	X is A-B,
	X < 0.5.

fabse(A, B) :-
	X is B-A,
	X < 0.5.

transform_to_time_list([(A, B)|R], [Res|RR]) :-
	Res is B/A,
	!,
	transform_to_time_list(R, RR).

transform_to_time_list([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% BENCHMARK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- regtype average_mode/1.

:- doc(average_mode(Flag), "@var{Flag} controls the treatement of the
           time lists. @var{Flag} can have any of the following values:
           @tt{null}, do nothing, @tt{min}, takes the minimum value of the
           list,
	   @tt{max}, takes the maximum value of the list,
           @tt{average}, computes the average of the list,
           @tt{use_pred(P)}, its invoque predicate P with times list as
           first argument and expect result in second argument.").


:- meta_predicate use_pred(pred(2)).


average_mode(null).
average_mode(min).
average_mode(max).
average_mode(average).
average_mode(use_pred(_X)).


:- pred benchmark(P, BenchList, NumTimes, Method, Reserved, OutList)
	: callable * list(pair) * int * average_mode * term * var =>
	callable * list(pair) * int * average_mode * term * list(pair)

# "The predicate @var{P}, which accepts ONE argument, is called with the
 first member of each pair of the @var{BenchList} list @var{NumTimes}. The
 entry list have pairs because the second member of the pair express the
 meaning of the first one in the X-Axis. For example, if we are doing a
 benchmark of qsort function, the first member will be a list for being
 ordered and the second one will be the length of the unordered list. The
 output is a list of (X,Y) points where Y means the time needed for its
 entry of \"cost\" X. @var{OutList} can be used as TimeList in predicate
 generate_plot. @var{Reserved} is reserved for future implementations (it
 will take the value of runtime, memory_used...)".

:- meta_predicate benchmark(addmodule, ?, ?, ?, ?, ?).

benchmark(Pred, Mod, BCH_LIST, Method, NumTimes, _, Times) :-
	module_concat(Mod, Pred, ModPred),
	ibenchmark(BCH_LIST, ModPred, Method, NumTimes, _, Times).




ibenchmark([(Arg1, XPos)|BCH_LIST], Pred, Method, NumTimes,
	    _, [(XPos, Med)|RTimes]) :-
	T =.. [Pred, Arg1],
	T = MT,
	module_concat(time_analyzer, do_nothing, DNT),
	time_of_bench_ntimes(_TimePred, MT,  NumTimes, Time),
	time_of_bench_ntimes(_TimePred, DNT, NumTimes, Time2),
	!,
	Med is Time - Time2,
	ibenchmark(BCH_LIST, Pred, Method, NumTimes, _, RTimes).
ibenchmark([], _Pred, _Method, _NumTimes, _, []).




% :- meta_predicate time_of_bench_ntimes( pred(2) , ? , ? , ? ).

time_of_bench_ntimes(_P, Arg1, Times, [Time]) :-
	statistics(runtime, [Start, _]),
	bench_ntimes(_P, Arg1, Times),
	statistics(runtime, [End, _]),
	Time is (End - Start) /Times.




bench_ntimes(_Pred, Arg1, Times) :-
	repeat(Times),
% 	T =.. [Pred , Arg1],
	T = Arg1,
	term_to_meta(T, MT),
	(call(MT) ->true;true),
	fail.
bench_ntimes(_, _, _).




% time_of_bench_ntimes( _Pred , _ , 0 , [] ) :- !.
%
% time_of_bench_ntimes( Pred , Arg1 , Times , [Time|RTimes] ) :-
%  	T =.. [Pred , Arg1 , Time],
%  	term_to_meta( T , MT ),
%  	\+ \+ call( MT ),
% 	T1 is Times - 1,
% 	time_of_bench_ntimes( Pred , Arg1 , T1 , RTimes ).




% :- meta_predicate measure_time( goal , ? ).
/*
measure_time(P, Time) :-
	statistics(runtime, [Start, _]),
	call(P),
	statistics(runtime, [End, _]),
	Time is (End - Start).

merge_ave_info([(_, XPos)|A1], [T|A2], [(XPos, T)|R]) :-
	merge_ave_info(A1, A2, R).
merge_ave_info([], [], []).
*/

compute_average(Times, null, Times).

compute_average(Times, min, MinTime) :-
	minimum(Times, <, MinTime).

compute_average(Times, max, MinTime) :-
	minimum(Times, >, MinTime).

compute_average(Times, average, MinTime) :-
	foldl(Times, 0, (''(A, B, C) :-C is A + B), Res),
	length(Times, Len),
	MinTime is Res / Len.

compute_average(Times, usepred(UserPred), MinTime) :-
	UserPred(Times, MinTime).


%%%%%%% COMPARE
:- meta_predicate compare_benchmark(addmodule, ?, ?, ?, ?, ?, ?).


:- pred compare_benchmark(ListPred, BenchList, Method, NumTimes,
	    BaseName, Reserved, GeneralOptions)
	: list(callable) * list * average_mode * int * atom * term * list
%       => list(callable) * list * average_mode * int * atom * term * list

# "It is the generalization of execute predicate @pred{benchmark/6}
  with several predicates. @pred{benchmark/6} predicate is called with
  each predicate in @var{ListPred}, and @var{BaseName} is used for the
  temporary file (without extension). @var{GeneralOptions} are aplied
  to the plot".

compare_benchmark(Preds, Mod, Bench, Method, NumTimes, BaseName, _, default) :-
	!,
	get_general_options(GO),
	compare_benchmark(Preds, Mod, Bench, Method, NumTimes, BaseName, _, GO
	).


compare_benchmark(Preds, Mod, Bench, Method, NumTimes, BaseName,
	    _, GeneralOptions) :-
	generate_benchmark_list(Preds, Mod, Bench, Method, NumTimes, _, Times),
	generate_plot(BaseName, Times, GeneralOptions).



generate_benchmark_list([(P1, LO1)|R], Mod, Bench, Method, NumTimes,
	    _, [(Times1, LO1)|TR]) :-
	module_concat(Mod, P1, NewP), !,
	ibenchmark(Bench, NewP, Method, NumTimes, _, Times1), !,
	generate_benchmark_list(R, Mod, Bench, Method, NumTimes, _, TR).

generate_benchmark_list([(P1, LO1, Module)|R], Mod, Bench, Method, NumTimes,
	    _, [(Times1, LO1)|TR]) :-
	module_concat(Module, P1, NewP), !,
	ibenchmark(Bench, NewP, NumTimes, Method, _, Times1), !,
	generate_benchmark_list(R, Mod, Bench, Method, NumTimes, _, TR).

generate_benchmark_list([function(A)|R], Mod, Bench, Method, NumTimes,
	    _, [function(A)|TR]) :-
	!,
	generate_benchmark_list(R, Mod, Bench, Method, NumTimes, _, TR).

generate_benchmark_list([P1|R], Mod, Bench, Method, NumTimes,
	    _, [(Times1, [])|TR]) :-
	module_concat(Mod, P1, NewP), !,
	ibenchmark(Bench, NewP, Method, NumTimes, _, Times1), !,
	generate_benchmark_list(R, Mod, Bench, Method, NumTimes, _, TR).

generate_benchmark_list([], _Mod, _Bench, _, _NumTimes, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VERSION 2
%%
%% In this version the bechmark predicate
%% can give us the time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% BENCHMARK 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate benchmark2(addmodule, ?, ?, ?, ?, ?).

:- pred benchmark2(P, BenchList, Method, NumTimes, What, OutList)
	: callable * list(pair) * average_mode * int * atom * var =>
	callable * list(pair) * average_mode * int * atom * list(pair)


# "The predicate @var{P}, which accepts TWO arguments, is called
 @var{NumTimes} with the first member of each pair of the
 @var{BenchList} list and a free variable as the second. The time of
 execution (in the future, the desired featured for be measured) is
 expected to be the second argument, that is because it is a
 variable. The entry list, @var{BenchList} have pairs because the
 second member of the pair express the cost of the first (in
 X-Axis). For example, if we are doing a benchmark of qsort function,
 the first member will be a list for being ordered and the second one
 will represent the lenght of the unordered list.  The output is a
 list of (X,Y) points where Y express the time needed for they entry
 of \"cost\" X. @var{OutList} can be used as TimeList in predicate
 generate_plot. @var{What} is reserved for future use".


benchmark2(Pred, Mod, BCH_LIST, Method, NumTimes, _, Times) :-
	module_concat(Mod, Pred, ModPred),
	ibenchmark2(ModPred, BCH_LIST, Method, NumTimes, _, Times).


ibenchmark2(Pred, [(Arg1, XPos)|BCH_LIST], Method, NumTimes, _,
	    [(XPos, Med)|RTimes]) :-
	time_of_bench_ntimes2(Pred, Arg1, NumTimes, Time), !,
	compute_average(Time, Method, Med),
	ibenchmark2(Pred, BCH_LIST, Method, NumTimes, _, RTimes).

ibenchmark2(_Pred, [], _Method, _NumTimes, _, []).


time_of_bench_ntimes2(_Pred, _, 0, []) :-
	!.
time_of_bench_ntimes2(Pred, Arg1, Times, [Time|RTimes]) :-
	T =.. [Pred, Arg1, Time],
	term_to_meta(T, MT),
%% DO NOT \+ \+ this call, because the need Time variable instantiated!!!
	call(MT),
	!,
	T1 is Times - 1,
	time_of_bench_ntimes2(Pred, Arg1, T1, RTimes).


%%%%%%% COMPARE 2
:- meta_predicate compare_benchmark2(addmodule, ?, ?, ?, ?, ?, ?).

:- pred compare_benchmark2(ListPred, BenchList, Method, NumTimes,
	    BaseName, Reserved, GeneralOptions)
	: list(callable) * list * average_mode * int * atom * atom * list =>
	list(callable) * list * average_mode * int * atom * atom * list

# "It is the generalization of execute predicate @pred{benchmark2/6}
  with several predicates.  @pred{benchmark2/6} is called with each
  predicate in @var{ListPred} and @var{BaseName} is used for the
  temporary file (without extension).  @var{GeneralOptions} are
  applied to the plot ('default' can be used for default General
  options)".

compare_benchmark2(Preds, Mod, Bench, Method, NumTimes, BaseName, _, default)
:- get_general_options(GO),
	compare_benchmark2(Preds, Mod, Bench, Method, NumTimes, BaseName, _, GO
	).


compare_benchmark2(Preds, Mod, Bench, Method, NumTimes,
	    BaseName, Opt, GeneralOptions) :-
	generate_benchmark_list2(Preds, Mod, Bench, Method, NumTimes, Opt, Times
	),
	generate_plot(BaseName, Times, GeneralOptions).

generate_benchmark_list2([(P1, LO1)|R], Mod, Bench, Method, NumTimes, Opt,
	    [(Times1, LO1)|TR]) :-
	module_concat(Mod, P1, NewP),
	ibenchmark2(NewP, Bench, Method, NumTimes, Opt, Times1), !,
	generate_benchmark_list2(R, Mod, Bench, Method, NumTimes, Opt, TR).

generate_benchmark_list2([(P1, LO1, Module)|R], Mod, Bench, Method, NumTimes,
	    Opt, [(Times1, LO1)|TR]) :-
	module_concat(Module, P1, NewP),
	ibenchmark2(NewP, Bench, Method, NumTimes, Opt, Times1), !,
	generate_benchmark_list2(R, Mod, Bench, Method, NumTimes, Opt, TR).

generate_benchmark_list2([function(A)|R], Mod, Bench, Method, NumTimes, Opt,
	    [function(A)|TR]) :-
	!,
	generate_benchmark_list2(R, Mod, Bench, Method, NumTimes, Opt, TR).

generate_benchmark_list2([P1|R], Mod, Bench, Method, NumTimes, Opt,
	    [(Times1, [])|TR]) :-
	module_concat(Mod, P1, NewP),
	ibenchmark2(NewP, Bench, Method, NumTimes, Opt, Times1), !,
	generate_benchmark_list2(R, Mod, Bench, Method, NumTimes, Opt, TR).

generate_benchmark_list2([], _Mod, _Bench, _, _NumTimes, _, []).
