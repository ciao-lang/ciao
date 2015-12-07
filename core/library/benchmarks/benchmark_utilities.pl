:- module(benchmark_utilities, [ common_loop/5, run_bench/5 ], []).

:- use_module(library(prolog_sys)).

:- use_module(results).

:- meta_predicate common_loop(?, ?, goal, ?, ?).
common_loop(_Reps, N, Goal, LIs, _Name):-               %% Heat up the cache
        repeated_call_failing(1, run_bench(N, Goal, LIs, _Name, no)).
common_loop(Reps, N, Goal, LIs, Name):-               %% Do actual benchmarking
        repeated_call_failing(Reps, run_bench(N, Goal, LIs, Name, yes)).
common_loop(_,_,_,_,_).                           %% Exit with success


%% Perform N repetitions of some goal.
:- meta_predicate repeated_call_failing(?, goal).
repeated_call_failing(N, Call):-
        repeat_loop(N),
        once(Call),
        fail.

:- meta_predicate run_bench(?, goal, ?, ?, ?).
run_bench(Reps, Goal, LIs, Id, Save):-
        get_data(Reps, Goal, Timings),
        calculate_klips(Timings, Reps, LIs, Total, Loop, Bench, KLIPS),
        (Save = yes -> add_timings(Id, Total, Loop, Bench, KLIPS) ; true).

calculate_klips(timings(T1, T2, T3), Reps, LIBench, OT,ELT,BT,KLIPS):-
        OT is T2 - T1,                          % Overal loop time
        ELT is T3 - T2,                         % Empty loop time
        BT is OT - ELT,                         % Benchmark time
        KLIPS is (LIBench * Reps) / BT.  

:- meta_predicate get_data(?, goal, ?).
get_data(Reps, Goal, timings(T1, T2, T3)):-
        statistics(runtime,[T1,_]),
        repeat_N(Reps, Goal),
        statistics(runtime,[T2,_]),
        repeat_N(Reps, local_true),
        statistics(runtime,[T3,_]).


%% The compensation loop is actually making 1 LI, which somewhat
%% changes the time accounting.  But subtracting this LI causes
%% problems, as there are benchmarks which have only 1 LI per call
%% (but it is a "big" LI).

local_true.

:- meta_predicate repeat_N(?, goal).
repeat_N(N, Goal):-
        repeat_loop(N),
        once(Goal),
        fail.
repeat_N(_, _).

repeat_loop(_).
repeat_loop(N):-
        N > 1,
        N1 is N - 1,
        repeat_loop(N1).

:- meta_predicate once(goal).
once(G):- call(G), !.
