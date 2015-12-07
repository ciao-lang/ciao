:- module(profiler_extra, [measure/2, measure/3, measure_nf/3,
		measure_0/3, get_option/1, time_option/2],
	    [assertions]).

:- use_module(library(hrtime)).

:- use_module(library(prolog_sys)).

:- doc(module, "Implementation of some predicates related with the
	profiler, but that don't requires the activation of the
	profiling option in the engine.").

:- data get_option/1.

% note that is better to use usertime, the problem is that currently
% for a better functioning, it requires the kernel reconfiguration :-S
% to bypass operating system limitations.

% get_option(usertime).
% get_option(walltime).
get_option(ticks).

:- meta_predicate measure(goal, ?).

:- true pred measure(Goal, Value) : callable(Goal) => num(Value) #
	"Same as measure/3, but uses ticks to measure the time.".

measure(Goal, Value) :-
	get_option(Option),
	measure(Option, Goal, Value).

:- meta_predicate measure(?, goal, ?).

:- true pred measure(Option, Goal, Value) ::
	(atom(Option), callable(Goal)) => num(Value)
# "Unifies @var{Value} with the time spent in evaluate @var{Goal},
   using the type of time @var{Option}.".

measure(Option, Goal, Value) :-
	time_option(Option, T1),
	\+ \+ call(Goal),
	time_option(Option, T2),
	Value is T2 - T1.

:- meta_predicate measure_nf(?, goal, ?).

measure_nf(Option, Goal, Value) :-
	time_option(Option, T1),
	(\+ call(Goal) -> true ; true),
	time_option(Option, T2),
	Value is T2 - T1.

:- meta_predicate measure_0(?, goal, ?).

measure_0(Option, Goal, Value) :-
	time_option(Option, T1),
	call(Goal),
	time_option(Option, T2),
	Value is T2 - T1.

time_option(ticks, Value) :-
	!,
	hrtime(Value).
time_option(Option, Value) :-
	time_option(Option),
	!,
	statistics(Option, [Value, _]).
