:- module(profiler_utils_native, [
		cost_center_node_value/3,
		cost_center_edge_value/4,
		cost_center_global_value/2
	    ], [assertions, nativeprops]).

:- doc(author, "Edison Mera").
:- doc(title, "Profiler Utils").

:- doc(module, "This inteface is similar to that of the SWI-Prolog profiler.").

:- use_module(library(profiler/profiler_rt), []).

:- doc(bug, "profile_dump/0 must be implemented in Prolog").

:- doc(bug, "Predicate profile_info/1 is implemented using
	temporary files instead of pipe/2 to avoid a bug that hangs
	this predicate. --EMM").

:- use_module(library(profiler/profiler_c), [
	cost_center_edge_counts/7,
	cost_center_edge_ticks/7,
	cost_center_node_counts/5,
	cost_center_node_ticks/5,
	cost_center_global_counts/3,
	cost_center_global_ticks/3
	]).
:- reexport(library(profiler/profiler_c), [
	profile_dump/0,
	using_timestamp/1,
	get_trace_active/1,
	get_trace_active/1,
	set_trace_active/1,
	get_hooks_active/1,
	set_hooks_active/1,
	do_profile_reset/0,

	dump_node_table_cc/0, 
	have_overhead/2,
	total_time/1
	]).

cost_center_node_value(Name/Arity, res(Res, Type), Counts) :-
	cost_center_node_value_1(Type, Res, Name, Arity, Counts).

cost_center_node_value_1(call_exit, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 0, 0, Value).
cost_center_node_value_1(call_fail, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 0, 1, Value).
cost_center_node_value_1(redo_exit, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 1, 0, Value).
cost_center_node_value_1(redo_fail, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 1, 1, Value).
cost_center_node_value_1(call, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 0, 0, V0),
	cost_center_node_value_2(Res, Name, Arity, 0, 1, V1),
	Value is V0 + V1.
cost_center_node_value_1(redo, Res, Name, Arity, Value) :-
	cost_center_node_value_2(Res, Name, Arity, 1, 0, V0),
	cost_center_node_value_2(Res, Name, Arity, 1, 1, V1),
	Value is V0 + V1.

cost_center_node_value_2(counts, Name, Arity, Enter, Leave, Counts) :-
	cost_center_node_counts(Name, Arity, Enter, Leave, Counts).
cost_center_node_value_2(ticks, Name, Arity, Enter, Leave, Ticks) :-
	cost_center_node_ticks(Name, Arity, Enter, Leave, Ticks).



cost_center_global_value(res(Res, Type), Counts) :-
	cost_center_global_value_1(Type, Res, Counts).

cost_center_global_value_1(call_exit, Res, Value) :-
	cost_center_global_value_2(Res, 0, 0, Value).
cost_center_global_value_1(call_fail, Res, Value) :-
	cost_center_global_value_2(Res, 0, 1, Value).
cost_center_global_value_1(redo_exit, Res, Value) :-
	cost_center_global_value_2(Res, 1, 0, Value).
cost_center_global_value_1(redo_fail, Res, Value) :-
	cost_center_global_value_2(Res, 1, 1, Value).
cost_center_global_value_1(call, Res, Value) :-
	cost_center_global_value_2(Res, 0, 0, V0),
	cost_center_global_value_2(Res, 0, 1, V1),
	Value is V0 + V1.
cost_center_global_value_1(redo, Res, Value) :-
	cost_center_global_value_2(Res, 1, 0, V0),
	cost_center_global_value_2(Res, 1, 1, V1),
	Value is V0 + V1.

cost_center_global_value_2(counts, Enter, Leave, Counts) :-
	cost_center_global_counts(Enter, Leave, Counts).
cost_center_global_value_2(ticks, Enter, Leave, Ticks) :-
	cost_center_global_ticks(Enter, Leave, Ticks).




cost_center_edge_value(Name0/Arity0, Name/Arity, res(Res, Type), Counts) :-
	cost_center_edge_value_1(Type, Res, Name0, Arity0, Name, Arity,
	    Counts).

cost_center_edge_value_1(call_exit, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 0, Value).
cost_center_edge_value_1(call_fail, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 1, Value).
cost_center_edge_value_1(redo_exit, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 0, Value).
cost_center_edge_value_1(redo_fail, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 1, Value).
cost_center_edge_value_1(call, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 0, V0),
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 0, 1, V1),
	Value is V0 + V1.
cost_center_edge_value_1(redo, Res, Name0, Arity0, Name, Arity, Value) :-
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 0, V0),
	cost_center_edge_value_2(Res, Name0, Arity0, Name, Arity, 1, 1, V1),
	Value is V0 + V1.

cost_center_edge_value_2(counts, Name0, Arity0, Name, Arity, Enter, Leave,
	    Counts) :-
	cost_center_edge_counts(Name0, Arity0, Name, Arity, Enter, Leave,
	    Counts).
cost_center_edge_value_2(ticks, Name0, Arity0, Name, Arity, Enter, Leave,
	    Ticks) :-
	cost_center_edge_ticks(Name0, Arity0, Name, Arity, Enter, Leave,
	    Ticks).
