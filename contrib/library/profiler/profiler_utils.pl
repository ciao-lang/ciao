:- module(profiler_utils, [
		profile/1,
		profile_call/1,
		profile_reset/0,
		profile_info/1,
		activate_trace/0,
		deactivate_trace/0,
		activate_hooks/0,
		deactivate_hooks/0
	    ], [assertions, nativeprops]).

:- doc(author, "Edison Mera").
:- doc(module, "Profiler Utils").

:- doc(summary, "This inteface is like the SWI-Prolog profiler.").

:- use_module(library(read)).
:- use_module(library(streams)).
:- use_module(library(system)).
:- use_module(library(profiler/profiler_rt),   [profile_module_init/1]).
:- use_module(library(profiler/profiler_type), [profile_info_type/1]).

:- reexport(library(profiler/profiler_cc)).
:- reexport(library(profiler/profiler_utils_native)).
:- use_module(library(profiler/profiler_utils_base)).

:- doc(bug, "Perhaps the time for preparing the call to a
	predicate is cumulated in the last called (or retried)
	predicate (this could be a bug or a behavior). -- EMM").

:- doc(bug, "The hash table implementation used by the profiler
	must be unified with the hash tables of the ciao engine").

:- doc(bug, "This profiler currently do not support exception nor
	signal handling. -- EMM").

% :- use_module(engine(internals), [term_to_meta/2]).

:- meta_predicate profile_call(goal).

% Note that undo/1 can not be used here (we need to cut the choice points).

profile_call(Goal) :-
%	term_to_meta(Goal0, Goal),
	'$metachoice'(CP),
	profile_leave_fail,
	'$metachoice'(C0),
	profile_enter_call,
	call(Goal),
%	'$meta_call'(Goal0),
	profile_leave_exit,
	'$metachoice'(C1),
	profile_enter_redo,
	(C0==C1 -> '$metacut'(CP) ; true).

:- meta_predicate profile(goal).

:- true pred profile(Goal) :: term #

"Evaluates @var{Goal}, collect profile information related with the
evaluation and dump the information.".

profile(Goal) :-
	catch(profile_call(Goal), E, profile_error(E)).

:- true pred profile_info/1 => profile_info_type.

:- doc(bug, "Predicate profile_info/1 is implemented using
	temporary files instead of pipe/2 to avoid a bug that hangs
	this predicate. --EMM").

/*
profile_info(ProfileInfo) :-
	pipe(ReadFrom, WriteTo),
	current_output(CO),
	set_output(WriteTo),
	profile_dump,
	display('.'),
	close(WriteTo),
	set_output(CO),
	read_term(ReadFrom, ProfileInfo, []),
	close(ReadFrom).
*/

profile_info(ProfileInfo) :-
	mktemp_in_tmp('profinfoXXXXXX', FileName),
	open_output(FileName, CO),
	profile_dump,
	display('.'),
	close_output(CO),
	open_input(FileName, CI),
	read_term(ProfileInfo, []),
	close_input(CI),
	delete_file(FileName).

profile_reset :-
	do_profile_reset,
	profile_module_init(_).

activate_trace :- set_trace_active(1).
deactivate_trace :- set_trace_active(0).

activate_hooks :- set_hooks_active(1).
deactivate_hooks :- set_hooks_active(0).
