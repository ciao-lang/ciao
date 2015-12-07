:- module(profiler_utils_base,
	    [
		profile_error/1,
		profile_enter_redo/0,
		profile_leave_fail/0],
	 [assertions, nativeprops]).


:- use_module(library(profiler/profiler_c), [
	profile_enter_redo_1/0, 
	profile_leave_fail_1/0,
	profile_leave_error/0
	]).

:- reexport(library(profiler/profiler_c), [
	profile_enter_call/0,
	profile_leave_exit/0,
	profile_leave_error/0
	]).

profile_leave_fail.
profile_leave_fail :- profile_leave_fail_1.

profile_enter_redo.
profile_enter_redo :- profile_enter_redo_1.

profile_error(E) :-
	profile_leave_error,
	throw(E).

% Debugger hook ??? Try to figure out if this is required:
% profile_start :- profile_start.



