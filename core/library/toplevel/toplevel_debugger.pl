% ---------------------------------------------------------------------------
% Debug mode per module/file

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(compiler),
	    [set_debug_mode/1, set_nodebug_mode/1, mode_of_module/2,
	     set_debug_module/1, set_nodebug_module/1,
	     set_debug_module_source/1]).
:- use_module(library(compiler/c_itf), [interpret_srcdbg/1]).

current_source_debugged(Ss) :-
	findall(S, current_fact(interpret_srcdbg(S)), Ss).

% ---------------------------------------------------------------------------
% Debugger toplevel interface

:- use_module(library(lists), [difference/3]).
:- use_module(library(format), [format/3]).

:- export(consult/1).
consult([]) :- !.
consult([File|Files]) :- !,
	consult(File),
	consult(Files).
consult(File) :-
	set_debug_mode(File),
	ensure_loaded(File).

:- export(compile/1).
compile([]) :- !.
compile([File|Files]) :- !,
	compile(File),
	compile(Files).
compile(File) :-
	set_nodebug_mode(File),
	ensure_loaded(File).

:- export(debug_module/1).
:- redefining(debug_module/1).
debug_module(M) :-
	set_debug_module(M),
	debugger:debug_module(M),
	( mode_of_module(M, Mode), Mode \== interpreted(raw) ->
	    message(user, ['{Consider reloading module ', M, '}'])
	; true
	),
	display_debugged.

:- export(nodebug_module/1).
:- redefining(nodebug_module/1).
nodebug_module(M) :-
	set_nodebug_module(M),
	debugger:nodebug_module(M),
	display_debugged.

:- export(debug_module_source/1).
:- redefining(debug_module_source/1).
debug_module_source(M) :-
	set_debug_module_source(M),
	debugger:debug_module_source(M),
	( mode_of_module(M, Mode), Mode \== interpreted(srcdbg) ->
	    message(user, ['{Consider reloading module ', M, '}'])
	; true
	),
	display_debugged.

:- export(display_debugged/0).
display_debugged :-
	current_debugged(Ms),
	current_source_debugged(Ss),
	difference(Ms, Ss, M),
	( M = [] ->
	    format(user, '{No module is selected for debugging}~n', [])
	; format(user, '{Modules selected for debugging: ~w}~n', [M])
	),
	( Ss = [] ->
	    format(user, '{No module is selected for source debugging}~n', [])
	; format(user, '{Modules selected for source debugging: ~w}~n', [Ss])
	).

