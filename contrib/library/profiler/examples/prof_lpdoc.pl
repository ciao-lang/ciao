:- module(_, _, [fsyntax]).

% TODO: This example is outdated (synchronize with more recent LPdoc versions)

:- use_module(library(write)).
:- use_module(library(profiler/profiler_utils)).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(lpdoc(lpdoc)).

t0 :-
	cc_auto_conf(ticks, [lpdoc, autodoc, lpdoclib],
	    handle_args(
		['-f', ~absolute_file_name(lpdocsrc(doc/'SETTINGS')), all]),
	    3,
	    Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(handle_args(['-f', ~absolute_file_name(lpdocsrc(doc/'SETTINGS')), all])),
	profile_dump.
