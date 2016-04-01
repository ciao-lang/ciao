:- module(_, _, [fsyntax]).

% TODO: This example is outdated (synchronize with more recent LPdoc versions)

:- use_module(library(write)).
:- use_module(library(profiler/profiler_utils)).
:- use_module(library(profiler/profiler_auto_conf)).
:- use_module(lpdoc(docmaker)).

t0 :-
	cc_auto_conf(ticks, [lpdoc, autodoc, lpdoclib],
	    doc_cmd(~absolute_file_name(lpdocsrc(doc/'SETTINGS')), [], gen(all)),
	    3,
	    Goals, Tree),
	write(Goals),
	nl,
	write(Tree),
	nl.

t1 :-
	profile_reset,
	profile(doc_cmd(~absolute_file_name(lpdocsrc(doc/'SETTINGS')), [], gen(all))),
	profile_dump.
