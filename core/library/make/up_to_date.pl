:- module(up_to_date, [up_to_date/2], [assertions, nortchecks]).

:- use_module(library(system)).

%% Assumes that source file exists
up_to_date(Target, Source) :-
	modif_time(Source, SourceTime),
	modif_time0(Target, TargetTime),
	SourceTime =< TargetTime.
