:- module(up_to_date, [up_to_date/2], [assertions, nortchecks]).

:- use_module(library(system), [modif_time/2, modif_time0/2]).

% Rule for checking up-to-date targets based on modification time.
% (NOTE: for use in other tools)

%% Assumes that source file exists
up_to_date(Target, Source) :-
	modif_time(Source, SourceTime),
	modif_time0(Target, TargetTime),
	SourceTime =< TargetTime.
