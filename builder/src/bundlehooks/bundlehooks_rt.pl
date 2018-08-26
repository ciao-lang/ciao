:- module(_, [], []).

:- use_module(engine(hiord_rt), [call/1]).

:- export(default_preinstalled/3).
% Use preinstalled version by default if available
:- meta_predicate default_preinstalled(goal, ?, ?).
default_preinstalled(IsPreinstalled, AllowAutoInstall, Value) :-
	( call(IsPreinstalled) -> Value = yes % use preinstalled
	; AllowAutoInstall = yes -> Value = no % try our own
	; Value = yes % not preinstalled but cannot try our own
	).

:- export(default_enabled/3).
% default_enabled(Bundle, UsePreinstalled, Value):
%   Enable by default if a preinstalled version is available or 
%   we can use our own version (do not use the preinstalled version).
:- meta_predicate default_enabled(goal, ?, ?).
default_enabled(IsPreinstalled, UsePreinstalled, Value) :-
	( UsePreinstalled = no -> Value = yes % enabled (use our own)
	; call(IsPreinstalled) -> Value = yes % enabled (use preinstalled)
	; Value = no
	).

:- export(need_auto_install/3).
% need_auto_install(Enabled, UsePreinstalled, Value):
%   Auto-install if enabled and we do not want the preinstalled version
need_auto_install(yes, no, yes) :- !.
need_auto_install(_, _, no).
