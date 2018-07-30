:- module(toplevel_io, [], []).

% This module is a simple abstraction for console (TTY) interaction
% for the toplevel (also used by the Ciao debugger).

% TODO: Currently just a copy of old ttyout.
% TODO: More abstract and flexible (replaceable?)

:- export(top_display/1).
top_display(X) :- display(user, X).

:- export(top_flush/0).
top_flush :- flush_output(user).

:- export(top_get/1).
top_get(N) :- get_code(user, N).

:- export(top_nl/0).
top_nl :- nl(user).

:- export(top_skipeol/0).
top_skipeol :- skip_code(user, 0'\n).

:- export(top_skip/1).
top_skip(X) :- skip_code(user, X).
