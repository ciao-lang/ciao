:- module(toplevel_io, [], []).

% This module is a simple abstraction for console (TTY) interaction
% for the toplevel (also used by the Ciao debugger).

% TODO: Currently just a copy of old ttyout.
% TODO: More abstract and flexible (replaceable?)

:- use_module(engine(stream_basic), [flush_output/1]).
:- use_module(engine(io_basic)).
:- use_module(library(stream_utils), [get_line/2]).

:- export(top_display/1).
top_display(X) :- display(user, X).

:- export(top_flush/0).
top_flush :- flush_output(user).

:- export(top_get_line/1).
% (see get_line/2 documentation)
top_get_line(Line) :- get_line(user, Line).

:- export(top_nl/0).
top_nl :- nl(user).
