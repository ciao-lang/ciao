:- module(builder_flags, [], [assertions, regtypes, datafacts]).

:- doc(title, "Flags for the builder").
% (atomic name, single value)


:- data builder_flag_/2.

:- export(get_builder_flag/2).
get_builder_flag(Name, Value) :-
    current_fact(builder_flag_(Name, Value)).

:- export(set_builder_flag/2).
set_builder_flag(Name, Value) :-
    retractall_fact(builder_flag_(Name, _)),
    asserta_fact(builder_flag_(Name, Value)).

:- export(cleanup_builder_flags/0).
cleanup_builder_flags :-
    retractall_fact(builder_flag_(_, _)).


