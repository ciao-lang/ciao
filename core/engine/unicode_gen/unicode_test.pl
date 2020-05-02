:- module(_, [], [foreign_interface, assertions]).

%:- use_module(library(streams)).
:- use_module(library(between)).
:- use_module(unicode_gen).

:- use_foreign_source(engine(rune)).

% Version using generated table (compressed)
:- export(rune_lookup_class/2).
:- trust pred rune_lookup_class(in(R), go(T)) :: c_int * c_int + (returns(T), foreign).

:- export(test_lookup_rune/0).
:- test test_lookup_rune + not_fails # "Rune class lookup is consistent with table".

test_lookup_rune :-
    prepare_tables,
    \+ ( between(0,0x110000,C), \+ rune_ok(C) ).

rune_ok(C) :-
    get_rune_class(C,X),
    rune_lookup_class(C,Y),
%    (X==Y->true;display(mismatch(C,X,Y)),nl,fail).
    X==Y.
