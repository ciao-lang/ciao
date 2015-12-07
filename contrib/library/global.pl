% TODO: Missing author, title, and documentation.

% TODO: A proper library for nonbacktrackable (with copy) global
%       variables would supersede this module, which is not really
%       used in out sources.

:- module(global, [
        set_global/2,
        get_global/2,
        push_global/2,
        pop_global/2,
        del_global/1],[assertions]).

:- data set/2.

set_global(N, T) :- 
        nonvar(N),
        (retract_fact(set(N, _)) -> true ; true),
        asserta_fact(set(N, T)).
get_global(N, T) :-
        nonvar(N),
        current_fact(set(N, T1)), !,
        T = T1.
push_global(N, T) :- 
        nonvar(N),
        asserta_fact(set(N, T)).
pop_global(N, T) :- 
        nonvar(N),
        retract_fact(set(N, T1)), !,
        T = T1.
del_global(N) :- 
        nonvar(N),
        retractall_fact(set(N,_)).
