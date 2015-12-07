:- module(actmodtr, [actmodtr/2], [assertions]).

actmodtr((:- use_active_module(M, Imports)), LocalDefs) :-
        define_remote_predicates(Imports, M, LocalDefs).

define_remote_predicates([],_M, []).
define_remote_predicates([F/A|Ps], M, [Def|Defs]) :-
        define_remote_predicate(F, A, M, Def),
        define_remote_predicates(Ps, M, Defs).

define_remote_predicate(F, A, M, Def) :-
        functor(P, F, A),
        Def = (P :- module_address(M,Add), remote_call(Add, P)).
