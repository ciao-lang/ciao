:- module(_, [type_of_goal/2, meta_to_list/2], [assertions,hiord]).

:- use_module(library(compiler/p_unit/native), [
    builtin/2,
    native_builtin/2,
    native_property/2,
    wam_builtin/2]).
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(compiler/p_unit/p_unit_db), [assertion_read/9, current_itf/3]).
:- use_module(library(lists), [member/2, length/2]).
:- use_module(library(hiordlib), [maplist/2]).

%% ---------------------------------------------------------------------------
:- pred type_of_goal(Type, Goal) :: ( cgoal(Goal),
        member(Type, [imported, exported, multifile, dynamic,
                metapred(T, Meta), builtin(T)]) )
     # "@var{Goal} is declared of type @var{Type}.".
% This one can be optimized depending on the calling mode!
% TODO: IG cuts missing? clauses for type_of_goal(builtin(_)) do have cuts
type_of_goal(imported, Goal) :-
    current_itf(imports, Goal, M), atom(M).
type_of_goal(imported,    \+(_)). % kludge while Ciao-1.11 does not work
type_of_goal(imported(M), Goal) :-
    current_itf(imports, Goal, M), atom(M).
% TODO: IG could the previous clauses be removed?
type_of_goal(exported, Goal) :-
    current_itf(exports, Goal, _).
type_of_goal(exported(M), Goal) :- % TODO: IG why?
    current_itf(exports, Goal, M).
type_of_goal(multifile, Goal) :-
    current_itf(multifile, Goal, _).
type_of_goal(dynamic, Goal) :-
    current_itf(dynamic, Goal, _).
type_of_goal(impl_defined, Goal) :-
    current_itf(impl_defines, Goal, _).
type_of_goal(wam_builtin,Goal) :- % TODO: Can this and previous clause be unified, or is the difference relevant in some cases?
    wam_builtin(Goal,_).
% these might be defined outside:
type_of_goal(metapred(call(X),     call(goal)),           call(X)).
type_of_goal(metapred(not(X),      \+(goal)),             \+(X)).
type_of_goal(metapred(if(X, Y, Z), if(goal, goal, goal)), if(X, Y, Z)).
%
type_of_goal(metapred(apply(X, Y),    call(goal, ?)), call(X, Y)).
type_of_goal(metapred(apply(X, Args), Meta),          Goal) :-
    functor(Goal, call, A),
    A > 2,
    Goal=..[call, X|Args],
    A1 is A-1,
    length(L, A1),
    maplist(=(?), L),
    Meta=..[call, goal|L].
type_of_goal(metapred(Type, Meta), Goal) :-
    current_itf(meta, Goal, Meta),
    ( type_of_goal(builtin(Type), Goal)
    -> true
    ; Type=Goal
    ).
type_of_goal(metapred(Type, Meta), 'hiord_rt:call'(G,As)) :- !,
    functor(As,_,N),
    Meta = 'hiord_rt:call'(pred(N), ?), % TODO: keep SYNC with meta_call.pl
    Type = G.
% the one introduced by prepare_ai_output:
type_of_goal(metapred(true(G), true(goal)), true(G)).
% ACC: Neccesary for parallelizers
type_of_goal(metapred('andprolog_rt:&'(A, B),
            'andprolog_rt:&'(goal, goal)),
        'andprolog_rt:&'(A, B)).
%
type_of_goal(builtin(Type), Goal) :-
    native_builtin(Goal, Type), !. % builtin tables
type_of_goal(builtin(Type), Goal) :-
    native_property(Goal, Type), !. % builtin tables
type_of_goal(builtin(Blt), Goal) :-
    assertion_read(Goal, _M, _Status, _Type, Body, _Dict, _S, _LB, _LE),
    assertion_body(Goal, _Compat, _Call, _Succ, Comp, _Comm, Body), % TODO: precompute like we do in init_native_props/0 (native(G,Prop,Type)?)
    (current_itf(meta, Goal, Meta) ->
     inverse_transform_metapred(Goal, Meta, GoalT); % TODO: is this really necessary?
     GoalT=Goal
    ),
    builtin(native(GoalT, Blt), Native),
    member(Native, Comp), !.

inverse_transform_metapred(H, Meta, HT) :-
    H =.. [F|A],
    Meta =.. [F|MA],
    inverse_transform_metapred__(MA, A, AT),
    HT =.. [F|AT].

inverse_transform_metapred__([],     [],     []).
inverse_transform_metapred__([E|Es], [A|As], [A, _|AsT]) :-
    (E = addmode ; E = addmodule(?)),
    !,
    inverse_transform_metapred__(Es, As, AsT).
inverse_transform_metapred__([_E|Es], [A|As], [A|AsT]) :-
    inverse_transform_metapred__(Es, As, AsT).

%% ---------------------------------------------------------------------------

% this is neccesary because of the addmodule.
% Imagine we have: (goal,?,addmodule)
% when trying to rewrite the body we will ask for the
% 1st argument: goal, the second, ?, the 3rd addmodule,
% but what happend with the 4th!??
%
% So this predicate will generate [goal,?,?,?] for the 
% example already said (goal,?,addmodule)

:- pred meta_to_list(M, ML) :: (term(M), list(ML))
# "Transform a meta-predicate declaration (something like
  metapred(goal.,?,addmodule), obtained from @pred{type_of_goal/2})
  into a list of meta-predicate options but without @tt{addmodule}
  option, i.e., remove @tt{addmodule} and add @tt{?, remove}. 

Example:

@begin{verbatim}
?- meta_to_list( metapred(goal,addmodule) , A ).

A = [goal,?,remove] ? ;

no
?- 
@end{verbatim}".
meta_to_list(Meta, MetaL) :-
    Meta =.. [_|MetaArgs],
    meta_to_list__(MetaArgs, MetaL).

meta_to_list__([],    []).
meta_to_list__([E|A], [?, remove|B]) :-
    (E = addmodule ; E = addmodule(?)),
    !,
    meta_to_list__(A, B).
meta_to_list__([C|A], [C|B]) :-
    meta_to_list__(A, B).
