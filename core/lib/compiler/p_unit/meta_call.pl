:- module(meta_call,[meta_call/1, process_meta_call/5],[assertions, isomodes]).

%-------------------------------------------------------------------------
% Handling metacalls.
% This module implements some predicates needed by tr_syntax for handling
% metacalls properly.
%-------------------------------------------------------------------------

:- use_module(library(compiler/p_unit/itf_db)).

:- pred process_meta_call(Goal,GList,NoGList,NGList,NGoal) : cgoal(Goal)
   => cgoal * list * list * list * cgoal
   # "Given a meta-predicate call @var{Goal}, returns in @var{GList} the list of
   arguments which are goals, in @var{NoGList} the arguments which are not
   goals, in @var{NGoal} a goal to a meta-predicate named @var{Goal} with but
   replacing the arguments which are goals by the elements of @var{GList}.".
process_meta_call(Goal,GList,NoGList,NGList,NGoal) :-
    meta_call_(Goal,Meta),
    Goal =.. [F|Args],
    Meta =.. [_|MetaArgs],
    peel_meta_call_args(Args,MetaArgs,GList,NoGList),
    build_meta_call_args(Args,MetaArgs,NGList,NArgs),
    NGoal =..[F|NArgs].

:- pred meta_call(Goal) : cgoal(Goal) 
   # "Succeeds if @var{Goal} is a meta-predicate call.".
meta_call(Goal):-
    meta_call_(Goal, _).

meta_call_(Goal, Meta) :-
    current_itf(meta,Goal,Meta), !.
meta_call_('hiord_rt:call'(_G,Args), Meta) :-
    % since call/N is harwired, we generate the meta information on the fly,
    % based on the number of arguments provided
    functor(Args,_,N),
    Meta = 'hiord_rt:call'(pred(N), ?). % TODO: keep SYNC with p_unit_basic.pl

meta_arg(goal).
meta_arg(pred(_N)).

:- pred peel_meta_call_args(Args,MetaArgs,GList,NoGList)
   : (list(Args), list(MetaArgs)) => (list(GList), list(NoGList)).
peel_meta_call_args([],[],[],[]).
peel_meta_call_args([A|Args],[MetaArg|Metaargs],[A|GList],NoGList):-
    meta_arg(MetaArg), !, 
    peel_meta_call_args(Args,Metaargs,GList,NoGList).
peel_meta_call_args([A|Args],[_|Metaargs],GList,[A|NoGList]):-
    peel_meta_call_args(Args,Metaargs,GList,NoGList).

:- pred build_meta_call_args(Args,MetaArgs,NGList,NArgs)
   : (list(Args), list(MetaArgs)) => (list(NGList), list(NArgs)).
build_meta_call_args([],[],[],[]).
build_meta_call_args([_|Args],[MetaArg|Metaargs],[G|NGList],[G|NArgs]):-
    meta_arg(MetaArg), !,
    build_meta_call_args(Args,Metaargs,NGList,NArgs).
build_meta_call_args([A|Args],[_|Metaargs],NGList,[A|NArgs]):-
    build_meta_call_args(Args,Metaargs,NGList,NArgs).
