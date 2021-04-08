:- module(rtchecks_rt, [
            condition/1,
            checkif_comp/4,
            checkc/4,
            checkc/2,
            checkif/7,
            rtcheck/6,
            rtcheck/4,
            add_info_rtsignal/4,
            call_stack/2,
            rtc_inst/2,
            rtc_compat/2
            % non_prop_check/3,
            % compat/1,
            % inst/1,
            % succeeds/1,
        ],
        [assertions, nortchecks, hiord]).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(attributes)).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(freeze), [freeze/2]).
:- use_module(engine(basic_props_rtc)). % [compat_*/1]

:- reexport(library(rtchecks/rtchecks_send)).

:- doc(author, "Edison Mera").

%:- doc(author, "Based on David Trallero's rtchecks package.").

:- doc(module, "This module contains the predicates that are
    required to implement run-time checks.").

%% check_comp copied from rtchecks_mod.pl to checkif_comp -- EMM

:- prop condition/1 + regtype.

condition(true).
condition(fail).

:- meta_predicate add_info_rtsignal(goal, ?, ?, ?).
add_info_rtsignal(Goal, PredName, Dict, Pos) :-
    intercept(Goal, rtcheck(comp, _, _, Prop, Valid, []),
        send_rtcheck(comp, PredName, Dict, Prop, Valid, Pos)).

:- pred checkif_comp(Condition, CompGoal, CompGoalArg, Head)

# "If @var{Condition} is @tt{true} then the @var{CompGoal} containing
the nested comp predicate calls is called with @var{Head} as
argument. To allow efficient implementation, @var{CompGoalArg} is the
last nested argument of @var{CompGoal}, so unifiying with @var{Head}
we have the comp check, and calling directly to @var{Head} we skip the
test. An example call could be:

@begin{verbatim}
checkif_comp(C,not_fails(is_det(A)),A,partition(_1,_2,_3,_4))
@end{verbatim}

so if C is true (it should come from @pred{checkc/2} predicate), then
A is unified with partition(_1,_2,_3,_4) and
not_fails(is_det(partition(_1,_2,_3,_4))) is called. Else, just
partiton(_1,_2,_3,_4) is called.".

% :- trust pred checkif_comp(Condition, CompGoal, CompGoalArg, Head)
%       :: condition * cgoal * term * cgoal.

:- doc(bug, "checkif_comp/4 generates a unnecessary run-time
    module expansion").

:- meta_predicate checkif_comp(?, goal, ?, goal).
checkif_comp(true, Comp, Goal, Goal) :- call(Comp).
checkif_comp(fail, _,    _,    Goal) :- call(Goal).

/*
:- doc(bug, "non_compat/2 and non_inst/2 are incompatible with
    attributed variables, due to the usage of the freeze/2
    predicate.").

selectvar(V, VL0, VL) :-
    var(V) -> VL0 = [V|VL] ; VL0 = VL.

selectvars([],    []).
selectvars([V|L], VL0) :-
    selectvar(V, VL0, VL),
    selectvars(L, VL).
*/

:- meta_predicate rtc_compat(goal, ?).

rtc_compat('$:'(Goal),Args) :- !, rtc_compat_(Goal,Args).

% These first cases are really a hardwired runtime table for rtc_impl (needed
% because in some parts of the system non_compat/2 is called programatically)
% TODO: create automatically
rtc_compat_('term_typing:var'(A)    , _   ) :- !, var(A).
rtc_compat_('term_typing:nonvar'(_) , _   ) :- !.
%
rtc_compat_('basic_props:atm'(A) , _   ) :- !, compat_atm(A).
rtc_compat_('basic_props:int'(A) , _   ) :- !, compat_int(A).
rtc_compat_('basic_props:nnegint'(A) ,_) :- !, compat_nnegint(A).
rtc_compat_('basic_props:num'(A) , _   ) :- !, compat_num(A).
rtc_compat_('basic_props:flt'(A) , _   ) :- !, compat_flt(A).
rtc_compat_('basic_props:struct'(A) , _) :- !, compat_struct(A).
%
rtc_compat_(Goal,Args) :- \+ non_compat(Goal,Args). % TODO: fix non_compat/2

non_compat(Goal                    , Args) :-
% A generic implementation of non_compat/2
    varset(Args, VS),
    '$metachoice'(C),
    cond_detach_attribute_list(VS),
    freeze_metacut_list(VS, C),
    '$meta_call'(Goal),
    % selectvars(Args, VS1),
    % varset(VS1, VS2),
    cond_detach_attribute_list(VS),
    !,
    fail.
non_compat(_, _).

cond_detach_attribute_list([]).
cond_detach_attribute_list([V|Vs]) :-
    cond_detach_attribute(V),
    cond_detach_attribute_list(Vs).

cond_detach_attribute(V) :-
    ( get_attribute(V, _) -> detach_attribute(V) ; true ).

detach_attribute_list([]).
detach_attribute_list([V|Vs]) :-
    detach_attribute(V),
    detach_attribute_list(Vs).

freeze_metacut_list([], _).
freeze_metacut_list([V|Vs], C) :-
    freeze(V, '$metacut'(C)),
    freeze_metacut_list(Vs, C).

attach_cut_fail_list([], _).
attach_cut_fail_list([V|Vs], C) :-
    attach_cut_fail(V, C),
    attach_cut_fail_list(Vs, C).

attach_cut_fail(V, C) :- attach_attribute(V, '$cut_fail'(V, C)).


:- meta_predicate rtc_inst(goal, ?).

rtc_inst('$:'(Goal),Args) :- !, rtc_inst_(Goal,Args).

% These first cases are really a hardwired runtime table for rtc_impl (needed
% because in some parts of the system non_inst/2 is called programatically)
% TODO: create automatically
rtc_inst_('term_typing:var'(A)   , _   ) :- !, var(A).
rtc_inst_('term_typing:nonvar'(A), _   ) :- !, nonvar(A).
rtc_inst_(Goal,Args) :- \+ non_inst(Goal,Args).

non_inst(Goal                   , Args) :- % TODO: fix
% A generic implementation of non_inst/2
    varset(Args, VS),
    '$metachoice'(C),
    cond_detach_attribute_list(VS),
    attach_cut_fail_list(VS,C),
    '$meta_call'(Goal),
    detach_attribute_list(VS),
    !,
    fail.
non_inst(_, _).

:- multifile verify_attribute/2.

verify_attribute('$cut_fail'(Var, C), _) :-
    detach_attribute(Var),
    '$metacut'(C),
    fail.

:- multifile combine_attributes/2.
combine_attributes('$cut_fail'(V1, C), '$cut_fail'(V2, C)) :-
    detach_attribute(V1),
    detach_attribute(V2),
    V1 = V2,
    '$metacut'(C),
    fail.


:- push_prolog_flag(multi_arity_warnings, off).

:- meta_predicate disj_neg_prop(list(goal), ?, ?).

disj_neg_prop([CheckProp|CheckProps], [PropName0|PropNames], PropName) :-
    (\+ CheckProp),
    PropName = PropName0
    ;
    disj_neg_prop(CheckProps, PropNames, PropName).

:- meta_predicate disj_neg_prop(list(goal)).

disj_neg_prop([CheckProp|CheckProps]) :- (\+ CheckProp) ; disj_neg_prop(CheckProps).

:- pop_prolog_flag(multi_arity_warnings).

:- meta_predicate checkc(list(goal), ?, ?, ?).
checkc(CheckProps, PropNames, PropName, Exit) :-
    disj_neg_prop(CheckProps, PropNames, PropName) -> Exit = fail ; Exit = true.

:- meta_predicate checkc(list(goal), ?).
checkc(CheckProps, Exit) :-
    disj_neg_prop(CheckProps) -> Exit = fail ; Exit = true.

:- meta_predicate checkif(?, ?, ?, ?, list(goal), ?, ?).
checkif(fail, _,       _,        _,    _,          _,      _).
checkif(true, ErrType, PredName, Dict, CheckProps, NProps, AsrLocs) :-
    rtcheck(ErrType, PredName, Dict, CheckProps, NProps, AsrLocs).

:- meta_predicate rtcheck(?, ?, ?, list(goal), ?, ?).
rtcheck(ErrType, PredName, Dict, CheckProps, NProps, AsrLocs) :-
    ( disj_neg_prop(CheckProps, NProps, PropName-ActualProp) ->
        send_rtcheck(ErrType, PredName, Dict, PropName, ActualProp, AsrLocs)
    ; true
    ).

:- meta_predicate rtcheck(goal, ?, ?, ?).
rtcheck(Check, PredName, Dict, Loc) :-
    rtcheck_(Check, PredName, Dict, Loc),
    fail.
rtcheck(_, _, _, _).

:- meta_predicate rtcheck_(goal, ?, ?, ?).
rtcheck_(Check, _, _, _) :-
    call(Check),
    !.
rtcheck_(Check, PredName, Dict, Loc) :-
    send_rtcheck(pp_check, PredName, Dict, Check, [], [pploc(Loc)]).

:- meta_predicate call_stack(goal, ?).
call_stack(Goal, Pos) :-
    intercept(Goal, rtcheck(Type, PredName, Dict, PropName, Valid, Poss),
        send_rtcheck(Type, PredName, Dict, PropName, Valid, [Pos|Poss])).
