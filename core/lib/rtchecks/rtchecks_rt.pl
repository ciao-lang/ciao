:- module(rtchecks_rt, [
		condition/1,
		checkif_comp/4,
		checkc/4,
		checkc/2,
		checkif/7,
		rtcheck/6,
		rtcheck/4,
		disj_prop/3,
		disj_prop/1,
		add_info_rtsignal/4,
		call_stack/2,
		non_inst/2,
		non_compat/2,
		% non_prop_check/3,
		% compat/1,
		% inst/1,
		% succeeds/1,
		attach_cut_fail/2,
		select_defined/3
	    ],
	    [assertions, nortchecks, hiord]).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(attributes)).
:- use_module(library(terms_vars)).
:- use_module(library(freeze)).
:- use_module(library(hiordlib)).

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
% 	:: condition * callable * term * callable.

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

:- meta_predicate non_compat(goal, ?).

non_compat('$:'(Goal),Args) :- !, non_compat_(Goal,Args).

non_compat_('term_typing:var'(A)    , _   ) :- !, nonvar(A).
non_compat_('term_typing:nonvar'(A) , _   ) :- !, var(A).
non_compat_('basic_props:atm'(A)    , _   ) :- !, \+ atm(A).
non_compat_('basic_props:int'(A)    , _   ) :- !, \+ int(A).
non_compat_('basic_props:nnegint'(A), _   ) :- !, \+ nnegint(A).
non_compat_('basic_props:num'(A)    , _   ) :- !, \+ num(A).
non_compat_(Goal                    , Args) :-
	varset(Args, VS),
	'$metachoice'(C),
	list(VS, cond_detach_attribute),
	list(VS, freeze('$metacut'(C))),
	'$meta_call'(Goal),
	% selectvars(Args, VS1),
	% varset(VS1, VS2),
	list(VS, cond_detach_attribute),
	!,
	fail.
non_compat_(_, _).

cond_detach_attribute(V) :-
	get_attribute(V, _) ->
	detach_attribute(V)
    ;
	true.

attach_cut_fail(V, C) :- attach_attribute(V, '$cut_fail'(V, C)).

:- meta_predicate non_inst(goal, ?).

non_inst('$:'(Goal),Args) :- !, non_inst_(Goal,Args).

non_inst_('term_typing:var'(A)   , _   ) :- !, nonvar(A).
non_inst_('term_typing:nonvar'(A), _   ) :- !, var(A).
non_inst_('basic_props:gnd'(A)   , _   ) :- !, \+ ground(A).
non_inst_('basic_props:int'(A)   , _   ) :- !, \+ integer(A).
non_inst_('basic_props:num'(A)   , _   ) :- !, \+ number(A).
non_inst_('basic_props:atm'(A)   , _   ) :- !, \+ atom(A).
non_inst_(Goal                   , Args) :-
	varset(Args, VS),
	'$metachoice'(C),
	list(VS, cond_detach_attribute),
	list(VS, attach_cut_fail(C)),
	'$meta_call'(Goal),
	list(VS, detach_attribute),
	!,
	fail.
non_inst_(_, _).

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

:- meta_predicate disj_prop(list(goal), ?, ?).

disj_prop([CheckProp|CheckProps], [PropName0|PropNames], PropName) :-
	CheckProp,
	PropName = PropName0
    ;
	disj_prop(CheckProps, PropNames, PropName).

:- meta_predicate disj_prop(list(goal)).

disj_prop([CheckProp|CheckProps]) :- CheckProp ; disj_prop(CheckProps).

:- meta_predicate checkc(list(goal), ?, ?, ?).
checkc(CheckProps, PropNames, PropName, Exit) :-
	disj_prop(CheckProps, PropNames, PropName) -> Exit = fail ; Exit = true.

:- meta_predicate checkc(list(goal), ?).
checkc(CheckProps, Exit) :-
	disj_prop(CheckProps) -> Exit = fail ; Exit = true.

:- meta_predicate checkif(?, ?, ?, ?, list(goal), ?, ?).
checkif(fail, _,       _,        _,    _,          _,      _).
checkif(true, ErrType, PredName, Dict, CheckProps, NProps, AsrLocs) :-
	rtcheck(ErrType, PredName, Dict, CheckProps, NProps, AsrLocs).

select_defined(N=V, SDict0, SDict) :-
	var(V) ->
	SDict = SDict0
    ;
	SDict0 = [N=V|SDict].

:- meta_predicate rtcheck(?, ?, ?, list(goal), ?, ?).
rtcheck(ErrType, PredName, Dict, CheckProps, NProps, AsrLocs) :-
	disj_prop(CheckProps, NProps, PropName-ActualProp) ->
	map(ActualProp, select_defined, ActualProp1, []),
	send_rtcheck(ErrType, PredName, Dict, PropName, ActualProp1, AsrLocs)
    ;
	true.

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
