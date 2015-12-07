:- module(_, [rtchecks_sentence_tr/4, valid_commands/1,
		rtchecks_goal_tr/3, collect_assertions/3, generate_rtchecks/7,
		generate_rtchecks/11],
	    [assertions, nortchecks, nativeprops, isomodes, dcg, hiord]).

% You can test the inliner package with this module
:- use_package(library(rtchecks/rtchecks_tr_inline)).
% :- use_package(library(rtchecks/rtchecks_tr_library)).

% see formulae, conj_to_list/2, list_to_conj/2
:- use_module(library(llists)).
:- use_module(library(aggregates)).
:- use_module(library(terms)).
:- use_module(library(sort)).
:- use_module(library(rtchecks/term_list)).
:- use_module(library(assertions/assrt_lib), [assertion_read/9,
		assertion_body/7, comps_to_goal/3, comps_to_goal/4]).
:- use_module(library(compiler/c_itf_internal),
	    [discontiguous/3, defines_module/2, exports_pred/3, location/1,
		location/3]).
:- use_module(library(inliner/inliner_tr), [in_inline_module_db/2, inline_db/4,
		lit_clause_arity/4, compound_struct/3]).

:- doc(author, "Edison Mera").

:- doc(module, "This module provides and implements the run-time
checking of predicate assertions transforming the procedure
definitions.

The semantic of run-time checks is explained in the paper:

@href{http://clip.dia.fi.upm.es/papers/assert-lang-disciplbook_bitmap.pdf}

The rtchecks are generated in such a way that redundant tests are
collapsed, in order to avoid overhead.

Several instrumentation modes are provided to implement the run-time
checks. One is inline, which instrument the code directly. The other
mode is library, that use external metapredicates to do the checks,
this mode save space but could have less performance.

Note that the instrumentation can be done in the call to predicates
(transforming calls), or in the procedure definitions (transforming
procedure definitions), which is the instrumentation method
implemented here.").

:- doc(bug, "Currently we can not deal with dynamic/data
	predicates. --EMM").

:- doc(bug, "The unit-tests fails when I use in a test a regular
	type defined in the same module that is not exported. To solve
	it the runtime-check package must export the regular type
	automatically, and perhaps, to show a warning informing the
	user about such situation. --EMM").

:- doc(bug, "Assertions must be defined before the predicate. This
	is deliberate, since we preferred not to have to process all
	the predicates, clauses and assertions at the end, losing the
	locator information and confusing the debugger. In case of an
	assertion defined later, a warning message is shown,
	recommending to write a discontiguous directive (?). --EMM").

:- doc(bug, "Unimplemented: another level of instrumentation
	should be defined to report what literal caused the throw of a
	runtime check error, when such literal is not using the
	rtchecks package. This should be done implementing the
	``transforming calls'' instrumentation method.  --EMM").

:- doc(bug, "Runtime-checks for builtins or implementation-defined
	predicates not implemented yet.  Idea: we can use the
	redefining/1 declaration in order to work around this
	problem. --EMM").

:- doc(bug, "Unify it with the rtchecks package by David Trallero.
	I am not using rtchecks by DTM because:
	 @item it depends on ciaopp
	 @item to use it you should use transform
	 @item it does not do a clean inline instrumentation of the code.
	 @item I tried to use a full set of assertions but it does not
	       generate the code correctly w.r.t. the specification.
--EMM").

:- doc(bug, "Redundant tests in [entry, exit] assertions are not
	collapsed with [calls, success] assertions. It is a bug or a
	behavior???, I am not sure, because [calls, success] are more
	restrictive and is not necessary to put it together with
	[entry, exit] assertions. --EMM").

:- doc(bug, "@var{Compat} are compatibility properties, while
	@var{Call} and @var{Succ} are instantiation properties.  Note
	that the semantic should be the same as in CiaoPP, but I see
	that in CiaoPP compat properties are currently ignored
	(?). --EMM").

:- data head_alias_db/3.
:- data goal_alias_db/3.
:- data generated_rtchecks_db/3.
% :- data rtchecks_db/3.
% :- data nortchecks_db/3.
:- data posponed_sentence_db/7.

valid_assertions(true,  entry) :- !, current_prolog_flag(rtchecks_entry, yes).
valid_assertions(check, exit) :- !, current_prolog_flag(rtchecks_exit,  yes).
valid_assertions(trust, test) :- !,
	current_prolog_flag(rtchecks_test,  yes),
	current_prolog_flag(rtchecks_trust, yes).
valid_assertions(Status, test) :- !,
	current_prolog_flag(rtchecks_test, yes),
	rtcheck_assr_status(Status).
valid_assertions(trust, Type) :- !,
	current_prolog_flag(rtchecks_trust, yes),
	rtcheck_assr_type(Type).
valid_assertions(check, Type) :- rtcheck_assr_type(Type).

rtcheck_assr_status(true).
rtcheck_assr_status(trust).
rtcheck_assr_status(check).

rtcheck_assr_type(calls).
rtcheck_assr_type(entry).
rtcheck_assr_type(pred).
rtcheck_assr_type(test).
rtcheck_assr_type(comp).
rtcheck_assr_type(exit).
rtcheck_assr_type(success).

add_cond_decl(Body0, Body, Decl, Clauses0, Clauses) :-
	( Body0 == Body -> Clauses = Clauses0
	; Clauses = [(:- Decl)|Clauses0]
	).

proc_posponed_sentence(Clauses, M) :-
	posponed_sentence_db(F, A, Head, Body0, loc(S, LB, LE), M, Dict),
	asserta_fact(location(S, LB, LE), Ref),
	transform_sentence(F, A, Head, Body0, Body, Clauses0, M, Dict),
	add_cond_decl(Body0, Body, use_inline(F/A), Clauses0, Clauses),
	erase(Ref).

remaining_preds(Preds, M) :-
	findall(F/A, remaining_pred(F, A, M), Preds0),
	sort(Preds0, Preds).

current_assertion_2(Pred0, Status, Type, Pred, Compat, Call, Succ, Comp0,
	    Dict0, S, LB, LE, F, A, M) :-
	assertion_read(Pred0, M, Status, Type, ABody, Dict0, S, LB, LE),
	valid_assertions(Status, Type),
	functor(Pred0, F, CA),
	lit_clause_arity(M, F, A, CA),
	\+ inline_db(F, A, _, M),
	(
	    (
		current_prolog_flag(rtchecks_level, inner)
	    ;
		current_prolog_flag(rtchecks_level, exports),
		defines_module(Base, M),
		( exports_pred(Base, all, all)
		; exports_pred(Base, F, A)
		)
	    ) -> true
	;
	    fail
	),
	assertion_body(Pred, Compat, Call, Succ, Comp0, _Comm, ABody),
	\+ member(no_rtcheck(_), Comp0),
	\+ black_list_pred(F, A).

remaining_pred(F, A, M) :-
	current_assertion_2(_, _, _, _, _, _, _, _, _, _, _, _, F, A, M),
	\+ generated_rtchecks_db(F, A, M).

black_list_pred('=', 2).

proc_remaining_assertions(Preds, [(:- redefining(F/A)), (:- inline(F/A)),
		(:- inline(F1/A1))|Clauses], M, Dict) :-
	member(F/A, Preds),
	functor(Head, F, A),
	transform_sentence(F, A, Head, '$orig_call'(Head), _, Clauses, M,
	    Dict),
	head_alias_db(Head, Head1, M),
	functor(Head1, F1, A1).

rtchecks_sentence_tr(0,           _,       _, _) :- !.
rtchecks_sentence_tr(end_of_file, Clauses, M, _) :-
	!,
	findall(Clauses0, proc_posponed_sentence(Clauses0, M), ClausesL0,
	    ClausesL1),
	remaining_preds(Preds, M),
	findall(Clauses1, proc_remaining_assertions(Preds, Clauses1, M, []),
	    ClausesL1, [
		(:- pop_prolog_flag(discontiguous_warnings)),
		(:- pop_prolog_flag(multi_arity_warnings)),
		end_of_file]),
	flatten([
		(:- push_prolog_flag(discontiguous_warnings, off)),
		(:- push_prolog_flag(multi_arity_warnings,   off))|ClausesL0],
	    Clauses),
	cleanup_db_0(M).
rtchecks_sentence_tr(_, _, M, _) :-
	in_inline_module_db(_, M),
	!,
	fail.
rtchecks_sentence_tr(Sentence, Sentence0, M, Dict) :-
	( do_rtchecks_sentence_tr(Sentence, Sentence0, M, Dict) -> true
	; Sentence = Sentence0 ).

do_rtchecks_sentence_tr((Head :- Body), Clauses, M, Dict) :-
	!,
	process_sentence(Head, Body, Clauses, M, Dict).
do_rtchecks_sentence_tr((:- _Decl), _, _, _) :-
	!,
	fail.
do_rtchecks_sentence_tr(Head, Clauses, M, Dict) :-
	process_sentence(Head, true, Clauses, M, Dict).

proc_ppassertion(check(Goal), PredName, Dict, Loc, rtcheck(Goal, PredName,
		Dict, Loc)).
proc_ppassertion(trust(Goal), PredName, Dict, Loc, RTCheck) :-
	( current_prolog_flag(rtchecks_trust, yes) ->
	    RTCheck = rtcheck(Goal, PredName, Dict, Loc)
	; RTCheck = true ).
proc_ppassertion(true(_),  _, _, _, true).
proc_ppassertion(false(_), _, _, _, true).

rtchecks_goal_tr(end_of_file, _,         M) :- !, cleanup_db(M).
rtchecks_goal_tr(PPAssertion, PPRTCheck, _) :-
	proc_ppassertion(PPAssertion, PredName, [], Loc, PPRTCheck),
	location(Loc),
	PredName = PPAssertion,
	!.
rtchecks_goal_tr('$orig_call'(Goal0), Goal, M) :-
	qualify_goal(M, Goal0, Goal).
rtchecks_goal_tr('$meta$rtc'(Goal, MG), MG=RM:Goal, M) :-
	functor(Goal, F, N),
	module_qualifier_i(F, N, M, RM).
rtchecks_goal_tr(Goal, Goal1, M) :-
	goal_alias_db(Goal, Goal1, M), !.

:- comment(bug, "Currently this will have problems with multifile
	predicates. --EMM").

:- use_module(library(compiler/c_itf_internal),
	    [multifile/3, defines/3, imports/5]).

qualify_goal(M, Goal0, Goal) :-
	functor(Goal0, F, N),
	(
	    imports(M, _IM, F, N, EM) ->
	    Goal = EM:Goal0
	;
	    rename_head('0', N, Goal0, Goal) % Kludge: force compilation error
	).

module_qualifier_i(F, N, M, RM) :-
	( multifile(M, F, N) -> RM = M % multifile
	; imports(M, _IM, F, N, EM) -> RM = EM % Imported have priority
	; defines(M, F, N) -> RM = M
	; RM = M
	).

cleanup_db_0(M) :-
	cleanup_head_alias_db(M),
	retractall_fact(posponed_sentence_db(_, _, _, _, _, M, _)),
	cleanup_generated_rtchecks_db(M).

cleanup_db(M) :-
	cleanup_goal_alias_db(M),
	cleanup_db_0(M).

cleanup_head_alias_db(M) :-
	retractall_fact(head_alias_db(_, _, M)).

cleanup_goal_alias_db(M) :-
	retractall_fact(goal_alias_db(_, _, M)).

cleanup_generated_rtchecks_db(M) :-
	retractall_fact(generated_rtchecks_db(_, _, M)).

process_sentence(Head, Body0, Clauses, M, Dict) :-
	functor(Head, F, A),
	(
	    discontiguous(F, A, Base),
	    defines_module(Base, M) ->
	    location(Loc),
	    assertz_fact(posponed_sentence_db(F, A, Head, Body0, Loc, M,
		    Dict)),
	    Clauses = []
	;
	    transform_sentence(F, A, Head, Body0, Body, Clauses0, M, Dict),
	    add_cond_decl(Body0, Body, use_inline(F/A), Clauses0, Clauses)
	).

transform_sentence(F, A, Head, Body0, Body, Clauses, M, Dict) :-
	current_prolog_flag(runtime_checks, yes),
	process_body(Dict, Head, F, A, Body0, Body),
	Decl = [],
	(
	    generated_rtchecks_db(F, A, M) ->
	    head_alias_db(Head, Head1, M),
	    append(Decl, [(Head1 :- Body)], Clauses)
	;
	    location(PLoc),
	    functor(Pred, F, A),
	    collect_assertions(Pred, M, Assertions),
	    Assertions \== [],
	    current_prolog_flag(rtchecks_asrloc,  UseAsrLoc),
	    current_prolog_flag(rtchecks_predloc, UsePredLoc),
	    UsePosLoc = (UsePredLoc, UseAsrLoc),
	    generate_rtchecks(F, A, M, Assertions, Pred, Dict, PLoc,
		UsePosLoc, Pred2, Clauses1, [(Head1 :- Body)]) ->
	    record_head_alias(Pred, Pred2, M), % TODO: Optimize this literal
	    head_alias_db(Head, Head1, M),
	    assertz_fact(generated_rtchecks_db(F, A, M)),
	    append(Decl, Clauses1, Clauses)
	;
	    append(Decl, [(Head :- Body)], Clauses)
	),
	!.
transform_sentence(_, _, Head, Body, Body, [(Head :- Body)], _, _).

:- export(body_expansion/3).
:- meta_predicate body_expansion(?, pred(2), ?).

body_expansion(Goal0, P, Goal) :-
	var(Goal0),
	!,
	P(Goal0, Goal).
body_expansion((A, B), P, (NA, NB)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB).
body_expansion((A; B), P, (NA; NB)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB).
body_expansion((A->B), P, (NA->NB)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB).
body_expansion((X^A), P, (X^NA)) :-
	!,
	body_expansion(A, P, NA).
body_expansion((\+ A), P, (\+ NA)) :-
	!,
	body_expansion(A, P, NA).
body_expansion(if(A, B, C), P, if(NA, NB, NC)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB),
	body_expansion(C, P, NC).
body_expansion(Goal0, P, Goal) :-
	P(Goal0, Goal),
	!.
body_expansion(Goal, _, Goal).

ppassr_expansion(Goal, _, _, _, Goal) :-
	var(Goal),
	!.
ppassr_expansion(PPAssertion, PredName, Dict, Loc, Goal) :-
	proc_ppassertion(PPAssertion, PredName, Dict, Loc, Goal).

process_body(Dict, Pred, F, A, Body0, Body) :-
	( current_prolog_flag(rtchecks_callloc, literal) ->
	    body_expansion(Body0, calllit_expansion(Dict, PredName0, Loc0),
		Body1)
	; body_expansion(Body0, ppassr_expansion(PredName0, Dict, Loc0), Body1)
	),
	(
	    Body0 == Body1 ->
	    Body = Body1
	;
	    location(Loc),
	    current_prolog_flag(rtchecks_namefmt, NameFmt),
	    get_predname(NameFmt, Dict, Pred, PredName),
	    Terms0 = [Loc = Loc0, PredName = PredName0],
	    collapse_terms(Body1, Terms0, Terms),
	    lists_to_lits([Terms, Body1], Body),
	    functor(Pred, F, A)
	).

put_call_stack(Goal0, Pos, Goal) :-
	Goal = call_stack(Goal0, Pos).

calllit_expansion(Goal0, PDict, PredName, Loc, Goal) :-
	var(Goal0),
	!,
	current_prolog_flag(rtchecks_namefmt, NameFmt),
	get_predname(NameFmt, PDict, Goal0, LitName),
	put_call_stack(Goal0, litloc(LitName, Loc-PredName), Goal).
calllit_expansion(!,           _,     _,        _,   !) :- !.
calllit_expansion(true,        _,     _,        _,   true) :- !.
calllit_expansion(A =.. B,     _,     _,        _,   A =.. B) :- !.
calllit_expansion(call(Goal0), _,     _,        _,   call(Goal0)) :- !.
calllit_expansion(PPAssertion, PDict, PredName, Loc, PPRTCheck) :-
	proc_ppassertion(PPAssertion, PredName, PDict, Loc, PPRTCheck),
	!.
calllit_expansion(Goal0, PDict, PredName, Loc, Goal) :-
	current_prolog_flag(rtchecks_namefmt, NameFmt),
	get_predname(NameFmt, PDict, Goal0, LitName),
	put_call_stack(Goal0, litloc(LitName, Loc-PredName), Goal).

rename_head(Tag, A, Head, Head1) :-
	compound_struct(Head, F, Args),
	atom_number(NA, A),
	atom_concat([F, '/', NA, '$rtc', Tag], F1),
	compound_struct(Head1, F1, Args).

record_head_alias(Head0, Head, M) :-
	functor(Head0, F0, A),
	functor(Pred0, F0, A),
	compound_struct(Pred0, _, Args),
	functor(Head, F, _),
	compound_struct(Pred, F, Args),
	assertz_fact(head_alias_db(Pred0, Pred, M)).

record_goal_alias(Head0, Head, M) :-
	functor(Head0, F0, A),
	functor(Pred0, F0, A),
	compound_struct(Pred0, _, Args),
	functor(Head, F, _),
	compound_struct(Pred, F, Args),
	assertz_fact(goal_alias_db(Pred0, Pred, M)).


/*

Algorithm: 

pred :- body.

is transformed in:

pred :-                        \
	"check entry...",       \___________ STEP
	"check exit...",        /            ONE
	'pred$rtc0'.           /

'pred$rtc0' :-                            \
	"check compat pre..."              \
	"check calls...",                   \
	"check success pre",                 \__________ STEP
	"check comp..."(                     /           TWO
	call_stack('pred$rtc1', Loc)),      /
	"check success pos",               /
	"check compat pos..."             /

call_stack(Goal, Loc) :-
	intercept(Goal,
	    rtcheck(LocStack, ...),
	    send_signal(rtcheck([Loc|LockStack], ...))).

'pred$rtc1' :-
	body.

And goals preds are renamed to 'pred$rtc0'.  There are other steps in
order to simplify the generated code as far as possible.

*/

generate_common_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, CallAssrt, SuccAssrt, CompAssrt) -->
	compat_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, [], ChkCompatL0),
	calls_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CallAssrt, [], CheckedL0),
	success_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    SuccAssrt, CheckedL0, CheckedL1),
	compatpos_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, ChkCompatL0),
	comp_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, CompAssrt,
	    CheckedL1).

generate_step1_rtchecks(Assertions, Pred, PLoc, UsePosLoc, Goal0, Goal) :-
	do_generate_step1_rtchecks(Assertions, Pred, PLoc, UsePosLoc,
	    PosLocs0, Goal1, Goal),
	(reverse(PosLocs0, PosLocs1) -> true),
	collapse_terms(Goal1, PosLocs1, PosLocs2),
	reverse(PosLocs2, PosLocs),
	append(PosLocs, Goal1, Goal0).

do_generate_step1_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs) -->
	{
	    current_prolog_flag(rtchecks_level, Level0),
	    neg_level(Level0, Level),
	    current_prolog_flag(rtchecks_entry, Entry),
	    current_prolog_flag(rtchecks_exit,  Exit),
	    current_prolog_flag(rtchecks_trust, Trust),
	    compat_assrt(Level, no, Trust, CompatAssrt, []),
	    call_assrt(Level, Trust, Entry, CallAssrt, []),
	    succ_assrt(Level, no, Trust, Exit, SuccAssrt, []),
	    comp_assrt(Level, no, Trust, CompAssrt, [])
	},
	generate_common_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, CallAssrt, SuccAssrt, CompAssrt).

generate_step2_rtchecks(Assertions, Pred, PDict, PLoc, UsePosLoc, Goal0,
	    Goal) :-
	do_generate_step2_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs0,
	    Goal1, Goal2),
	(
	    Goal1 == Goal2 ->
	    Goal = Goal2
	;
	    current_prolog_flag(rtchecks_namefmt, NameFmt),
	    current_prolog_flag(rtchecks_callloc, CallLoc),
	    generate_callloc(CallLoc, UsePosLoc, PDict, PLoc, PosLocs0, Pred,
		NameFmt, Goal2, Goal)
	),
	(reverse(PosLocs0, PosLocs1) -> true),
	collapse_terms(Goal1, PosLocs1, PosLocs2),
	reverse(PosLocs2, PosLocs),
	append(PosLocs, Goal1, Goal0).

do_generate_step2_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs) -->
	{
	    current_prolog_flag(rtchecks_level, Level),
	    current_prolog_flag(rtchecks_trust, Trust),
	    current_prolog_flag(rtchecks_test,  Test),
	    compat_assrt(Level, Test, Trust, CompatAssrt, []),
	    call_assrt(Level, Trust, no, CallAssrt, []),
	    succ_assrt(Level, Test, Trust, no, SuccAssrt, []),
	    comp_assrt(Level, Test, Trust, CompAssrt, [])
	},
	generate_common_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, CallAssrt, SuccAssrt, CompAssrt).

% ----------------------------------------------------------------------------
neg_level(inner,   exports).
neg_level(exports, inner).
% ----------------------------------------------------------------------------
test_compat_assrt(no) --> [].
test_compat_assrt(yes) --> [(check, test)].

trust_compat_assrt(no) --> [].
trust_compat_assrt(yes) --> [(trust, pred)].

compat_assrt(exports, _,    _) --> [].
compat_assrt(inner,   Test, Trust) -->
	test_compat_assrt(Test),
	trust_compat_assrt(Trust),
	[(check, pred)].
% ----------------------------------------------------------------------------
entry_assrt(no) --> [].
entry_assrt(yes) --> [(true, entry)].

trust_call_assrt(no) --> [].
trust_call_assrt(yes) --> [(trust, calls), (trust, pred)].

level_call_assrt(exports, _) --> [].
level_call_assrt(inner,   Trust) -->
	trust_call_assrt(Trust),
	[(check, calls), (check, pred)].

call_assrt(Level, Trust, Entry) -->
	entry_assrt(Entry),
	level_call_assrt(Level, Trust).
% ----------------------------------------------------------------------------
test_succ_assrt(no) --> [].
test_succ_assrt(yes) --> [(check, test)].

trust_succ_assrt(no) --> [].
trust_succ_assrt(yes) --> [(trust, success), (trust, pred)].

level_succ_assrt(exports, _,    _) --> [].
level_succ_assrt(inner,   Test, Trust) -->
	test_succ_assrt(Test),
	trust_succ_assrt(Trust),
	[(check, success), (check, pred)].

trust_exit_assrt(no) --> [].
trust_exit_assrt(yes) --> [(trust, exit)].

exit_succ_assrt(no,  _) --> [].
exit_succ_assrt(yes, Trust) -->
	[(check, exit)],
	trust_exit_assrt(Trust).

succ_assrt(Level, Test, Trust, Exit) -->
	exit_succ_assrt(Exit, Trust),
	level_succ_assrt(Level, Test, Trust).
% ----------------------------------------------------------------------------
test_comp_assrt(no) --> [].
test_comp_assrt(yes) --> [(check, test)].

trust_comp_assrt(no) --> [].
trust_comp_assrt(yes) --> [(trust, comp), (trust, pred)].

level_comp_assrt(exports, _,    _) --> [].
level_comp_assrt(inner,   Test, Trust) -->
	test_comp_assrt(Test),
	trust_comp_assrt(Trust),
	[(check, comp), (check, pred)].

comp_assrt(Level, Test, Trust) -->
	level_comp_assrt(Level, Test, Trust).
% ----------------------------------------------------------------------------

generate_callloc2(Dict, PLoc, PosLocs, Pred, NameFmt, Body0, Body) :-
	get_predname(NameFmt, Dict, Pred, PredName),
	push_term(PLoc, PosLocs, Loc),
	!,
	put_call_stack(Body, callloc(PredName, Loc), Body0).

generate_callloc(predicate, (yes, _), Dict, Loc, PosLocs, Pred, NameFmt) -->
	!,
	generate_callloc2(Dict, Loc, PosLocs, Pred, NameFmt).
generate_callloc(_, _, _, _, _, _, _) --> [].

generate_rtchecks(Assertions, Pred, PDict, PLoc, UsePosLoc, Lits, Goal) :-
	generate_step1_rtchecks(Assertions, Pred, PLoc, UsePosLoc, Goal0,
	    Goal1),
	generate_step2_rtchecks(Assertions, Pred, PDict, PLoc, UsePosLoc,
	    Goal1, Goal),
	lists_to_lits(Goal0, Lits),
	!.

generate_rtchecks(F, A, M, Assertions, Pred, PDict, PLoc, UsePosLoc,
	    Pred2) -->
	{generate_step1_rtchecks(Assertions, Pred, PLoc, UsePosLoc, Body0,
		Body01)},
	(
	    {(Body0 \== Body01)} ->
	    {
		rename_head('0', A, Pred, Pred1),
		record_goal_alias(Pred, Pred1, M),
		Body01 = Pred1,
		lists_to_lits(Body0, Lits0)
	    },
	    [(:- use_inline(F/A)), (Pred :- Lits0)]
	;
	    {Pred = Pred1}
	),
	{generate_step2_rtchecks(Assertions, Pred, PDict, PLoc, UsePosLoc,
		Body1, Body12)},
	(
	    {
		Body1 \== Body12 ->
		rename_head('1', A, Pred, Pred2),
		Body12 = Pred2,
		lists_to_lits(Body1, Lits1),
		functor(Pred1, F1, A1)
	    },
	    [(:- use_inline(F1/A1)), (Pred1 :- Lits1)]
	;
	    {Pred1 = Pred2}
	).

current_assertion(Pred0, M,
	    assr(Pred, Status, Type, Compat, Call, Succ, Comp, Loc,
		PredName, CompatName, CallName, SuccName, CompName, Dict)) :-
	current_assertion_2(Pred0, Status, Type, Pred, Compat, Call, Succ,
	    Comp0, Dict0, S, LB, LE, _F, _A, M),
	Loc = loc(S, LB, LE),
	collapse_dups(Comp0, Comp),
	Term = n(Pred, Compat, Call, Succ, Comp),
	current_prolog_flag(rtchecks_namefmt, NameFmt),
	get_pretty_names(NameFmt, Term, Dict0, TermName, Dict),
	TermName = n(PredName, CompatName, CallName, SuccName, CompName).

assertion_pred(assr(Pred, _, _, _, _, _, _, _, _, _, _, _, _, _), Pred).

collect_assertions(Pred, M, Assertions) :-
	findall(Assertion, current_assertion(Pred, M, Assertion), Assertions),
	list(Assertions, assertion_pred(Pred)).

pre_lit(pre(ChkProp, Prop, _, Exit), cui(Prop - true, Exit, ChkProp)).

pre_fails(pre(_, _, _, Exit), cui(Exit, _, (Exit == fail))).

pre_error(pre(_, _, Error, Exit), cui(Exit, _, Error)).

compat_rtcheck(
	    assr(Pred, Status, Type, Compat, _, _, _, ALoc,
		PName, CompatNames, _, _, _, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    pre(ChkCompat, Compat,
		send_rtcheck(compat, PredName, Dict, PropName, PropDict,
		    PosLoc),
		Exit)) :-
	member((Status, Type), StatusTypes),
	\+(Compat == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_prop_args(Compat, Pred, Args),
	get_checkc(compat, Compat, Args, CompatNames, PropName-PropDict, Exit,
	    ChkCompat).

compat_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    StatusTypes, CheckedL0, CheckedL) -->
	{collect_checks(Assertions, compat_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), ChkCalls)},
	body_check_pre(ChkCalls, pre_lit, pre_fails, pre_error, collapse_prop,
	    CheckedL0, CheckedL).

calls_rtcheck(
	    assr(Pred, Status, Type, _, Call, _, _, ALoc,
		PName, _, CallNames, _, _, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    pre(ChkCall, Call,
		send_rtcheck(calls, PredName, Dict, PropName, PropDict,
		    PosLoc),
		Exit)) :-
	member((Status, Type), StatusTypes),
	\+(Call == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_prop_args(Call, Pred, Args),
	get_checkc(call, Call, Args, CallNames, PropName-PropDict, Exit,
	    ChkCall).

calls_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL0, CheckedL) -->
	{collect_checks(Assertions, calls_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), ChkCalls)},
	body_check_pre(ChkCalls, pre_lit, pre_fails, pre_error, collapse_prop,
	    CheckedL0, CheckedL).

success_call_lit(succ(ChkCall, Call, Exit, _, _),
	    cui(Call - true, Exit, ChkCall)).

success_succ_lit(succ(_, _, Exit, ChkSucc, Succ),
	    cui(Succ - Exit, _, ChkSucc)).

:- pred success_rtchecks/10 + not_fails.
success_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL0, CheckedL) -->
	{collect_checks(Assertions, success_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), CheckSuccs)},
	body_check_pos(CheckSuccs, success_call_lit, success_succ_lit,
	    collapse_prop, pos(Pred, success), CheckedL0,
	    CheckedL).

check_poscond(PosLoc, PredName, Dict, Prop, PropNames, Exit,
	    i(PosLoc, PredName, Dict, Prop, PropNames, Exit)).

success_rtcheck(
	    assr(Pred, Status, Type, _, Call, Succ, _, ALoc,
		PName, _, _, SuccNames, _, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    succ(ChkCall, Call, Exit, ChkSucc, Succ)) :-
	member((Status, Type), StatusTypes),
	\+(Succ == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_prop_args(Call, Pred, Args),
	get_checkc(call, Call, Args, Exit, ChkCall),
	check_poscond(PosLoc, PredName, Dict, Succ, SuccNames, Exit, ChkSucc).

compatpos_compat_lit(compatpos(ChkCompat, _, Compat, Exit),
	    cui(Compat - true, Exit, ChkCompat)).

compatpos_lit(compatpos(_, ChkCompatPos, Compat, Exit),
	    cui(Compat - Exit, _, ChkCompatPos)).

compatpos_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL0) -->
	{collect_checks(Assertions, compatpos_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), CheckSuccs)},
	body_check_pos(CheckSuccs, compatpos_compat_lit, compatpos_lit,
	    collapse_prop, pos(Pred, compatpos), CheckedL0, _).

compatpos_rtcheck(
	    assr(Pred, Status, Type, Compat, _, _, _, ALoc,
		PName, CompatNames, _, _, _, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    compatpos(ChkCompat, ChkCompatPos, Compat, Exit)) :-
	member((Status, Type), StatusTypes),
	\+(Compat == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_prop_args(Compat, Pred, Args),
	get_checkc(compat, Compat, Args, Exit, ChkCompat),
	check_poscond(PosLoc, PredName, Dict, Compat, CompatNames, Exit,
	    ChkCompatPos).

:- pred collapse_dups(+list, ?list) # "Unifies duplicated terms.".

collapse_dups([],            []).
collapse_dups([Comp|Comps0], Comps) :-
	collapse_dups2(Comp, Comps0, Comps).

collapse_dups2(Comp0, Comps0, Comps) :-
	select(Comp, Comps0, Comps1),
	Comp==Comp0 ->
	collapse_dups2(Comp, Comps1, Comps)
    ;
	collapse_dups3(Comps0, Comp0, Comps).

collapse_dups3([],             Comp, [Comp]).
collapse_dups3([Comp0|Comps0], Comp, [Comp|Comps]) :-
	collapse_dups2(Comp0, Comps0, Comps).

comps_to_comp_lit(Exit, Comp, Body0, Body) :-
	!,
	comps_parts_to_comp_lit(Exit, Comp, Body1, Body),
	lists_to_lits(Body1, Body0).

valid_commands([times(_, _), try_sols(_, _)]).

comps_parts_to_comp_lit(Exit, Comp0, Body0, Body) :-
	valid_commands(VC),
	difference(Comp0, VC, Comp),
	comps_to_goal(Comp, Body1, Body2),
	(
	    Body1 == Body2 ->
	    Body0 = Body
	;
	    Exit == true ->
	    Body2 = Body,
	    Body0 = Body1
	;
	    Body0 = checkif_comp(Exit, Body1, Body2, Body)
	).

get_chkcomp(Comp, Exit, PredName, Dict, PosLoc, Body0, Body) :-
	comps_to_comp_lit(Exit, Comp, Body1, Body),
	Body0 = add_info_rtsignal(Body1, PredName, Dict, PosLoc).

comp_rtcheck(
	    assr(Pred, Status, Type, _, Call, _, Comp, ALoc,
		PName, _, _, _, CompNames, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    comp(ChkCall, Call, Exit, ChkComp, Comp)) :-
	member((Status, Type), StatusTypes),
	\+(Comp == []),
	get_prop_args(Call, Pred, Args),
	get_checkc(call, Call, Args, Exit, ChkCall),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	check_poscond(PosLoc, PredName, Dict, Comp, CompNames, Exit, ChkComp).

comp_call_lit(comp(ChkCall, Call, Exit, _, _),
	    cui(Call - true, Exit, ChkCall)).

comp_comp_lit(comp(_, _, Exit, ChkComp, Comp), cui(Comp - Exit, _, ChkComp)).

compound_comp(Goal0-Goal, Goal0, Goal).

:- discontiguous body_check_comp/4.
body_check_comp([],       _,         Body,  Body) :- !.
body_check_comp(ChkComps, CheckedL0, Body0, Body) :-
	compound_rtchecks_end(comp_call_lit, collapse_prop,
	    ChkComps, CheckedL0, CompCall),
	compound_rtchecks_end(comp_comp_lit, collapse_prop,
	    ChkComps, [],        CompCompL),
	map(CompCompL, comp_to_lit, ChkComp0),
	sort(ChkComp0, ChkComp),
	comps_to_goal(ChkComp, compound_comp, CompsBody, Body),
	Body0 = [CompCall, CompsBody].

comp_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL) -->
	{collect_checks(Assertions, comp_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), ChkComps)},
	body_check_comp(ChkComps, CheckedL).

comp_to_lit(CompCompL, ChkComp-Goal) :-
	CompCompL = i(PosLoc, PredName, Dict, Comp, _CompNames, Exit),
	get_chkcomp(Comp, Exit, PredName, Dict, PosLoc, ChkComp, Goal).
