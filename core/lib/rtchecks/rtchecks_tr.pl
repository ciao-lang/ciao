:- module(rtchecks_tr, [
    rtchecks_sentence_tr/4,
    rtchecks_goal_tr/3,
    collect_assertions/3,
    generate_rtchecks/7,
    generate_rtchecks/11
], [assertions, regtypes, nortchecks, nativeprops, isomodes, dcg, hiord, fsyntax, datafacts]).

:- use_module(library(hiordlib), [maplist/3]).
:- use_module(library(rtchecks/rtchecks_basic)).
:- use_module(library(rtchecks/rtchecks_meta)).

:- use_module(engine(runtime_control), [current_prolog_flag/2]).
:- use_module(library(assertions/assertions_props), [head_pattern/1]).
% see formulae, conj_to_list/2, list_to_conj/2
:- use_module(library(aggregates), [findall/3, findall/4]).
:- use_module(library(lists), [member/2, intersection/3, difference/3]).
:- use_module(library(lists), [append/3, reverse/2, select/3]).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(sort), [sort/2, keylist/1, keypair/1]).
:- use_module(library(rtchecks/term_list), [push_term/3, collapse_terms/3]).
:- use_module(library(assertions/assrt_lib), [
    assertion_read/9,
    assertion_body/7,
    comps_to_goal/3,
    comps_to_goal/4]).
:- use_module(library(compiler/c_itf), [
    discontiguous/3,
    defines_module/2,
    exports_pred/3,
    location/1,
    location/3]).

% ---------------------------------------------------------------------------

:- use_module(library(compiler/c_itf), [meta_args/2, imports_pred/7]). % TODO: refine
:- use_module(engine(meta_inc), [meta_inc_args/3]).

lit_clause_arity(M, F, LitArity, ClauseArity) :-
    ( meta_predicate(F, LitArity, Meta, M),
      meta_inc_args(Meta, LitArity, ClauseArity) ->
        true
    ; LitArity = ClauseArity
    ).

meta_predicate(F, A, PredSpec, M) :-
    multifile(M, F, A),
    functor(PredSpec, F, A),
    meta_args(multifile, PredSpec).
meta_predicate(F, A, PredSpec, M) :-
    functor(PredSpec, F, A),
    defines_module(Base, M),
    imports_pred(Base, _, F, A, _, PredSpec, _).

% ---------------------------------------------------------------------------

:- doc(author, "Edison Mera").

:- doc(module, "This module provides and implements the run-time
checking of predicate assertions transforming the procedure
definitions.

The semantic of run-time checks is explained in the paper:

@href{http://clip.dia.fi.upm.es/papers/assert-lang-disciplbook_bitmap.pdf}

The rtchecks are generated in such a way that redundant tests are
collapsed, in order to avoid overhead.

@comment{
Several instrumentation modes are provided to implement the run-time
checks. One is inline, which instrument the code directly. The other
mode is library, that use external metapredicates to do the checks,
this mode save space but could have less performance.
}

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

:- doc(bug, "Assertions cannot be defined after the predicate.
   Fix this, asking the compiler to rename heads and introduce
   instrumentation at the end of sentence translation --JF").
%% *Original bug message*: This is deliberate, since we preferred not to have to
%% process all the predicates, clauses and assertions at the end, losing the
%% locator information and confusing the debugger. In case of an assertion
%% defined later, a warning message is shown, recommending to write a
%% discontiguous directive (?). --EMM

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

:- regtype rtcheck_assr_status/1
    # "The types of assertion statuses processed by the rtchecks library".
rtcheck_assr_status(true).
rtcheck_assr_status(trust).
rtcheck_assr_status(check).

:- regtype rtcheck_assr_type/1
    # "The admissible kinds of assertions for the rtchecks library".
rtcheck_assr_type(calls).
rtcheck_assr_type(entry).
rtcheck_assr_type(pred).
rtcheck_assr_type(test).
rtcheck_assr_type(comp).
rtcheck_assr_type(exit).
rtcheck_assr_type(success).

proc_posponed_sentence(Clauses, M) :-
    posponed_sentence_db(F, A, Head, Body0, loc(S, LB, LE), M, Dict),
    asserta_fact(location(S, LB, LE), Ref),
    transform_sentence(F, A, Head, Body0, _Body, Clauses, M, Dict),
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

proc_remaining_assertions(Preds, [(:- redefining(F/A))|Clauses], M, Dict) :-
    member(F/A, Preds),
    functor(Head, F, A),
    transform_sentence(F, A, Head, '$orig_call'(Head), _, Clauses, M, Dict).

rtchecks_sentence_tr(0,           _,       _, _) :- !,
    clean_rtc_impl_db.
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
%                 get_prop_impl_mods(ModClauses),
    flatten([%ModClauses,
            (:- push_prolog_flag(discontiguous_warnings, off)),
            (:- push_prolog_flag(multi_arity_warnings,   off))|ClausesL0],
             Clauses),
    cleanup_db_0(M).
rtchecks_sentence_tr(Sentence, Sentence0, M, Dict) :-
    ( do_rtchecks_sentence_tr(Sentence, Sentence0, M, Dict) -> true
    ; Sentence = Sentence0 ).

do_rtchecks_sentence_tr((Head :- Body), Clauses, M, Dict) :-
    !,
    process_sentence(Head, Body, Clauses, M, Dict).
do_rtchecks_sentence_tr((:- Decl),[],_,_) :-
    Decl = rtc_impl(PropDef, PropImpl), !,
    PropDef  = :(_ModDef ,/(DefF ,DefA)),
    PropImpl = :(_ModImpl,/(ImplF,ImplA)),
    asserta_fact(rtc_impl(DefF, DefA, ImplF, ImplA)).
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

% --------------------------------------------- (begin) goal translation

rtchecks_goal_tr(end_of_file, _,         M) :- !,
    cleanup_db(M),
    clean_rtc_impl_db.
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
rtchecks_goal_tr('$test_entry_body'(A,B,C,D), Goal1, _M) :- !, % unittest
    test_entry_body_goal('$test_entry_body'(A,B,C,D), Goal1).
rtchecks_goal_tr(Goal, Goal1, M) :-
    goal_alias_db(Goal, Goal1, M), !.

:- doc(bug, "Currently this will have problems with multifile
    predicates. --EMM").

:- use_module(library(compiler/c_itf),
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

% ----------------------------------------------- (end) goal translation

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
        transform_sentence(F, A, Head, Body0, _Body, Clauses, M, Dict)
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

ppassr_expansion(_, _, _, Goal, Goal) :- var(Goal), !.
ppassr_expansion(PredName, Dict, Loc, PPAssertion, Goal) :-
    proc_ppassertion(PPAssertion, PredName, Dict, Loc, Goal).

process_body(Dict, Pred, F, A, Body0, Body) :-
    ( current_prolog_flag(rtchecks_callloc, literal) ->
        body_expansion(Body0, calllit_expansion(Dict, PredName0, Loc0), Body1)
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

calllit_expansion(PDict, PredName, Loc, Goal0, Goal) :-
    calllit_expansion_(Goal0, PDict, PredName, Loc, Goal).

calllit_expansion_(Goal0, PDict, PredName, Loc, Goal) :-
    var(Goal0),
    !,
    current_prolog_flag(rtchecks_namefmt, NameFmt),
    get_predname(NameFmt, PDict, Goal0, LitName),
    put_call_stack(Goal0, litloc(LitName, Loc-PredName), Goal).
calllit_expansion_(!,           _,     _,        _,   !) :- !.
calllit_expansion_(true,        _,     _,        _,   true) :- !.
calllit_expansion_(A =.. B,     _,     _,        _,   A =.. B) :- !.
calllit_expansion_(call(Goal0), _,     _,        _,   call(Goal0)) :- !.
calllit_expansion_(PPAssertion, PDict, PredName, Loc, PPRTCheck) :-
    proc_ppassertion(PPAssertion, PredName, PDict, Loc, PPRTCheck),
    !.
calllit_expansion_(Goal0, PDict, PredName, Loc, Goal) :-
    current_prolog_flag(rtchecks_namefmt, NameFmt),
    get_predname(NameFmt, PDict, Goal0, LitName),
    put_call_stack(Goal0, litloc(LitName, Loc-PredName), Goal).

rename_head(Tag, A, Head, Head1) :-
    Head =.. [F|Args],
    atom_number(NA, A),
    atom_concat([F, '/', NA, '$rtc', Tag], F1),
    Head1 =.. [F1|Args].

record_head_alias(Head0, Head, M) :-
    functor(Head0, F0, A),
    functor(Pred0, F0, A),
    Pred0 =.. [_|Args],
    functor(Head, F, _),
    Pred =.. [F|Args],
    assertz_fact(head_alias_db(Pred0, Pred, M)).

record_goal_alias(Head0, Head, M) :-
    functor(Head0, F0, A),
    functor(Pred0, F0, A),
    Pred0 =.. [_|Args],
    functor(Head, F, _),
    Pred =.. [F|Args],
    assertz_fact(goal_alias_db(Pred0, Pred, M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Algorithm:
%%
%% pred :- body.
%%
%% is transformed in:
%%
%% pred :-                     \
%%      "check entry...",       \___________ STEP
%%      "check exit...",        /            ONE
%%      'pred$rtc0'.           /
%%
%% 'pred$rtc0' :-                         \
%%      "check compat pre..."              \
%%      "check calls...",                   \
%%      "check success pre",                 \__________ STEP
%%      "check comp..."(                     /           TWO
%%      call_stack('pred$rtc1', Loc)),      /
%%      "check success pos",               /
%%      "check compat pos..."             /
%%
% TODO: LocStack outdated?
%% call_stack(Goal, Loc) :-
%%      intercept(Goal,
%%          rtcheck(LocStack, ...),
%%          send_signal(rtcheck([Loc|LocStack], ...))).
%%
%% 'pred$rtc1' :-
%%      body.
%%
%% And goals preds are renamed to 'pred$rtc0'.  There are other steps in
%% order to simplify the generated code as far as possible.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

generate_rtchecks(_F, A, M, Assertions, Pred, PDict, PLoc, UsePosLoc, Pred2) -->
    { generate_step1_rtchecks(Assertions, Pred, PLoc, UsePosLoc, Body0, Body01) },
    ( { Body0 \== Body01 } ->
        { rename_head('0', A, Pred, Pred1),
          record_goal_alias(Pred, Pred1, M),
          Body01 = Pred1,
          lists_to_lits(Body0, Lits0)
        },
        [(Pred :- Lits0)]
    ; {Pred = Pred1}
    ),
    { generate_step2_rtchecks(Assertions, Pred, PDict, PLoc, UsePosLoc, Body1, Body12) },
    ( { Body1 \== Body12 } ->
        { rename_head('1', A, Pred, Pred2),
          Body12 = Pred2,
          lists_to_lits(Body1, Lits1)
        },
        [(Pred1 :- Lits1)]
    ; {Pred1 = Pred2}
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

collect_assertions(Pred, M, Assertions) :-
    findall(Assertion, current_assertion(Pred, M, Assertion), Assertions),
    unif_pred(Assertions, Pred).

unif_pred([], _Pred).
unif_pred([A|As], Pred) :- assertion_pred(A, Pred), unif_pred(As, Pred).

assertion_pred(assr(Pred, _, _, _, _, _, _, _, _, _, _, _, _, _), Pred).

pre_lit(pre(ChkProp, Prop, _, Exit), cui(Prop - true, Exit, ChkProp)).

pre_fails(pre(_, _, _, Exit), cui(Exit, _, (Exit == fail))).

pre_error(pre(_, _, Error, Exit), cui(Exit, _, Error)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates in this code section have a common input/output pattern
% on their arguments, that can be generalized as:
%
% INPUTS:
% assr(Pred ,Status,Type,Compat,     Call,     Succ,     Comp     ,ALoc,
%      PName,            CompatNames,CallNames,SuccNames,CompNames,Dict)
%      Pred   | +head_pattern
%      Status | +rtcheck_assr_status
%      Type   | +rtcheck_assr_type
%      Compat \
%      Call    \ a list of properties from
%      Succ    / the respective assertion part
%      Comp   /
%      ALoc   | loc(+atm,+int,+int) assertion location data loc(Source,LB,LE)
%      PName  | Pred with renamed variables
%      CompatNames \
%      CallNames    \ assertion parts with renamed variables and
%      SuccNames    / corresponding dictionaries (each property term
%      CompNAmes   /  turned into PropertyName-PropertyDict pair)
%      Dict   | varnamesl/1 dictionary
%
% UsePosLoc   | (+atm,+atm) tuple where each atom is from {yes,no}
% Pred        | +head_pattern
% PLoc        | loc(+atm,+int,+int) predicate location data loc(Source,LB,LE)
% PosLocs     | list of the expanded PName goal PredName and all locations data
%             | ['$meta$rtc'(PName,PredName),
%             |   PLoc=Lp,
%             |   predloc(PredName,Lp)=Lpa,
%             |   asrloc(ALoc)=La |_]
% StatusTypes | +list of pairs (rtcheck_assr_status,rtcheck_assr_type)
%
% OUTPUT:
% pre(ChkCall, Call, SendRTC, Exit)
%     ChkCall : checkc(RTCCall, CallNames, PropName-PropDict, Exit)
%                   RTCCall  : +list of rtc_compat/2 or rtc_inst/2
%                              wrappers on props from Call
%                   PropName : -var
%                   PropDict : -var
%     SendRTC: send_rtcheck(calls, PredName, Dict, PropName, PropDict, PosLoc)
%                   PredName : -var
%                   PosLoc   : [Lpa,La]
%     Exit   : -var (variable to which on runtime assertion evaluation result
%              will be saved, of {true, false})

compat_rtcheck(
        UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
        assr(Pred, Status, Type, Compat, _, _, _, ALoc,
            PName, CompatNames, _, _, _, Dict),
        pre(ChkCompat, Compat,
            send_rtcheck(compat, PredName, Dict, PropName, PropDict,
                PosLoc),
            Exit)) :-
    member((Status, Type), StatusTypes),
    \+(Compat == []),
    insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
    get_prop_args(Compat, Pred, Args),
    get_prop_impl(Compat, compat, Pred, RtcCompat),
    get_checkc(compat, RtcCompat, Args, CompatNames, PropName-PropDict, Exit,
        ChkCompat).

compat_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
        StatusTypes, CheckedL0, CheckedL) -->
    {collect_checks(Assertions, compat_rtcheck(UsePosLoc, Pred,
                PLoc, PosLocs, StatusTypes), ChkCalls)},
    body_check_pre(ChkCalls, pre_lit, pre_fails, pre_error, collapse_prop,
        CheckedL0, CheckedL).

calls_rtcheck(
        UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
       assr(Pred, Status, Type, _, Call, _, _, ALoc,
            PName, _, CallNames, _, _, Dict),
       pre(ChkCall, Call,
         send_rtcheck(calls, PredName, Dict, PropName, PropDict, PosLoc),
       Exit)) :-
    member((Status, Type), StatusTypes),
    \+(Call == []),
    insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
    get_prop_args(Call, Pred, Args),
    get_prop_impl(Call, calls, Pred, RtcCall),
    get_checkc(call, RtcCall, Args, CallNames, PropName-PropDict, Exit,
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

check_poscond(Check, PosLoc, Pred, PredName, Dict, Prop, PropNames, Exit,
        i(PosLoc, PredName, Dict, RtcProp, PropNames, Exit)) :-
    get_prop_impl(Prop, Check, Pred, RtcProp).

success_rtcheck(
        UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
        assr(Pred, Status, Type, _, Call, Succ, _, ALoc,
            PName, _, _, SuccNames, _, Dict),
        succ(ChkCall, Call, Exit, ChkSucc, Succ)) :-
    member((Status, Type), StatusTypes),
    \+(Succ == []),
    insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
    get_prop_args(Call, Pred, Args),
    get_prop_impl(Call, calls, Pred, RtcCall),
    get_checkc(call, RtcCall, Args, Exit, ChkCall),
    check_poscond(success, PosLoc, Pred, PredName, Dict, Succ, SuccNames, Exit, ChkSucc).

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
        UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
        assr(Pred, Status, Type, Compat, _, _, _, ALoc,
            PName, CompatNames, _, _, _, Dict),
        compatpos(ChkCompat, ChkCompatPos, Compat, Exit)) :-
    member((Status, Type), StatusTypes),
    \+(Compat == []),
    insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
    get_prop_args(Compat, Pred, Args),
    get_checkc(compat, Compat, Args, Exit, ChkCompat),
    check_poscond(compatpos, PosLoc, Pred, PredName, Dict, Compat, CompatNames, Exit,
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

% TODO: try_sols should really be "max_sols", since the test assertion
%       harness tries all solutions by default.
valid_texec_comp_props([times(_, _), try_sols(_, _)]).

comps_parts_to_comp_lit(Exit, Comp0, Body0, Body) :-
    valid_texec_comp_props(VC),
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
    get_prop_impl(Comp, comp, '$none', RtcComp),
    comps_to_comp_lit(Exit, RtcComp, Body1, Body),
    Body0 = add_info_rtsignal(Body1, PredName, Dict, PosLoc).
% TODO: The third argument of get_prop_impl/4 is input but it is not
% needed.

comp_rtcheck(
        UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
        assr(Pred, Status, Type, _, Call, _, Comp, ALoc,
            PName, _, _, _, CompNames, Dict),
        comp(ChkCall, Call, Exit, ChkComp, Comp)) :-
    member((Status, Type), StatusTypes),
    \+(Comp == []),
    get_prop_args(Call, Pred, Args),
    get_prop_impl(Call, calls, Pred, RtcCall),
    get_checkc(call, RtcCall, Args, Exit, ChkCall),
    insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
    check_poscond(comp, PosLoc, Pred, PredName, Dict, Comp, CompNames, Exit, ChkComp).

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
    maplist(comp_to_lit, CompCompL, ChkComp0),
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

% ----------------------------------------------------------------------
% ------------------------------- unittest special case goal translation
% ----------------------------------------------------------------------

:- doc(bug, "There is currently no way to preserve the non-default
    values of such flags as @tt{rtchecks_predloc} and
    @tt{rtchecks_asrloc} while expanding unit tests in the module
    wrapper. Can be fixed by passing the flag manipulation
    directives to the wrapper module.").

:- pred combine_locators(YesNo,PLoc0,PredName,AsrLoc, PLoc,PosLoc)
    : atm * term * term * struct * var * var
 # "This predicate combines the locators of a predicate and its
   corresponding test assertion if specified by the @var{YesNo} flag
   that takes values @tt{yes} or @tt{no}. @var{PLoc0} can be either a
   predicate locator of the form @tt{loc(Alias,LB,LE)} (and then it is
   returned in the @var{PLoc} variable) or the atom @tt{none} (see
   @tt{unittest:do_gen_each_test_entry/7}).  @var{PredName} is a
   predicate head term. @var{AsrLoc} is the test assertion locator of
   the form @tt{asrloc(loc(ASource, ALB, ALE))}.".

combine_locators(yes,PLoc0,PredName,AsrLoc, PLoc,PosLoc) :-
    \+ PLoc0 = none, !,
    PLoc     = PLoc0,
    PredLoc  = predloc(PredName, PLoc),
    PosLoc   = [PredLoc, AsrLoc].
combine_locators(no,_,_,AsrLoc, _,[AsrLoc]) :- !.
combine_locators(_,_,_,_,_,_).

% ----------------------------------------------------------------------

:- use_module(engine(messages_basic), [messages/1]).

:- pred texec_warning(AType, GPProps, Pred, AsrLoc)
    : (atm(AType), list(GPProps), term(Pred), struct(AsrLoc))
  # "A @tt{texec} assertion cannot contain any computational properties
    except @pred{times/2} and @pred{try_sols/2}.  So if a @tt{texec}
    assertion contains any other computational property the correcponding
    test will be generated as if the assertion was of the type
    @tt{test}. The user is notified of this fact by a warning message.
    @var{AType} takes values @tt{texec} and @tt{test}, @var{GPProps}
    is a list of non-ground comp properties, @var{Pred} is a predicate
    from the test assertion and @var{AsrLoc} is the locator of the
    test assertion.".

:- doc(bug, "The warning message is not showh to the user unless the
    @tt{dump_error} unittest option is provided. Manually set the
    stream to which the message should be printed?").

texec_warning(texec, GPProps, Pred, asrloc(loc(ASource, ALB, ALE))) :-
    \+ GPProps == [], !,
    functor(Pred, F, A),
    maplist(comp_prop_to_name, GPProps, GPNames),
    Message = message_lns(ASource, ALB, ALE, warning,
            ['texec assertion for ', F, '/', A,
             ' can have only unit test commands, ',
             'not comp properties: \n', ''(GPNames),
             '\nProcessing it as a test assertion']),
    messages([Message]). % TODO: use message/2?
texec_warning(_, _, _, _).

comp_prop_to_name(C0, C) :- C0 =.. [F, _|A], C =.. [F|A].

% ----------------------------------------------------------------------

:- pred test_entry_body_goal(TestEntryBody, TestBodyGoal)
    : struct(TestEntryBody) => struct(TestBodyGoal)
 # "Given a @var{TestEntryBody} structure that contains a test assertion,
 a (possibly empty) set of assertions for the same predicate, predicate
 locator term and the path to the temporary directory where the unit test
 files are located, the body of a unit test is generated from these terms.
 See @lib{unittest} library for the details of the test generation.".

% DP part of the (test) assertions is completely ignored
% during unit test generation, although the rtchecks library
% has the necessary functionality to test it (see
% rtc_compat/2) in library(rtchecks/rtchecks_rt). --NS

test_entry_body_goal(TestEntryBody, TestBodyGoal) :-
    TestEntryBody = '$test_entry_body'(TestInfo, Assertions, PLoc0, TmpDir),
    TestInfo = testinfo(TestId, AType, Pred, ABody, ADict, ASource, ALB, ALE),
    AsrLoc   = asrloc(loc(ASource, ALB, ALE)),
    assertion_body(Pred, DP, CP, AP, GP, _, ABody),
    % split GP into GPTexec and GPProps
    intersection(GP, ~valid_texec_comp_props, GPTexec),
    difference(GP, ~valid_texec_comp_props, GPProps),
    get_prop_impl(GPTexec,_,Pred,RtcGPTexec),
    get_prop_impl(GPProps,comp,Pred,RtcGPProps),
    get_prop_impl(AP,success,Pred,RtcAP),
    get_prop_impl(CP,calls,Pred,RtcCP),
    %
    comps_to_goal(RtcGPTexec, TestBodyGoal, TestBodyGoal0),
    comps_to_goal(RtcGPProps, GPPropsGoal, GPPropsGoal0),
    %
    texec_warning(AType, GPProps, Pred, AsrLoc),
    %
    current_prolog_flag(rtchecks_namefmt, NameFmt),
    Term = n(Pred, DP, RtcCP, RtcAP, GP), % here no free variables must appear
    get_pretty_names(NameFmt, Term, ADict, TermName, DictName),
    TermName = n(PredName, _, _, RtcAPName, _),
    %
    current_prolog_flag(rtchecks_predloc, UsePredLoc),
    combine_locators(UsePredLoc,PLoc0,PredName,AsrLoc, PLoc,PosLoc),
    % compose checks for GP properties
    ( GPProps = [] ->
        GPCheckGoal = GPPropsGoal
    ; GPCheckGoal = add_info_rtsignal(GPPropsGoal, PredName, DictName, PosLoc)
    ),
    GPPropsGoal0 = Pred,
    % compose check for AP properties
    ( AP == [] ->
        APCheckGoal = GPCheckGoal
    ; get_prop_args(RtcAP, Pred, Args),
      get_checkif(success, true, PredName, DictName, RtcAP, Args, RtcAPName, [AsrLoc], APChkLit),
      APCheckGoal = (GPCheckGoal, catch(APChkLit, Ex, throw(postcondition(Ex))))
    ),
    % Generate rtchecks if needed
    ( Assertions == [] ->
        RTCheck = APCheckGoal
    ; current_prolog_flag(rtchecks_asrloc, UseAsrLoc),
      UsePosLoc = (UsePredLoc, UseAsrLoc),
      generate_rtchecks(Assertions, Pred, DictName, PLoc, UsePosLoc, RTCheck, APCheckGoal)
    ),
    TestBodyGoal0 = testing(TestId, TmpDir, ~list_to_lits(RtcCP), RTCheck).

% ----------------------------------------------------------------------
% --------------------------- code to enable custom property definitions
% ----------------------------------------------------------------------

% filled on the beginning of sentence translation, emptied at the end of
% goal translation for the module
%
%       rtc_impl(PropF,PropA,Spec,RtcF, RtcA,RtcSpec)
:- data rtc_impl/4.

clean_rtc_impl_db :-
    retractall_fact(rtc_impl(_,_,_,_)).

% ----------------------------------------------------------------------

:- pred get_prop_impl(L1,Chk,Sg,L2) : list * term * term * var => list
* term * term * list # "For every property from a list of property
terms @var{L1} either add to @var{L2} a custom implementation of this
property (e.g.  for run-time checks) if one exists or add the property
term with no changes. @var{Chk} denotes which kind of run-time check
it is coming from. Can be an @tt{atm} or @tt{var}. @var{Sg} is the
goal of the assertion from where the run-time check comes from.".

get_prop_impl([],_,_,[]).
get_prop_impl([Prop|Props],Check,Pred,Tail) :-
    get_prop_impl_(Prop,Check,Pred,RtcProp),
    Tail = [RtcProp | NewTail],
    get_prop_impl(Props,Check,Pred,NewTail).

get_prop_impl_(Prop, Check, _, RtcProp) :-
     functor(Prop, F, A),
     % consulting the internal database of props and versions
     rtc_impl(F, A, RtcF, A), %, Spec)
     \+(compat_check(Check)), !,
     Prop     =.. [F | Args],
     RtcProp0 =.. [RtcF | Args],
     wrap_rtc_impl(Check,RtcProp0,RtcProp).
%        add_rtc_impl_mod(Spec)

get_prop_impl_(mshare(Sh),_,Pred,succeeds(rtc_mshare(Vs2,Sh2))) :- !,
    varset(Pred,Vs),
    mshare_tr(Vs,Sh,Vs2,Sh2).
get_prop_impl_(Prop,_,_,Prop).

% IC: The property mshare/1 needs to be treated as a special case for
% two reasons:
%
%  a) The first one is that it is a property about all the variables
%     in the goal of the assertion, whether or not they appear in the
%     property itself (in the sharing sets), and therefore we need
%     those possibly missing variables to perform the runtime
%     check. See T307. To access the whole set of those variables, we
%     have introduced a new argument in get_prop_impl/4, the goal of
%     the assertion (3rd argument), which is only used for this and
%     has been propagated through other predicates to be available
%     here.
%
%  b) The other reason is that the translation between the property
%     and its runtime-check version is not as straighforward as with
%     other properties with runtime-check versions declared. In those
%     others we only change the functor of the property, and keep the
%     arguments as they are. In this case, for correctness and
%     efficiency, we need to modify the arguments of the property.

% TODO: (a) remove if T307 is fixed, as well as Pred arg in
% get_prop_impl/4
:- use_module(library(terms_vars), [varset/2]).

% TODO: (b) rtc_impl declaration do not support syntactic translations
% beyond predicate renaming, so we do the translation here (this could
% be generalized as special compilation patterns, meta-predicates,
% custom translation rules, etc).

mshare_tr(Vs,Sh,Vs2,Sh2) :- % list(var) * list(list(var)) * var * var
    copy_term(Vs-Sh,Vs0-Sh0),
    number_vars(Vs0,1),
    sort(Sh0,Sh2),
    zip(Vs0,Vs,Vs2).

number_vars([N|Vs],N) :-
    N1 is N+1,
    number_vars(Vs,N1).
number_vars([],_).

zip([A|As],[B|Bs],[A-B|ABs]) :-
    zip(As,Bs,ABs).
zip([],[],[]).

compat_check(compat).
compat_check(compatpos).

wrap_rtc_impl(calls,Prop0,Prop)   :- !, Prop = succeeds(Prop0).
wrap_rtc_impl(success,Prop0,Prop) :- !, Prop = succeeds(Prop0).
wrap_rtc_impl(_      ,Prop ,Prop).

%% ----------------------------------------------------------------------
%% the code below has been desactivated since currently all rtc-modules
%% for system properties are added by this translation to the source file
%% without any optimizations.
%%
%add_rtc_impl_mod(Spec) :-
%        rtc_impl_mod(Spec),!.
%add_rtc_impl_mod(Spec) :-
%        asserta_fact(rtc_impl_mod(Spec)).
%
%get_prop_impl_mods(ModClauses) :-
%        findall(Spec, rtc_impl_mod(Spec), Specs),
%        % cannot use maplist/3 like
%        %     maplist((''(X,Y) :- Y = (:- use_module(X))),Specs,ModClauses)
%        % because
%        %    {Compiling .../ciao-devel/core/lib/rtchecks/rtchecks_tr.pl
%        %    ERROR: (lns 971-977) Predicate (:-)/4 undefined in source
%        %    ERROR: (lns 971-977) Predicate (:-)/4 undefined in source
%        %    ERROR: Aborted module compilation
%        spec_to_drv(Specs,ModClauses).
%
%spec_to_drv([], []).
%spec_to_drv([Spec|Specs],Tail) :-
%        Tail = [(:- use_module(Spec))|NewTail],
%        spec_to_drv(Specs,NewTail).
%
%% ----------------------------------------------------------------- TODO
%% improve the internal database of custom property versions like
%%
%% :- rtc_impl(native_props:is_det/1,
%%             library(assertions/native_props_rtc), rtc_is_det/1).
%%
%% so it can be accessed as
%%
%% sent((:- rtc_impl(M:F/A, Spec, F2/A2)), ...).
