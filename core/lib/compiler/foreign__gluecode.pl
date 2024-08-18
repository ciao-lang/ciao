:- module(_, [], [assertions, basicmodes, dcg, fsyntax, datafacts]).

% Generation of gluecode to foreign functions, processing of ttr tables

:- use_module(library(lists), [select/3]).

:- export(foreign_prototype/6).
foreign_prototype(ForeignName, Arguments, ResVar, NeedsCtx) -->
    { ResVar = [ResN], select(arg(ResN, ResTTr, _, _), Arguments, Arguments1) ->
        ResCType = ~ttr_ctype_res(ResTTr)
    ; ResCType = void, Arguments1 = Arguments
    },
    { Args0 = ~foreign_prototype_args(Arguments1) },
    { NeedsCtx = yes -> Args1 = [ctx:ciao_ctx|Args0] ; Args1 = Args0 },
    { Args1 = [] -> Args = [void] ; Args = Args1 },
    [ForeignName:function(Args, ResCType)].

foreign_prototype_args([]) := [] :- !.
foreign_prototype_args([A|As]) := [~foreign_prototype_arg(A)|~foreign_prototype_args(As)] :- !.

foreign_prototype_arg(arg(_, TTr, _, _)) := X :- X = ~ttr_ctype_call(TTr), !.
foreign_prototype_arg(_) := ciao_term.

% -----------------------------------------------------------------------------

:- use_module(engine(internals), [module_concat/3]).

:- export(interface_function_body/8).
interface_function_body(F/A, Module, ForeignName, Arguments, ResVar, NeedsCtx) -->
    % optim_comp: 'string' is required by the C code writer, and MF/A is given as PredName
    % [call('ERR__FUNCTOR', [string(~atom_codes(MF)), A])], 
    { module_concat(Module, F, MF) },
    [call('ERR__FUNCTOR', [~atom_codes(MF), A])],
    % variable declaration 
    params_apply(Arguments, t_decl),
    params_apply(Arguments, v_decl),
    params_apply(Arguments, u_decl),
    % variable initialization
    [call('CiaoDeclCtx', [ctx])],
    [call('CiaoInitCtx', [ctx])],
    [call(ciao_frame_begin_s, [ctx])],
    params_apply(Arguments, ref),
    % prolog -> c 
    params_apply(~filter_single(Arguments), check),
    params_apply(~filter_compound(Arguments), check),
    params_apply(~filter_single(Arguments), to_c),
    params_apply(~filter_compound(Arguments), to_c),
    % c call
    do_call(ForeignName, Arguments, ResVar, NeedsCtx), 
    % c -> prolog
    params_apply(Arguments, from_c),
    params_apply(Arguments, free),
    params_apply(Arguments, unify),
    [call(ciao_frame_end_s, [ctx])],
    [return('TRUE')].

filter_single([]) := [] :- !.
filter_single([X|Xs]) := [X|~filter_single(Xs)] :- X = arg(_, _, compound(_), _), !.
filter_single([_|Xs]) := ~filter_single(Xs) :- !.

filter_compound([]) := [] :- !.
filter_compound([X|Xs]) := [X|~filter_compound(Xs)] :- X = arg(_, _, single, _), !.
filter_compound([_|Xs]) := ~filter_compound(Xs) :- !.

params_apply([], _) --> !.
params_apply([X|Xs], Action) --> param_apply(Action, X), params_apply(Xs, Action).

param_apply(t_decl, X) --> !, param_apply_t_decl(X).
param_apply(v_decl, X) --> !, param_apply_v_decl(X).
param_apply(u_decl, X) --> !, param_apply_u_decl(X).
param_apply(ref, X) --> !, param_apply_ref(X).
param_apply(check, X) --> !, param_apply_check(X).
param_apply(to_c, X) --> !, param_apply_to_c(X).
param_apply(from_c, X) --> !, param_apply_from_c(X).
param_apply(free, X) --> !, param_apply_free(X).
param_apply(unify, X) --> !, param_apply_unify(X).

param_apply_v_decl(arg(N, TTr, _, _)) --> { CType = ~ttr_ctype_decl(TTr) }, !,
    [(~c(N)):CType].
param_apply_v_decl(_) --> !.

param_apply_u_decl(arg(N, TTr, _, _)) --> { _ = ~ttr_from_c(TTr) }, !,
    [(~u(N)):ciao_term].
param_apply_u_decl(_) --> !.

param_apply_t_decl(arg(N, _, _, _)) --> !,
    [(~t(N)):ciao_term].

param_apply_ref(arg(N, _, _, _)) --> [~t(N) = call(ciao_ref, [ctx, ~x(N)])].

param_apply_check(X) --> { Check = ~check_code(X) }, !,
    [if(logical(\ Check), ~exception_code(X))]. 
param_apply_check(_) --> !.

check_code(arg(N, TTr, _, _)) := call(Check, [ctx, ~t(N)]) :- Check = ~ttr_check(TTr).

exception_code(arg(N, TTr, _, _)) := X :- X = ~exception_code_2(N, ~ttr_exception(TTr)), !.
exception_code(_) := return('FALSE') :- !.

exception_code_2(N, error_in_arg(Err)) := call('ERROR_IN_ARG', [~x(N), N + 1, Err]) :- !.
exception_code_2(_, usage_fault(Msg)) := call('USAGE_FAULT', [Msg]) :- !.

param_apply_to_c(arg(N, TTr, single, _)) --> { ToC = ~ttr_to_c(TTr) }, !,
    [~c(N) = call(ToC, [ctx, ~t(N)])].
param_apply_to_c(arg(N, TTr, compound(_), _)) --> { ToC = ~ttr_to_c(TTr) }, !,
    [~c(N) = call(ToC, [ctx, ~t(N)])].
param_apply_to_c(_) --> !.

param_apply_from_c(arg(N, TTr, XN, _)) --> { FromC = ~ttr_from_c(TTr) }, !,
    [~u(N) = ~from_c_code(FromC, N, XN)].
param_apply_from_c(_) --> !.

from_c_code('=', N, single) := ~c(N) :- !.
from_c_code(FromC, N, single) := call(FromC, [ctx, ~c(N)]) :- !.
from_c_code(FromC, N, compound(LengthN)) := call(FromC, [ctx, ~c(N), ~c(LengthN)]) :- !.

param_apply_free(arg(N, TTr, _, no)) --> { Free = ~ttr_free(TTr) }, !,
    [call(Free, [~c(N)])].
param_apply_free(_) --> !.

param_apply_unify(arg(N, TTr, _, _)) --> { _ = ~ttr_from_c(TTr) }, !,
    [if(logical(\ call(ciao_unify_s, [ctx, ~u(N), ~t(N)])), return('FALSE'))].
param_apply_unify(_) --> !.

do_call(ForeignName, Arguments, ResVar, NeedsCtx) -->
    { ResVar = [ResN], select(arg(ResN, _, _, _), Arguments, Arguments1) -> true ; Arguments1 = Arguments },
    { Args0 = ~call_args(Arguments1) },
    { NeedsCtx = yes -> Args = [ctx|Args0] ; Args = Args0 },
    ( { NeedsCtx = no } -> [call('CiaoSetImplicitCtx', [ctx])] ; [] ),
    { ResVar = [N] -> Call = (~c(N) = call(ForeignName, Args)) ; Call = (call(ForeignName, Args)) },
    [call('GLUECODE_TRY', [Call])].

call_args([]) := [] :- !.
call_args([X|Xs]) := [~call_arg(X)|~call_args(Xs)] :- !.

call_arg(arg(N, TTr, _, _)) := address(~call_arg_v(N, TTr)) :- _ = ~ttr_call_cref(TTr), !.
call_arg(arg(N, TTr, _, _)) := ~call_arg_v(N, TTr) :- !.

call_arg_v(N, TTr) := ~t(N) :- \+ _ = ~ttr_ctype_decl(TTr), !.
call_arg_v(N, _) := ~c(N) :- !.

c(N) := identifier("c~d", [N]).
u(N) := identifier("u~d", [N]).
t(N) := identifier("t~d", [N]).
x(N) := call('X', [N]).

% -----------------------------------------------------------------------------
% TODO: move to other module...
% Table of type translations

:- export(ttr_ctype_res/2).
:- data ttr_ctype_res/2.
:- data ttr_ctype_call/2.
:- data ttr_ctype_decl/2.
:- data ttr_check/2.
:- data ttr_exception/2.
:- data ttr_to_c/2.
:- export(ttr_compound/2).
:- data ttr_compound/2.
:- data ttr_call_cref/2.
:- data ttr_from_c/2.
:- data ttr_free/2.

load_ttr_defs([Decl|Decls]) :- !,
    ( Decl = ttr_def(X, Ys) -> assert_ttr_def(Ys, X) ; true ), 
    load_ttr_defs(Decls).
load_ttr_defs([]) :- !.

assert_ttr_def([(Y = V)|Ys], X) :- !, assert_ttr_def_2(Y, X, V), assert_ttr_def(Ys, X).
assert_ttr_def([], _) :- !.

assert_ttr_def_2(ctype_res, X, V) :- !, asserta_fact(ttr_ctype_res(X, V)).
assert_ttr_def_2(ctype_call, X, V) :- !, asserta_fact(ttr_ctype_call(X, V)).
assert_ttr_def_2(ctype_decl, X, V) :- !, asserta_fact(ttr_ctype_decl(X, V)).
assert_ttr_def_2(check, X, V) :- !, asserta_fact(ttr_check(X, V)).
assert_ttr_def_2(exception, X, V) :- !, asserta_fact(ttr_exception(X, V)).
assert_ttr_def_2(to_c, X, V) :- !, asserta_fact(ttr_to_c(X, V)).
assert_ttr_def_2(compound, X, V) :- !, asserta_fact(ttr_compound(X, V)).
assert_ttr_def_2(call_cref, X, V) :- !, asserta_fact(ttr_call_cref(X, V)).
assert_ttr_def_2(from_c, X, V) :- !, asserta_fact(ttr_from_c(X, V)).
assert_ttr_def_2(free, X, V) :- !, asserta_fact(ttr_free(X, V)).

:- data ttr_match_0/4.

load_ttr_matchs([Decl|Decls]) :- !,
    ( Decl = ttr_match(X, (D, C, A)) -> asserta_fact(ttr_match_0(D, C, A, X)) ; true ), 
    load_ttr_matchs(Decls).
load_ttr_matchs([]) :- !.

:- export(ttr_match/4).
ttr_match(D, C, A) := TTr :- TTr = ~ttr_match_0(D, C, A), !.
ttr_match(_, _, _) := '$$any_term$$' :- !.

:- export(load_all_ttr/1).
load_all_ttr(Decls) :-
    load_ttr_matchs(Decls),
    load_ttr_defs(Decls).

:- export(clean_all_ttr/0).
clean_all_ttr :-
    retractall_fact(ttr_match_0(_, _, _, _)),
    retractall_fact(ttr_ctype_res(_, _)),
    retractall_fact(ttr_ctype_call(_, _)),
    retractall_fact(ttr_ctype_decl(_, _)),
    retractall_fact(ttr_check(_, _)),
    retractall_fact(ttr_exception(_, _)),
    retractall_fact(ttr_to_c(_, _)),
    retractall_fact(ttr_compound(_, _)),
    retractall_fact(ttr_call_cref(_, _)),
    retractall_fact(ttr_from_c(_, _)),
    retractall_fact(ttr_free(_, _)).


