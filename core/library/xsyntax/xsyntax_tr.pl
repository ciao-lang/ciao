:- module(xsyntax_tr, [defunc/3, defunc_goal/2], [datafacts]).

:- include(library(fsyntax/ops)).

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(terms), [copy_args/3]).
:- use_module(library(terms_check), [instance/2]).

% Database to store declaration for the currently compiled module
:- data fun_eval/3.
:- data eval_arith/2.
% Flags for different extensions: % TODO: add user documentation
%  - hiord: enabled {...} syntax for higher order
%  - statevars: enable statevar expansion
%  - funhead: enable (_ := _) heads
%  - funexp: enable ~F expansion
%  - arithfunexp: arith in ~F expansion
%  - condexp: (_?_|_) conditional expressions
%  - quote: ^F to prevent expansions
%  - deffun: implicit ~ while definining a function
:- data eval_flag/2.
%
:- data fun_return/4.
:- data macro_rule/5. % notations % TODO: document

% ---------------------------------------------------------------------------
%! # Sentence translation

% defunc(FuncItem, PredItem, Module) :- PredItem is a clause, query or
% command which is equivalent to FuncItem but without functions.

% defunc(0, _, Mod) :- !, % no need for initialization
defunc(end_of_file, end_of_file, Mod) :- !,
    retractall_fact(fun_eval(_,Mod,_)),
    retractall_fact(eval_arith(Mod,_)),
    retractall_fact(macro_rule(_,Mod,_,_,_)),
    retractall_fact(eval_flag(_,Mod)),
    retractall_fact(fun_return(_,_,Mod,_)).
defunc((?- _), _, _) :- !, fail.
defunc(('SHELL' :- Decl), R, Mod) :- toplevel_decl(Decl), !, % declarations from the toplevel
    defunc_decl(Decl, _, Mod), R = ('SHELL':-true).
defunc((:- Decl), NewDecl, Mod) :- !,
    defunc_decl(Decl, NewDecl, Mod).
% TODO: the following rule seems to be redundant (jfmc)
defunc((FuncHead := FuncValOpts), Clauses, Mod) :-
    nonvar(FuncValOpts),
    FuncValOpts = (FuncVal1 | FuncValR),
    enabled_flag(funhead, Mod),
    !,
    Clauses = [Clause1 | ClauseR],
    defunc((FuncHead := FuncVal1), Clause1, Mod),
    defunc((FuncHead := FuncValR), ClauseR, Mod).
defunc(Def, Clauses, Mod) :-
    defunc_pred(Def, Clauses, Mod).

defunc_pred((FuncHead := CondFuncVal), (Head :- Body), Mod) :-
    nonvar(CondFuncVal),
    CondFuncVal = (Cond ? FuncVal),
    enabled_flag(funhead, Mod),
    !,
    make_tmp_fun_eval(FuncHead, Mod, Ref),
    defunc_funhead(FuncHead, Mod, Head, Ret_Arg, Body, RestBody),
    normalize(Cond, Mod, NCond),
    normalize(FuncVal, Mod, NFuncVal),
    delt_tmp_fun_eval(Ref),
    concat_bodies(NCond, (!, '\6\Unif_ret'(Ret_Arg, NFuncVal)), RestBody).
defunc_pred((FuncHead := FuncVal), (Head :- Body), Mod) :-
    enabled_flag(funhead, Mod),
    !,
    make_tmp_fun_eval(FuncHead, Mod, Ref),
    normalize(FuncVal, Mod, NFuncVal),
    defunc_funhead(FuncHead, Mod, Head, NFuncVal, AddBody, true),
    delt_tmp_fun_eval(Ref),
    del_last_true(AddBody, Body).
defunc_pred((FuncHead := FuncVal :- FuncBody), (Head :- Body), Mod) :-
    enabled_flag(funhead, Mod),
    !,
    make_tmp_fun_eval(FuncHead, Mod, Ref),
    defunc_funhead(FuncHead, Mod, Head, Ret_Arg, Body, RestBody),
    normalize(FuncBody, Mod, NFuncBody),
    normalize(FuncVal, Mod, NFuncVal),
    delt_tmp_fun_eval(Ref),
    concat_bodies(NFuncBody, '\6\Unif_ret'(Ret_Arg, NFuncVal), RestBody).
defunc_pred((Head :- Body), (NewHead :- NewBody), Mod) :- !,
    normalize(Body, Mod, NBody),
    defunc_head(Head, Mod, NewHead, NewBody, NBody).
defunc_pred(Head, (NewHead :- NewBody), Mod) :-
    defunc_head(Head, Mod, NewHead, Body, true),
    del_last_true(Body, NewBody).

defunc_decl(fun_eval(Spec), _, Mod) :- !,
    ( Spec = QM:F/A, functor(P, F, A) ->
        make_fun_eval(P, Mod, QM)
    ; Spec = F/A, functor(P, F, A) ->
        make_fun_eval(P, Mod, (-))
    ; is_flag(Spec, Flag, Val) ->
        set_flag(Flag, Val, Mod)
    ; function_output_arg(Spec, Fun, A, QM) ->
        asserta_fact(fun_return(Fun, A, Mod, QM)),
        make_fun_eval(Fun, Mod, QM)
    ; Spec = notation(Pattern, Val) -> % TODO: document
        asserta_fact(macro_rule(Pattern, Mod, Val, -, -))
    ; Spec = macro(Pattern, Val, SubOut, SubExpr) -> % TODO: document
        asserta_fact(macro_rule(Pattern, Mod, Val, SubOut, SubExpr))
    ; message(error, ['Invalid fun_eval specification: ',Spec])
    ).
defunc_decl(fun_return(FSpec), _, Mod) :- !,
    ( function_output_arg(FSpec, Fun, A, QM) ->
        asserta_fact(fun_return(Fun, A, Mod, QM))
    ;
        message(error, ['Invalid fun_return specification: ',FSpec])
    ).
defunc_decl(lazy(Decl), (:- lazy(LazySpec)), Mod) :- !,
    defunc_lazy_decl(Decl, LazySpec, Mod).
defunc_decl(initialization(Goal), (:- initialization(NGoal)), Mod) :- !,
    normalize(Goal, Mod, NGoal).
defunc_decl(on_abort(Goal), (:- on_abort(NGoal)), Mod) :- !,
    normalize(Goal, Mod, NGoal).

is_flag(arith(Val), arith, Val) :- nonvar(Val), valid_arith(Val).
is_flag(hiord(Val), hiord, Val) :- nonvar(Val), is_bool(Val).
is_flag(statevars(Val), statevars, Val) :- nonvar(Val), is_bool(Val).
is_flag(funhead(Val), funhead, Val) :- nonvar(Val), is_bool(Val).
is_flag(funexp(Val), funexp, Val) :- nonvar(Val), is_bool(Val).
is_flag(arithfunexp(Val), arithfunexp, Val) :- nonvar(Val), is_bool(Val).
is_flag(condexp(Val), condexp, Val) :- nonvar(Val), is_bool(Val).
is_flag(quote(Val), quote, Val) :- nonvar(Val), is_bool(Val).
is_flag(defined(Val), deffun, Val) :- nonvar(Val), is_bool(Val).

is_bool(true).
is_bool(false).

set_flag(arith, Arith, Mod) :- !,
    retractall_fact(eval_arith(Mod,_)),
    ( Arith = false -> true
    ; asserta_fact(eval_arith(Mod,Arith))
    ).
set_flag(Flag, Val, Mod) :-
    ( Val = true ->
        ( eval_flag(Flag, Mod) -> true ; assertz_fact(eval_flag(Flag, Mod)) )
    ; Val = false ->
        ( retract_fact(eval_flag(Flag, Mod)) -> true ; true )
    ; fail
    ).

enabled_flag(Flag, Mod) :- eval_flag(Flag, Mod), !.

% decl allowed in toplevel
toplevel_decl(fun_eval(_)).

defunc_lazy_decl(fun_eval(Spec), LazySpec, Mod) :- !,
    ( Spec = F/A, functor(P, F, A)  ->
        make_fun_eval(P, Mod, (-)),
        A1 is A + 1,
        LazySpec = F/A1
    ; functor(Spec, F, A), has_tilde(A, Spec, Arg) ->
        Am1 is A - 1,
        functor(Fun, F, Am1),
        asserta_fact(fun_return(Fun, Arg, Mod, (-))),
        make_fun_eval(Fun, Mod, (-)),
        LazySpec = F/A-Arg
    ; message(error, ['Invalid fun_eval specification in lazy declaration: ',Spec])
    ).
defunc_lazy_decl(fun_return(Spec), LazySpec, Mod) :- !,
    ( functor(Spec, F, A), has_tilde(A, Spec, Arg) ->
        Am1 is A - 1,
        functor(Fun, F, Am1),
        asserta_fact(fun_return(Fun, Arg, Mod, (-))),
        LazySpec = F/A-Arg
    ; message(error, ['Invalid fun_return specification in lazy declaration: ',Spec])
    ).

% Are arithmetic operation interpreted as functions?
arith_flag(Mod, ArithF) :-
    ( eval_arith(Mod, Arith) -> ArithF = Arith ; ArithF = false ).

defunc_funhead(Head, Mod, NPred, Ret_Arg, AddBody, RestBody) :-
    fun_to_pred_ret(Head, (-), Mod, Pred, Ret_Arg),
    defunc_nrf_args_of(Pred, NPred, AddBody, RestBody).

defunc_head(Head, Mod, NewHead, AddBody, RestBody) :-
    arith_flag(Mod, Arith),
    normalize_args_of(Head, Mod, Arith, NHead),
    defunc_nrf_args_of(NHead, NewHead, AddBody, RestBody).

concat_bodies(V, B, NB) :- var(V), !,
    NB = (V, B).
concat_bodies((G, Gs), B, (G, NB)) :- !,
    concat_bodies(Gs, B, NB).
concat_bodies(G, B, (G, B)).

% ---------------------------------------------------------------------------
%! # Translating terms to a normal form

normalize(T, Mod, NT) :-
    arith_flag(Mod, Arith),
    normalize_(T, Mod, Arith, NT).

normalize_(Var,_Mod,_Arith, NT) :- var(Var), !, NT = Var.
normalize_(T, _Mod, _Arith, NT) :- T = ^(X), var(X), !, NT = T. % TODO: just quote (bug caught with ISO functor/3)
normalize_(^(T), Mod, Arith, NT) :- enabled_flag(quote, Mod), !,
    normalize_args_of(T, Mod, Arith, NT).
normalize_(F, Mod, Arith, NrF) :-
    match_macro_rule(F, Mod, NF, SubOut, SubExpr), !, % TODO: detect loops?
    ( SubOut == (-) -> true
    ; normalize_(SubExpr, Mod, Arith, NSubExpr), % (treat inner macro parts)
      SubOut = NSubExpr
    ),
    normalize_(NF, Mod, Arith, NrF).
normalize_(~T, Mod, Arith, NrF) :- enabled_flag(funexp, Mod), !,
    ( var(T) ->
        NrF = '\6\Eval'(call(T, Ret_Arg), Ret_Arg) % Apply?
    ; arith_exp(T), enabled_flag(arithfunexp, Mod) ->
        ( is_inner_arith(Arith) -> NrF = NF
        ; NrF = '\6\Arit'(true, NF) % TODO: allow other arith (clpfd, etc.) in explicit '~'
        ),
        inner_arith(Arith, NArith),
        normalize_args_of(T, Mod, NArith, NF)
    ; take_qualification(T, QM, Fun),
      fun_to_pred_ret_tilde(Fun, QM, Mod, Pred0, Ret_Arg),
      add_qualification(QM, Pred0, Pred),
      NrF = '\6\Eval'(Pred, Ret_Arg)
    ).
normalize_(F, Mod, Arith, NrF) :-
    is_arith_exp(F, Arith), !,
    ( is_inner_arith(Arith) -> NrF = NF
    ; NrF = '\6\Arit'(Arith, NF)
    ),
    inner_arith(Arith, NArith),
    normalize_args_of(F, Mod, NArith, NF).
normalize_(F, Mod, Arith, NrF) :-
    is_arith_rel(F, Arith), !,
    NrF = '\6\AritRel'(Arith, NF),
    inner_arith(Arith, NArith),
    normalize_args_of(F, Mod, NArith, NF).
normalize_(F, Mod, _Arith, NrF) :-
    take_qualification(F, QM, Fun),
    nonvar(Fun),
    fun_eval(Fun, Mod, QM), !,
    fun_to_pred_ret(Fun, QM, Mod, Pred0, Ret_Arg),
    add_qualification(QM, Pred0, Pred),
    NrF = '\6\Eval'(Pred, Ret_Arg).
normalize_((A|B), Mod, _Arith, '\6\Opts'(NA, NB)) :- enabled_flag(condexp, Mod), !,
    normalize(A, Mod, NA),
    normalize(B, Mod, NB).
normalize_((A?B), Mod, _Arith, '\6\Cond'(NA,NB)) :- enabled_flag(condexp, Mod), !,
    normalize(A, Mod, NA),
    normalize(B, Mod, NB).
normalize_('\006\curly_block'(Sents), Mod, _Arith, NrF) :-
    enabled_flag(hiord, Mod),
    !,
    norm_curly_block(Sents, Mod, NF),
    norm_predabs_arity(NF, Mod, N),
    NrF = '\6\Predabs'(N, NF).
normalize_({F}, Mod, _Arith, NrF) :-
    enabled_flag(hiord, Mod),
    is_predabs(F, Mod),
    !,
    defunc_predabs(F, NF, Mod),
    norm_predabs_arity(NF, Mod, N),
    NrF = '\6\Predabs'(N, NF).
normalize_({F}, Mod, _Arith, NrF) :-
    enabled_flag(hiord, Mod),
    !,
    normalize(F, Mod, NF),
    NrF = '\6\Block'(NF).
normalize_(T, Mod, Arith, NT) :-
    normalize_args_of(T, Mod, Arith, NT).

normalize_args_of(T, Mod, Arith, NT) :-
    ( enabled_flag(statevars, Mod) -> normalize_statevars(T, T1)
    ; T1 = T
    ),
    functor(T1, F, A),
    functor(NT, F, A),
    normalize_args_(A, T1, Mod, Arith, NT).

normalize_args_(0, _, _Mod,_Arith, _ ) :- !.
normalize_args_(N, T0, Mod, Arith, T1) :-
    arg(N, T0, A0),
    arg(N, T1, A1),
    N1 is N-1,
    normalize_(A0, Mod, Arith, A1),
    normalize_args_(N1, T0, Mod, Arith, T1).

% ---------------------------------------------------------------------------

:- use_module(library(terms_vars), [varset/2]). % (for def_shvs)

% Is it a predicate (or function) abstraction? (gives arity in N)
is_predabs(B, Mod) :-
    split_shvs(B, Mod, _, A),
    ( nonvar(A), A = (A1 :- _) -> true ; A1 = A ),
    nonvar(A1),
    ( A1 = (A2 := _), enabled_flag(funhead, Mod) ->
        nonvar(A2), functor(A2, '', _)
    ; functor(A1, '', _)
    ).

% Get arity of a normalized predicate abstraction
norm_predabs_arity(B, Mod, N) :-
    split_shvs(B, Mod, _, A),
    ( nonvar(A), A = (A1 :- _) -> true ; A1 = A ),
    functor(A1, _, N).

split_shvs(F, Mod, MaybeShVs, F0) :-
    nonvar(F),
    ( F = (ShVs -> Head := R :- Body), enabled_flag(funhead, Mod) ->
        MaybeShVs = yes(ShVs), F0 = (Head := R :- Body)
    ; F = (ShVs -> Head := R), enabled_flag(funhead, Mod) ->
        MaybeShVs = yes(ShVs), F0 = (Head := R)
    ; F = (ShVs -> Head :- Body) ->
        MaybeShVs = yes(ShVs), F0 = (Head :- Body)
    ; F = (ShVs -> Head) ->
        MaybeShVs = yes(ShVs), F0 = Head
    ; MaybeShVs = no, F0 = F
    ).

% effective head for sharing (FuncHead or Head)
sh_head(F, Mod, H) :-
    nonvar(F),
    ( F = (Head := _ :- _), enabled_flag(funhead, Mod) -> H = Head
    ; F = (Head := _), enabled_flag(funhead, Mod) -> H = Head
    ; F = (Head :- _) -> H = Head
    ; H = F
    ).

% mark closure (non)shared vars (if needed)
add_shvs(yes(ShVs), (H :- B), (ShVs -> H :- B)).
add_shvs(no, HB, HB).

% Defunc for predicate (or function) abstractions
defunc_predabs(F, NF, Mod) :-
    split_shvs(F, Mod, MaybeShVs0, F0),
    defunc_pred(F0, NF0, Mod),
    def_shvs(MaybeShVs0, F0, Mod, MaybeShVs),
    add_shvs(MaybeShVs, NF0, NF).

% set default (non)shared vars (if needed)
def_shvs(no, F, Mod, MaybeShVs) :- !,
    % head variables "shadow" parent variables (share-parent excluding
    % variables in the head)
    sh_head(F, Mod, H),
    varset(H, HeadVars),
    MaybeShVs = yes(-HeadVars). % Note: yes(-[]) for share parent
def_shvs(MaybeShVs, _, _, MaybeShVs).

% Merge multiple clauses into a single one
% TODO: This is a temporary solution until multiple clauses are
% supported in PA
norm_curly_block(Sents0, Mod, NrF) :-
    split_shvs_block(Sents0, Mod, MaybeShVs0, Sents1),
    defunc_pred_sents(Sents1, Mod, Cls),
    ( check_heads(Cls, HeadN, HeadF) -> true
    ; fail % TODO: emit an error instead
    ),
    functor(Head, HeadN, HeadF),
    merge_cls(Cls, Head, Mod, Body),
    def_shvs_block(MaybeShVs0, Cls, MaybeShVs),
    add_shvs(MaybeShVs, (Head :- Body), NrF).

% (shvs only in first clause)
split_shvs_block([S0|Ss], Mod, MaybeShVs, [S|Ss]) :-
    S0 = sentence(Cl, VarNames, Singletons, Ln0, Ln1),
    split_shvs(Cl, Mod, MaybeShVs, Cl2),
    S = sentence(Cl2, VarNames, Singletons, Ln0, Ln1).

% All heads must be consistent
check_heads([], _, _).
check_heads([Cl|Cls], N, F) :-
    cl_decomp(Cl, H, _),
    functor(H, N, F),
    check_heads(Cls, N, F).

defunc_pred_sents([], _Mod, []).
defunc_pred_sents([sentence(Cl0,_,_,_,_)|Ss], Mod, [Cl|Cls]) :-
    defunc_pred(Cl0, Cl, Mod),
    defunc_pred_sents(Ss, Mod, Cls).

cl_decomp(X, _, _) :- var(X), !, fail.
cl_decomp((H :- B), H, B).
cl_decomp(H, H, true).

% Compose a single body
merge_cls([Cl], Head, Mod, Body) :- !,
    merge_cls_body(Cl, Head, Mod, Body).
merge_cls([Cl|Cls], Head, Mod, (Body ; RestBody)) :-
    merge_cls_body(Cl, Head, Mod, Body),
    merge_cls(Cls, Head, Mod, RestBody).

merge_cls_body(Cl, Head, _Mod, Body) :-
    cl_decomp(Cl, H, B),
    Body = (Head=H, B).

% set default (non)shared vars for blocks (if needed)
def_shvs_block(no, Cls, MaybeShVs) :- !,
    % head variables (from any clause) "shadow" parent variables
    get_heads(Cls, Heads),
    varset(Heads, HeadVars),
    MaybeShVs = yes(-HeadVars). % Note: yes(-[]) for share parent
def_shvs_block(MaybeShVs, _, MaybeShVs).

get_heads([], []).
get_heads([Cl|Cls], [H|Hs]) :- cl_decomp(Cl, H, _), get_heads(Cls, Hs).

% ---------------------------------------------------------------------------
% NOTE: arith expression notation propagates through the whole
%   expression (not first first functor).

is_arith_exp(F, Arith) :-
    \+ Arith = false,
    arith_exp(F).

is_arith_rel(F, Arith) :-
    \+ Arith = false,
    arith_rel(F).

is_inner_arith(tempfalse(_)).

inner_arith(false, NArith) :- !, NArith = false.
inner_arith(Arith, NArith) :- Arith = tempfalse(_), !, NArith = Arith.
inner_arith(Prev,  tempfalse(Prev)). % (tempfalse(_) for inner expressions, not expanded)

% ---------------------------------------------------------------------------

match_macro_rule(F, Mod, NF, SubOut, SubExpr) :-
    functor(F, N, A),
    functor(F0, N, A),
    macro_rule(F0, Mod, NF0, SubOut0, SubExpr0), instance(F, F0), % (pattern matching)
    !,
    F = F0, NF = NF0, SubOut = SubOut0, SubExpr = SubExpr0.

% ---------------------------------------------------------------------------

% PRE: 1st is not var
take_qualification(QM:T, QM, T) :- !.
take_qualification(T,   (-), T).

add_qualification(Q, T, T) :- Q == (-), !.
add_qualification(QM, T, QM:T).

fun_to_pred_ret_tilde(Fun,_QM, Mod, Pred, Ret_Arg) :-
    functor(Fun, F, A),
    has_tilde(A, Fun, Arg), !, % Quick check
    functor(Pred, F, A),
    arg(Arg, Pred, Ret_Arg),
    normalize_args_but(A, Arg, Fun, Mod, Pred).
fun_to_pred_ret_tilde(Fun, QM, Mod, Pred, Ret_Arg) :-
    fun_to_pred_ret(Fun, QM, Mod, Pred, Ret_Arg).

normalize_args_but(N, N, T0, Mod, T1) :- !,
    N1 is N-1,
    normalize_args(N1, T0, Mod, T1).
normalize_args_but(N, Exc, T0, Mod, T1) :-
    arg(N, T0, A0),
    arg(N, T1, A1),
    N1 is N-1,
    normalize(A0, Mod, A1),
    normalize_args_but(N1, Exc, T0, Mod, T1).

fun_to_pred_ret(Fun, QM, Mod, Pred, Ret_Arg) :-
    functor(Fun, F, A),
    A1 is A+1,
    functor(Pred, F, A1),
    ( fun_return(Fun, Arg, Mod, QM) -> true
    ; Arg = A1
    ),
    arg(Arg, Pred, Ret_Arg),
    normalize_args_fun_but(A1, Arg, Fun, Mod, Pred).

normalize_args_fun_but(N, N, T0, Mod, T1) :- !,
    N1 is N-1,
    normalize_args(N1, T0, Mod, T1).
normalize_args_fun_but(N, Exc, T0, Mod, T1) :-
    N1 is N-1,
    arg(N1, T0, A0),
    arg(N, T1, A1),
    normalize(A0, Mod, A1),
    normalize_args_fun_but(N1, Exc, T0, Mod, T1).

normalize_args(0, _, _Mod, _ ) :- !.
normalize_args(N, T0, Mod, T1) :-
    arg(N, T0, A0),
    arg(N, T1, A1),
    N1 is N-1,
    normalize(A0, Mod, A1),
    normalize_args(N1, T0, Mod, T1).

% TODO: first translate !V to \6\statevar then expand in defunc_nrf so that only goals are affected
% Expand !V arguments in atom as '\6\before'(V) and '\6\after'(V)
normalize_statevars(X, X2) :-
    ( nonvar(X),
      functor(X, _, A),
      A > 0,
      X =.. [F|Args],
      normalize_statevars_(Args, Args2),
      \+ Args == Args2 ->
        X2 =.. [F|Args2]
    ; % (no !V, discard)
      X2 = X
    ).

normalize_statevars_([], []).
normalize_statevars_([Arg|Args], ['\6\before'(V),'\6\after'(V)|Args2]) :- nonvar(Arg), Arg = (!(V)), var(V), !,
    normalize_statevars_(Args, Args2).
normalize_statevars_([Arg|Args], [Arg|Args2]) :-
    normalize_statevars_(Args, Args2).

% ---------------------------------------------------------------------------
%! # Translating normal forms to terms + goals

% defunc_nrf(Exp, NewExp, AddGoal, RestGoal) :- NewExp is an expression
% without functions which is equivalent to normal form Exp when adding goals
% AddGoal minus RestGoal.

% Assumes is/2 is imported
% PRE: If is_evaluable(Exp) do not bind to non-variable NewExp
defunc_nrf(V, V, G, G) :- var(V), !.
defunc_nrf(^^(T), ^^(T), G, G) :- !.
defunc_nrf('\6\Arit'(Arith, Fun), V, Add, Rest) :-
    defunc_nrf_args_of(Fun, NFun, Add, Rest0),
    arith_exp_eval(Arith, V, NFun, Eval),
    Rest0 = (Eval, Rest).
defunc_nrf('\6\AritRel'(_,X), X, G, G) :- !. % (non goals)
defunc_nrf('\6\Predabs'(N,X1), V, Add, Rest) :- !,
    mexp_pred_eval(N, X1, V, MExp),
    Add = (MExp, Rest).
defunc_nrf('\6\Eval'(Pred, Ret_Arg), Ret_Arg, Add, Rest) :-
    defunc_nrf_args_of(Pred, NPred, Add, Rest0),
    Rest0 = (NPred, Rest).
defunc_nrf(Opts, V, Add, Rest) :-
    Opts = '\6\Opts'(_,_), !,
    Add = (Assigns, Rest),
    defunc_nrf_opts(Opts, V, Assigns).
defunc_nrf('\6\Cond'(Cond, Val), V, Add, Rest) :- !,
    Add = ((Cond -> Assign), Rest),
    defunc_nrf_assign(Val, V, Assign).
defunc_nrf('\6\Block'(Goal), V, Add, Rest) :- !,
    Add = (('\6\block_expr'(Goal,Val),Assign), Rest),
    defunc_nrf_assign(Val, V, Assign).
defunc_nrf(T0, T1, Add, Rest) :-
    defunc_nrf_args_of(T0, T1, Add, Rest).

defunc_nrf_args_of(T, NT, Add, Rest) :-
    functor(T, F, A),
    functor(NT, F, A),
    defunc_nrf_args(A, T, NT, Add, Rest).

defunc_nrf_args(0, _, _, X, X) :- !. 
defunc_nrf_args(N, T0, T1, Add, Rest) :-
    arg(N, T0, A0),
    arg(N, T1, A1),
    N1 is N-1,
    defunc_nrf(A0, A1, NRest, Rest),
    defunc_nrf_args(N1, T0, T1, Add, NRest).

defunc_nrf_opts(A, V, (V = A)) :- var(A), !.
defunc_nrf_opts('\6\Opts'(A,B), V, (A_As ; Assigns)) :- !,
    defunc_nrf_assign(A, V, A_As),
    defunc_nrf_opts(B, V, Assigns).
defunc_nrf_opts(A, V, A_As) :-
    defunc_nrf_assign(A, V, A_As).

% TODO: rename 'assign' by 'unify'? (jfmc) (to distinguish from imperative assignment)
defunc_nrf_assign(Val, V, (V = Val)) :- var(Val), !.
defunc_nrf_assign(Val, V, Assign) :-
    is_evaluable(Val), !,
    defunc_nrf(Val, V, Assign1, true),
    del_last_true(Assign1, Assign).
defunc_nrf_assign(Val, V, Assign) :-
    defunc_nrf(Val, NVal, Assign, (V = NVal)).

% ---------------------------------------------------------------------------
%! # Goal translation, translates normal forms

% TODO: use enabled_flag(funexp,Mod) to enable/disable goal translations (see
%   compiler_oc/mexpand.pl). This cannot be easily done at this moment
%   since defunc_goal do not have access to the translation context,
%   instead if relies on reserved functors.
% TODO:[oc-merge] mexpand:defunc_exp/3 does normalize+defunc_goal

% defunc_goal(Goal, NewGoal) :- NewGoal is a goal which is equivalent to Goal
% (which is normalized) but without functions.
%
% Do not translate the following special goals (needed to treat meta args correctly)
defunc_goal('$meta_exp'(_,_,_), _) :- !, fail.
defunc_goal('\6\loop'(_,_,_,_), _) :- !, fail.
defunc_goal('\6\block_goal'(_), _) :- !, fail.
defunc_goal('\6\block_expr'(_,_), _) :- !, fail.
%
defunc_goal(^^(G), G) :- !.
defunc_goal('\6\Unif_ret'(R, Val), Goal) :-
    ( nonvar(Val),
      is_evaluable(Val) ->
        defunc_nrf(Val, R, AddGoal, true),
        del_last_true(AddGoal, Goal)
    ;
      defunc_nrf(Val, NVal, AddGoal, true),
      del_last_true((R = NVal, AddGoal), Goal)
    ).
defunc_goal('\6\Arit'(_,G), G) :- !.    % For integer/1, float/1
defunc_goal('\6\AritRel'(Arith, Rel), NewGoal) :- !,
    defunc_nrf_args_of(Rel, NRel, NewGoal, NRel2),
    arith_rel_goal(Arith, NRel, NRel2).
defunc_goal('\6\Eval'(G,X), NG) :- !,    % A predicate is like a function
    functor(G, F, A),
    A1 is A-1,
    functor(NG, F, A1),
    take_out_arg(A, G, X, NG).
defunc_goal('\6\Opts'(A,B), (A|B)) :- !. % To give a warning
defunc_goal('\6\Cond'(A,B), (A?B)) :- !. % To give a warning
defunc_goal('\6\Block'(Goal), NG) :- !,
    NG = '\6\block_goal'(Goal).
defunc_goal((U1 = U2), NewGoal) :-
    (V = U1, Fun = U2 ; V = U2, Fun = U1),
    ( nonvar(Fun),
      Fun = '\6\Eval'(_,_)
    ; var(V), nonvar(Fun),
      is_evaluable(Fun)
    ), !,
    defunc_nrf(Fun, V, AddGoal, true),
    del_last_true(AddGoal, NewGoal).
defunc_goal(QM:Goal, NewGoal) :- !,
    defunc_nrf_args_of(Goal, Goal1, NewGoal, QM:Goal1),
    NewGoal \== QM:Goal.
defunc_goal(Goal, NewGoal) :-
    defunc_nrf_args_of(Goal, Goal1, NewGoal, Goal1),
    NewGoal \== Goal.

take_out_arg(A, G, X, NG) :-
    arg(A, G, Ag),
    A1 is A-1,
    ( X == Ag ->
        copy_args(A1, G, NG)
    ;
        arg(A1, NG, Ag),
        take_out_arg(A1, G, X, NG)
    ).

is_evaluable('\6\Arit'(_,_)).
is_evaluable('\6\Eval'(_,_)).
is_evaluable('\6\Opts'(_,_)).
is_evaluable('\6\Cond'(_,_)).
is_evaluable('\6\Block'(_)).
is_evaluable('\6\Predabs'(_,_)).

make_fun_eval(P, Mod, QM) :-
    current_fact(fun_eval(P, Mod, QM)), !.
make_fun_eval(P, Mod, QM) :-
    asserta_fact(fun_eval(P, Mod, QM)).

make_tmp_fun_eval(Fun, Mod, Ref) :-
    enabled_flag(deffun, Mod), !,
    functor(Fun, F, A),
    functor(P, F, A),
    ( current_fact(fun_eval(P, Mod, (-))) ->
        Ref = []
    ; asserta_fact(fun_eval(P, Mod, (-)), Ref)
    ).
make_tmp_fun_eval(_F, _Mod, []).

delt_tmp_fun_eval([]) :- !.
delt_tmp_fun_eval(Ref) :- erase(Ref).

function_output_arg(QM:FSpec, Fun, Arg, QM) :-
    nonvar(QM), !,
    function_output_arg(FSpec, Fun, Arg, (-)).
function_output_arg(FSpec, Fun, Arg, (-)) :-
    functor(FSpec, F, A),
    has_tilde(A, FSpec, Arg),
    A1 is A-1,
    functor(Fun, F, A1).

has_tilde(N, F, Arg) :-
    N > 0,
    arg(N, F, F_N),
    N1 is N-1,
    ( F_N == (~) ->
        Arg = N,
        not_tilde(N1, F)
    ; has_tilde(N1, F, Arg)
    ).

not_tilde(0, _) :- !.
not_tilde(A, F) :-
    arg(A, F, F_A),
    F_A \== (~), !,
    A1 is A-1,
    not_tilde(A1, F).
not_tilde(_, F) :-
    message(error, ['More than one "~" marking function return argument in ',F]).

del_last_true(true, true).
del_last_true((G, Gs), NG) :-
    del_last_true_(Gs, G, NG).

del_last_true_(true, G, G).
del_last_true_((G,Gs), G0, (G0,NG)) :-
    del_last_true_(Gs, G, NG).

% ---------------------------------------------------------------------------
% Syntax tables

valid_arith(false).
valid_arith(true).
valid_arith(clpr).
valid_arith(clpq).
valid_arith(clpfd).

% TODO: Ask the compiler which terms are arithmetic expressions, do not place a table here
arith_exp(-(_)).
arith_exp(+(_)).
arith_exp(--(_)).
arith_exp(++(_)).
arith_exp(+(_,_)).
arith_exp(-(_,_)).
arith_exp(*(_,_)).
arith_exp(/(_,_)).
arith_exp(//(_,_)).
arith_exp(rem(_,_)).
arith_exp(mod(_,_)).
arith_exp(#(_,_)).
arith_exp(/\(_,_)).
arith_exp(\/(_,_)).
arith_exp(\(_)).
arith_exp(<<(_,_)).
arith_exp(>>(_,_)).
arith_exp(integer(_)).
arith_exp(truncate(_)).
arith_exp(float(_)).
arith_exp(gcd(_,_)).
arith_exp(abs(_)).
arith_exp(sign(_)).
arith_exp(float_integer_part(_)).
arith_exp(float_fractional_part(_)).
arith_exp(floor(_)).
arith_exp(round(_)).
arith_exp(ceiling(_)).
arith_exp(**(_,_)).
arith_exp(exp(_)).
arith_exp(log(_)).
arith_exp(sqrt(_)).
arith_exp(sin(_)).
arith_exp(cos(_)).
arith_exp(atan(_)).

arith_rel(_>_).
arith_rel(_>=_).
arith_rel(_<_).
arith_rel(_=<_).
arith_rel(_=\=_).
% TODO: enable only if clpq or clpr are used
arith_rel('.=.'(_,_)).
arith_rel('.>.'(_,_)).
arith_rel('.<.'(_,_)).
arith_rel('.>=.'(_,_)).
arith_rel('.=<.'(_,_)).
arith_rel('.<>.'(_,_)).
% TODO: enable only if clpfd is used
arith_rel('#='(_,_)).
arith_rel('#>'(_,_)).
arith_rel('#<'(_,_)).
arith_rel('#>='(_,_)).
arith_rel('#=<'(_,_)).
arith_rel('#\='(_,_)).

% Goal for arithmetic evaluation
arith_exp_eval(true, V, NFun, Eval) :- Eval = (V is NFun). % using engine(arithmetic)
arith_exp_eval(clpq, V, NFun, Eval) :- Eval = '.=.'(V, NFun). % using clpq
arith_exp_eval(clpr, V, NFun, Eval) :- Eval = '.=.'(V, NFun). % using clpr
arith_exp_eval(clpfd, V, NFun, Eval) :- Eval = '#='(V, NFun). % using clpfd

% Goal for arithmetic relation
arith_rel_goal(true, G, G2) :- G2=G. % (no mapped)
arith_rel_goal(clpq, G, G2) :- ( arith_rel_goal_clpqr(G, G1) -> G2 = G1 ; G2 = G ).
arith_rel_goal(clpr, G, G2) :- ( arith_rel_goal_clpqr(G, G1) -> G2 = G1 ; G2 = G ).
arith_rel_goal(clpfd, G, G2) :- ( arith_rel_goal_clpfd(G, G1) -> G2 = G1 ; G2 = G ).

arith_rel_goal_clpqr(A>B, '.>.'(A,B)).
arith_rel_goal_clpqr(A<B, '.<.'(A,B)).
arith_rel_goal_clpqr(A>=B, '.>=.'(A,B)).
arith_rel_goal_clpqr(A=<B, '.=<.'(A,B)).
arith_rel_goal_clpqr(A=\=B, '.<>.'(A,B)).

arith_rel_goal_clpfd(A>B, '#>'(A,B)).
arith_rel_goal_clpfd(A<B, '#<'(A,B)).
arith_rel_goal_clpfd(A>=B, '#>='(A,B)).
arith_rel_goal_clpfd(A=<B, '#=<'(A,B)).
arith_rel_goal_clpfd(A=\=B, '#\='(A,B)).

mexp_pred_eval(N, PA, V, '$meta_exp'(pred(N),PA,V)).
