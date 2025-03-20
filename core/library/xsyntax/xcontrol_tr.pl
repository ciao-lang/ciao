:- module(_, [], [assertions, nortchecks, isomodes]).

%! \title Low-level support for xsyntax
%  \author Jose F. Morales
%
%  \module This expansion module is in charge of:
%
%    - threading state variables (expanding `'\6\before'` and `'\6\after'` terms)
%    - dealing with `'\6\assign'/2`
%    - expanding `'\6\loop'/5`
%    - turn explicit PAEnv unification into new `$pa_def/4` (optimized)
%      (see `old_pa_def_lit/2`; % TODO: make mexpand.pl generate this directly)
%    - enable support for PA with shared-by-default variables
%      (`-[X1...Xn] -> Head :- Body`), where all common variables are
%      shared except `X1...Xn`.
%
%  Do not use directly, use `xsyntax` with `statevars(true)` or
%  `hiord(true)` instead.

:- use_module(library(terms_vars)).
:- use_module(library(sort)).
:- use_module(library(lists), [append/3, length/2]).

% ---------------------------------------------------------------------------
%! # Translation (mexp)

:- export(tr_clause/3).
tr_clause(Cl, Cl2, M) :- Cl = clause(_, _),
    tr_clause_(Cl, M, [], clause(Head,Body)),
    anot_linking_vars(Head, Body, Body2),
    Cl2 = clause(Head,Body2).

tr_clause_(clause(H, B), M, ParentEnv, clause(H2, B2)) :-
    atom_upds(H, H2, Env0_, Env),
    % (inherit from ParentEnv, just for in state vars)
    p_env__upd_out(Env0_, ParentEnv, Env0),
    %
    an_body(B, Ba, [], _), % annotate shared statevars in loops
    tr_body(Ba, B1, M, Env0, Env1),
    p_env__finish(Env0, Env1, Env, B1, B2).

tr_body(X, X, _M, Env0, Env0) :- var(X), !.
tr_body('basiccontrol:\\+'(X), G, M, Env0, Env) :- !,
    tr_body('basiccontrol:;'('basiccontrol:->'(X,'basiccontrol:fail'),'basiccontrol:true'), G, M, Env0, Env).
tr_body('basiccontrol:,'(X,Y), 'basiccontrol:,'(X1,Y1), M, Env0, Env) :- !,
    tr_body(X, X1, M, Env0, Env1),
    tr_body(Y, Y1, M, Env1, Env).
tr_body('basiccontrol:->'(X,Y), 'basiccontrol:->'(X1,Y1), M, Env0, Env) :- !,
    tr_body(X, X1, M, Env0, Env1),
    tr_body(Y, Y1, M, Env1, Env).
tr_body('basiccontrol:;'(X,Y), 'basiccontrol:;'(X2,Y2), M, Env0, Env) :- !,
    tr_body(X, X1, M, Env0, StX),
    tr_body(Y, Y1, M, Env0, StY),
    p_env__meet(Env0, StX, StY, X1, X2, Y1, Y2, Env).
tr_body('xcontrol_rt:\6\assign'(Var,Val), G, _M, Env0, Env) :- !,
    replace_var(Val, Env0, Val2),
    p_env__set(Var, S, Env0, Env),
    G = 'term_basic:='(S,Val2).
%
tr_body('xcontrol_rt:\6\stpa_def'(PAMode, NegShVs, Args, Body, PA), G, M, Env0, Env) :- !,
    % TODO: weak? needed because we need to extract negative sharing once statevars has been expanded
    % TODO: added NegShVs to specify extra negative sharing
    Env = Env0,
    H =.. [''|Args],
    tr_clause_(clause(H, Body), M, Env0, clause(H2, B2)),
    varset((NegShVs,H2), NegShVs2),
    G = 'hiord_rt:$pa_def'(PAMode, -NegShVs2, clause(H2, B2), PA). % (negative sharing, see anot_linking_vars/3)
tr_body(G0, G, M, Env0, Env) :- old_pa_def_lit(G0, G1), !, % TODO: make mexpand.pl generate $pa_def/6 directly!
    tr_body(G1, G, M, Env0, Env).
tr_body(G0, G, M, Env0, Env) :- G0 = 'hiord_rt:$pa_def'(PAMode, ShVs, PACode, V), !,
    Env = Env0,
    tr_clause_(PACode, M, Env0, PACode2),
    G = 'hiord_rt:$pa_def'(PAMode, ShVs, PACode2, V).
%
tr_body('hiord_rt:call'(PA), G, M, Env0, Env) :- !,
    tr_body('hiord_rt:call'(PA,''), G, M, Env0, Env). % (so that we can optimize it later)
tr_body('hiord_rt:call'(PA,Args0), G, M, Env0, Env) :- !,
    tr_body(Args0, Args, M, Env0, Env), % (Args0 = ''(...) so we treat as a goal)
    G = 'hiord_rt:call'(PA,Args).
tr_body('hiord_rt:$pa_call'(PA,Args0), G, M, Env0, Env) :- !,
    tr_body(Args0, Args, M, Env0, Env), % (Args0 = ''(...) so we treat as a goal)
    G = 'hiord_rt:$pa_call'(PA,Args).
%
tr_body('xcontrol_rt:\6\shloop'(NegShVs, StLoopVs, Init, Cond, Next, Goal), G, M, Env0, Env) :- !,
    tr_loop(NegShVs, StLoopVs, Init, Cond, Goal, Next, G, M, Env0, Env).
%
tr_body(G, G2, _M, Env0, Env) :- !,
    atom_upds(G, G1, EnvIn, EnvOut),
    replace_var(G1, Env0, G2),
    p_env__upd_in(EnvIn, Env0),
    p_env__upd_out(EnvOut, Env0, Env).

% Update Env (in)
p_env__upd_in([], _Env).
p_env__upd_in([v(V,VIn)|Upds], Env) :-
    replace_var(V, Env, VIn),
    p_env__upd_in(Upds, Env).

% Update Env (out)
p_env__upd_out([], Env, Env).
p_env__upd_out([v(V,VOut)|Upds], Env0, Env) :-
    p_env__set(V, VOut, Env0, Env1),
    p_env__upd_out(Upds, Env1, Env).

% ---------------------------------------------------------------------------

% Expand '\6\before'(V) and '\6\after'(V)
% Obtain EnvIn and EnvOut for state variable connections.
atom_upds(Head, Head2, EnvIn, EnvOut) :-
    ( Head =.. [F|Args],
      atom_args_upds(Args, Args2, EnvIn, EnvOut),
      \+ ( EnvIn = [] ; EnvOut = [] ) ->
        Head2 =.. [F|Args2]
    ; % (no !V, discard)
      Head2 = Head,
      EnvIn = [],
      EnvOut = []
    ).

% TODO: indeed we'd only need to replace '\6\after'
atom_args_upds([], [], [], []).
atom_args_upds([Arg|Args], [Arg2|Args2], EnvIn, EnvOut) :- 
    ( nonvar(Arg), Arg = '\6\before'(V) ->
        Arg2 = VIn, EnvIn = [v(V,VIn)|EnvIn0], EnvOut = EnvOut0
    ; nonvar(Arg), Arg = '\6\after'(V) ->
        Arg2 = VOut, EnvIn = EnvIn0, EnvOut = [v(V,VOut)|EnvOut0]
    ; Arg2 = Arg, EnvIn = EnvIn0, EnvOut = EnvOut0
    ),
    atom_args_upds(Args, Args2, EnvIn0, EnvOut0).

% ---------------------------------------------------------------------------

p_env__vars([], []).
p_env__vars([v(V, _)|Env], [V|Vs]) :- p_env__vars(Env, Vs).

st_fresh([], []).
st_fresh([V|Vs], [v(V, _)|Env]) :-
    st_fresh(Vs, Env).

% ---------------------------------------------------------------------------

p_env__finish(Env0, Env1, Env, A1, A2) :-
    p_env__finish_diff(Env0, Env1, Env, Eq),
    add_unifs(Eq, A1, A2).

% ---------------------------------------------------------------------------

p_env__finish_diff(_, [], _, []) :- !.
p_env__finish_diff(Env0, [v(V, Tail)|Env1], Env, Eq) :-
    ( p_env__get(V, Env, S) -> % needs out connection
        ( p_env__get(V, Env0, S0),
          Tail\==S0 ->
            S=Tail, % state changed, unify S and Tail at compile time
            Eq=EqRest
        ; % state did not change, add explicit unification to connect
          Eq=['term_basic:='(S,Tail)|EqRest]
        )
    ; Eq=EqRest % ignore otherwise
    ),
    p_env__finish_diff(Env0, Env1, Env, EqRest).

% ---------------------------------------------------------------------------

p_env__meet(Env0, StA, StB, A1, A2, B1, B2, Env) :-
    sort(Env0, SEnv0),
    sort(StA, SStA),
    sort(StB, SStB),
    p_env__diff(SEnv0, SStA, SStB, Env, EqA, EqB),
    add_unifs(EqA, A1, A2),
    add_unifs(EqB, B1, B2).

% TODO: merge with optim-comp p_env__diff
p_env__diff([], [], [], [], [], []) :- !.
p_env__diff([v(V, Val0)|Env0], [v(Va, ValA)|StA], [v(Vb, ValB)|StB], StC, EqA, EqB) :- Va == Vb, V == Va, !,
    ( Val0 == ValA, Val0 == ValB -> % unchanged in A and B
        StC = [v(V, Val0)|StC0], EqA = EqA0, EqB = EqB0
    ; Val0 == ValA -> % unchanged in A
        StC = [v(V, ValB)|StC0],
        EqA = ['term_basic:='(ValB, Val0)|EqA0],
        EqB = EqB0
    ; Val0 == ValB -> % unchanged in B
        StC = [v(V, ValA)|StC0],
        EqA = EqA0,
        EqB = ['term_basic:='(ValA, Val0)|EqB0]
    ; % changed in A and B
      StC = [v(V, ValA)|StC0],
      ValA = ValB, % unify both new values
      EqA = EqA0, EqB = EqB0
    ),
    p_env__diff(Env0, StA, StB, StC0, EqA0, EqB0).
% (fill missing entries as v(V,V))
p_env__diff(Env0, StA, StB, StC, EqA, EqB) :- StA = [v(Va, _)|_], StB = [v(Vb, _)|_], Va @< Vb, !,
    p_env__diff(Env0, StA, [v(Va, Va)|StB], StC, EqA, EqB).
p_env__diff(Env0, StA, StB, StC, EqA, EqB) :- StA = [v(Va, _)|_], StB = [], !,
    p_env__diff(Env0, StA, [v(Va, Va)|StB], StC, EqA, EqB).
p_env__diff(Env0, StA, StB, StC, EqA, EqB) :- StA = [v(Va, _)|_], StB = [v(Vb, _)|_], Va @> Vb, !,
    p_env__diff(Env0, [v(Vb, Vb)|StA], StB, StC, EqA, EqB).
p_env__diff(Env0, StA, StB, StC, EqA, EqB) :- StA = [], StB = [v(Vb, _)|_], !,
    p_env__diff(Env0, [v(Vb, Vb)|StA], StB, StC, EqA, EqB).
p_env__diff(Env0, StA, StB, StC, EqA, EqB) :- StA = [v(Va, _)|_], StB = [v(Vb, _)|_], Va == Vb, !,
    p_env__diff([v(Va, Va)|Env0], StA, StB, StC, EqA, EqB).

% ---------------------------------------------------------------------------

% Add unifications in the right place (see later)
% TODO: see optim-comp

% TODO: what is the right place?
add_unifs([], G0, G) :- !, G = G0.
add_unifs(Unifs, G0, G) :-
    add_unifs__1(G0, G, Unifs).

% Repeat unifs for each branch
add_unifs__1('basiccontrol:;'(A,B), G, Unifs) :- !,
    G = 'basiccontrol:;'(A2,B2),
    add_unifs__1(A, A2, Unifs),
    add_unifs__1(B, B2, Unifs).
add_unifs__1('basiccontrol:->'(A,B), G, Unifs) :- !,
    G = 'basiccontrol:->'(A,B2),
    add_unifs__2(B, B2, Unifs). % TODO: add after cut here? maybe... because we are connecting ctx vars in head and cut affects the clause, not this if-then-else
add_unifs__1(G0, G, Unifs) :-
    add_unifs__2(G0, G, Unifs).

add_unifs__2(G0, G, Unifs) :-
    mconj_to_list(G0, GList),
    ( split_at_cut(GList, PreCut, PostCut) ->
        % Add after cut
        add_unifs__and(PostCut, PostCut2, Unifs),
        append(PreCut, PostCut2, GList2),
        list_to_mconj(GList2, G)
    ; % No visible cut (may be deeper)
      add_unifs__and(GList, GList2, Unifs),
      list_to_mconj(GList2, G)
    ).

% Skip user unifications
add_unifs__and([X|Xs], Ys, Unifs) :- X = 'term_basic:='(_,_), !,
    Ys = [X|Ys0], add_unifs__and(Xs, Ys0, Unifs).
add_unifs__and(Xs, Ys, Unifs) :- add_unifs__and__2(Unifs, Xs, Ys).

add_unifs__and__2([], Xs, Ys) :- !, Ys = Xs.
add_unifs__and__2([U|Us], Xs, [U|Ys]) :- add_unifs__and__2(Us, Xs, Ys).

split_at_cut(['basiccontrol:!'|As], Pre, Post) :- !,
    Pre = ['basiccontrol:!'], Post = As.
split_at_cut([A|As], Pre, Post) :- !,
    Pre = [A|Pre0],
    split_at_cut(As, Pre0, Post).

% conjunction to list
mconj_to_list(A, Xs) :-
    mconj_to_list_(A, Xs, []).

mconj_to_list_(A, Xs, Xs0) :- var(A), !, Xs = [A|Xs0].
mconj_to_list_('basiccontrol:,'(A,B), Xs, Xs0) :- !,
    mconj_to_list_(A, Xs, Xs1),
    mconj_to_list_(B, Xs1, Xs0).
mconj_to_list_('basiccontrol:true', Xs, Xs0) :- !, Xs = Xs0.
mconj_to_list_(A, [A|Xs], Xs).

list_to_mconj([], 'basiccontrol:true') :- !.
list_to_mconj([X], X) :- !.
list_to_mconj([X|Xs], 'basiccontrol:,'(X,Y)) :- list_to_mconj(Xs, Y).

:- use_module(library(terms_vars), [term_variables/2]).

% (translate from old to new $pa_def)
% (see mexpand:pred_expansion_pa/7)
old_pa_def_lit(G, NewG) :-
    nonvar(G), G = 'term_basic:='(A,V),
    nonvar(A), A = '$:'(A1),
    nonvar(A1), A1 = 'PAEnv'(ShVs, PA),
    nonvar(PA), PA = 'PA'(PASh,Head,Body),
    PASh = ShVs, % unify, it will be renamed again later
    % TODO: use ''(...) for ShVs? more efficient (both positive and negative)
    ( nonvar(ShVs), ShVs = -(_) -> ShVs2 = ShVs % (already a list)
    ; term_variables(ShVs, ShVs2)
    ),
    NewG = 'hiord_rt:$pa_def'(dynamic, ShVs2, clause(Head, Body), V).

% ---------------------------------------------------------------------------
%! # Letvar and blocks

% TODO: native implementation! implement in this way:
%  - make it part of tr_body (see optim-comp blocks)
%  - Env should be a stack of envs, this pushes a new one
%  - 'let' adds variables in the most recent env
%  - FIX loop scope issue:
%    - try to use this (?)
%    - use anot_linking_vars/3 parent scope for shared loop vars (write examples first)
%    - (blocks do not use parent scope)
% TODO: implement interaction with statevars
% TODO: handle errors

tr_block_goal(Goal, Goal2) :-
    tr_block_expr(Goal, _, Goal2). % (just ignore return val) % TODO: error/warning?

tr_block_expr(Goal, RetVar, Goal2) :-
    mconj_to_list(Goal, Xs),
    collect_let(Xs, Xs2, [], Vs, RetVar),
    rename_vars(Vs, Xs2, Xs3),
    list_to_mconj(Xs3, Goal2).

collect_let([], [], Vs, Vs, _RetVar).
collect_let([X|Xs], [Y|Ys], Vs0, Vs, RetVar) :-
    ( X = 'xcontrol_rt:\6\letvar'(V, Val) -> Vs1 = [V|Vs0], Y = 'term_basic:='(V,Val) % TODO: reuse <- ?
    ; X = 'xcontrol_rt:\6\return'(V) -> Vs1 = Vs0, Y = 'term_basic:='(RetVar,V) % TODO: errors?
    ; Vs1 = Vs0, Y = X
    ),
    collect_let(Xs, Ys, Vs1, Vs, RetVar).

% ---------------------------------------------------------------------------
%! # Loops
%
% Loops have the form:
%
%   <<begin>> :- Init, <<loop>>.
%   <<loop>> :-
%     ( Cond ->
%         Goal, Next, <<loop>>
%     ; true
%     ).
%
% `StLoopVs` are the loop state variables.

% TODO: merge with optimcomp mexpand__loop
% TODO: missing 'break' (early return) and 'continue' statements
% TODO: add NegCond for declarative loops without cut

% optim_loops :- fail.
optim_loops. % NOTE: closures with loops are cyclic terms! see prune_recpa in pl2wam

tr_loop(NegShVs, StLoopVs, Init, Cond, Goal, Next, G, M, Env0, Env) :-
    bangvars(StLoopVs, LoopArgs),
    LoopArgsP =.. [''|LoopArgs],
    ( optim_loops -> PAMode = static ; PAMode = dynamic ),
    BeginDef = 'xcontrol_rt:\6\stpa_def'(
        PAMode,
        [],
        LoopArgs,
        'basiccontrol:,'(
            Init, 
            'basiccontrol:,'(
                LoopDef,
                'hiord_rt:$pa_call'(LoopPA,LoopArgsP)
            )
        ),
        BeginPA),
    LoopDef = 'xcontrol_rt:\6\stpa_def'(
        PAMode,
        NegShVs, % variables local to loop iteration 
        LoopArgs,
        'basiccontrol:;'(
            'basiccontrol:->'(
                Cond,
                'basiccontrol:,'(Goal, 'basiccontrol:,'(Next, 'hiord_rt:$pa_call'(LoopPA,LoopArgsP))) 
            ),
            'basiccontrol:true'
        ),
        LoopPA),
    tr_body('basiccontrol:,'(
        BeginDef,
        'hiord_rt:$pa_call'(BeginPA,LoopArgsP)
    ), G, M, Env0, Env).

bangvars([], []).
bangvars([V|Vs], ['\6\before'(V),'\6\after'(V)|As]) :- bangvars(Vs,As).

% ---------------------------------------------------------------------------
%! # Envs

% :- use_module(library(dict)). % TODO: use instead of lists
:- use_module(library(lists), [select/3]).

p_env__get(V, Env, S) :- member(v(V0, S0), Env), V0==V, !, S = S0.

p_env__set(V, S, Env0, Env) :-
    ( select(v(V0, _), Env0, Env1), V0==V ->
        Env = [v(V,S)|Env1]
    ; Env = [v(V,S)|Env0]
    ).

% ---------------------------------------------------------------------------
% Replace variables in terms

replace_var(X, Env, X2) :- var(X), !,
    ( p_env__get(X, Env, S) -> X2 = S
    ; X2 = X
    ).
replace_var(X, _, X2) :- atomic(X), !, X2 = X.
replace_var(X, Env, X2) :-
    functor(X, F, A),
    functor(X2, F, A),
    replace_var_args(1, A, X, Env, X2).

replace_var_args(I, A, _X, _Env, _X2) :- I > A, !.
replace_var_args(I, A, X, Env, X2) :-
    arg(I, X, Xi),
    arg(I, X2, X2i),
    replace_var(Xi, Env, X2i),
    I1 is I + 1,
    replace_var_args(I1, A, X, Env, X2).

% ===========================================================================
%! # Annotate statevars
%
% Annotate shared statevars in '\6\loop'/5 constructs, as '\6\shloop'/6.

% StVs are the statevars used in goal X
% TODO: improve it! proper lifetime analysis?
% TODO: this does block_goal and block_expr expansion here because
%   they do var renaming

an_body(X, X, StVs0, StVs0) :- var(X), !.
an_body('basiccontrol:\\+'(X), 'basiccontrol:\\+'(X1), StVs0, StVs) :- !,
    an_body(X, X1, StVs0, StVs).
an_body('basiccontrol:,'(X,Y), 'basiccontrol:,'(X1,Y1), StVs0, StVs) :- !,
    an_body(X, X1, StVs0, StVs1),
    an_body(Y, Y1, StVs1, StVs).
an_body('basiccontrol:->'(X,Y), 'basiccontrol:->'(X1,Y1), StVs0, StVs) :- !,
    an_body(X, X1, StVs0, StVs1),
    an_body(Y, Y1, StVs1, StVs).
an_body('basiccontrol:;'(X,Y), 'basiccontrol:;'(X1,Y1), StVs0, StVs) :- !,
    an_body(X, X1, StVs0, StVs1),
    an_body(Y, Y1, StVs1, StVs).
%
% (do block_goal and block_expr expansion here) % TODO: improve
an_body('xcontrol_rt:\6\block_goal'(Goal), G, StVs0, StVs) :- !,
    tr_block_goal(Goal, Goal2),
    an_body(Goal2, G, StVs0, StVs).
an_body('xcontrol_rt:\6\block_expr'(Goal, V), G, StVs0, StVs) :- !,
    tr_block_expr(Goal, V, Goal2),
    an_body(Goal2, G, StVs0, StVs).
%
% (no statevars can be shared in PA)
an_body(G0, G, StVs0, StVs) :- G0 = 'xcontrol_rt:\6\stpa_def'(_, _, _, _, _), !,
    G = G0, StVs = StVs0.
an_body(G0, G, StVs0, StVs) :- old_pa_def_lit(G0, G1), !, % TODO: make mexpand.pl generate $pa_def/4 directly!
    an_body(G1, G, StVs0, StVs).
an_body(G0, G, StVs0, StVs) :- G0 = 'hiord_rt:$pa_def'(_, _, _, _), !,
    G = G0, StVs = StVs0.
%
an_body('xcontrol_rt:\6\loop'(NegShVs, Init, Cond, Next, Goal), G, StVs0, StVs) :- !,
    % TODO: do linkingvars with state so that BeginArgs is more refined than LoopArgs
    an_body(Init, Init2, [], StLoopVs0),
    an_body(Cond, Cond2, StLoopVs0, StLoopVs1),
    an_body(Next, Next2, StLoopVs1, StLoopVs2),
    an_body(Goal, Goal2, StLoopVs2, StLoopVs),
    %
    G = 'xcontrol_rt:\6\shloop'(NegShVs, StLoopVs, Init2, Cond2, Next2, Goal2),
    rec_stvars(StLoopVs, StVs0, StVs). % TODO: only shared!
%
an_body(G0, G, StVs0, StVs) :- G0 = 'xcontrol_rt:\6\assign'(Var,_), !,
    G = G0,
    rec_stvar(Var, StVs0, StVs).
an_body(G, G, StVs0, StVs) :- !,
    rec_stvars_goal(G, StVs0, StVs).

rec_stvars_goal(Head, StVs0, StVs) :-
    Head =.. [_|Args],
    rec_stvars_args(Args, StVs0, StVs).

% TODO: indeed we'd only need to replace '\6\after'
rec_stvars_args([], StVs, StVs).
rec_stvars_args([Arg|Args], StVs0, StVs) :-
    ( nonvar(Arg), Arg = '\6\after'(V) ->
        rec_stvar(V, StVs0, StVs1)
    ; StVs1 = StVs0
    ),
    rec_stvars_args(Args, StVs1, StVs).

rec_stvar(V, StVs0, StVs) :-
    ( \+ (member(V0,StVs0), V0==V) ->
        StVs = [V|StVs0]
    ; StVs = StVs0
    ).

rec_stvars([], StVs, StVs).
rec_stvars([V|Vs], StVs0, StVs) :-
    rec_stvar(V, StVs0, StVs1),
    rec_stvars(Vs, StVs1, StVs).

% ===========================================================================
%! # Annotate linking vars (for PA)
% (based on straight_clause from pl2wam.pl)

anot_linking_vars(Head, Body0, Body3) :-
    pseq_to_list(Body0, Body1),
    mk_occurrences_list(Body1, 1, List),
    record_occurrences(Head, 0, List),
    straight_body(Body1, 1, List, Body2),
    list_to_pseq(Body2, Body3).

% (backward traverse)
mk_occurrences_list([], _, _).
mk_occurrences_list([G|Gs], Gn, List) :-
    Gn1 is Gn+1,
    mk_occurrences_list(Gs, Gn1, List),
    recordable(G, G2),
    record_occurrences(G2, Gn, List). % TODO: bad complexity if G needs_linked

% (forward traverse)
straight_body([], _, _, []).
straight_body([S|Gs], Gn, List, [Goal|Gs1]) :-
    ( needs_linked(S) ->
        ( do_not_record(S) ->
            record_occurrences(S, Gn, LocalList),
            linking_vars_local(LocalList, List, Shared)
        ; linking_vars(List, Gn, Shared) % TODO: review! used now?
        ),
        straight_body2(S, right, Shared, Goal)
    ; Goal = S
    ),
    Gn1 is Gn+1,
    straight_body(Gs, Gn1, List, Gs1).

% the subgoal requires linked vars? should it be annotated?
needs_linked(G) :- var(G), !, fail.
needs_linked('basiccontrol:;'(_,_)).
needs_linked('basiccontrol:->'(_,_)).
needs_linked('\6\shpa_right_sh'(_,_,_)).

% the subgoal defines a new scope, do not record its vars
do_not_record('\6\shpa_right_sh'(_,parent,_)).

% term with recordable variables
recordable('\6\shpa_right_sh'(_,parent,_), G) :- !, G = [].
recordable('\6\shpa_right_sh'(_,vs(ShVs),_), G) :- !, G = ShVs.
recordable(G, G).

straight_body2(G, right, _, R) :- atom(G), !, R = G.
straight_body2('basiccontrol:;'(G1,G2), right, Shared, R) :- !,
    R = 'basiccontrol:;'(G1b,G2b),
    straight_body2(G1, left, Shared, G1b),
    straight_body2(G2, right, Shared, G2b).
straight_body2('\6\shpa_right_sh'(PAMode,parent,PACode), _, Shared, R) :- !,
    % go inside shpa, including Head in head
    PACode = clause(Head,Body),
    R = '\6\shpa_right'(PAMode,Shared,clause(Head,Body2)),
    anot_linking_vars(Head-Shared, Body, Body2).
straight_body2('\6\shpa_right_sh'(PAMode,vs(ShVs),PACode), _, _Shared, R) :- !,
    % go inside shpa, including Head in head
    PACode = clause(Head,Body),
    R = '\6\shpa_right'(PAMode,ShVs,clause(Head,Body2)),
    anot_linking_vars(Head-ShVs, Body, Body2).
straight_body2(Goal0, _, Shared, Goal) :-
    anot_linking_vars(Shared, Goal0, Goal).

%-----------------------------------------------------------------------------
%! ## Ocurrences and linking vars

:- use_module(library(lists), [list_lookup/3,nonsingle/1,contains_ro/2,contains1/2,member/2]).

record_occurrences(Var, Gn, D) :- var(Var), !,
    list_lookup(D, Var, Occs),
    contains1(Occs, Gn).
record_occurrences(T, Gn, D) :-
    T =.. [_|Args],
    record_occurrences_args(Args, Gn, D).

record_occurrences_args([], _, _).
record_occurrences_args([X|Xs], Gn, D) :-
    record_occurrences(X, Gn, D),
    record_occurrences_args(Xs, Gn, D).

linking_vars(List, _, Xs) :- var(List), !, Xs = [].
linking_vars([V-Occs|List], Gn, Xs) :-
    nonsingle(Occs), contains_ro(Occs, Gn), !,
    Xs = [V|Xs0],
    linking_vars(List, Gn, Xs0).
linking_vars([_|List], Gn, Xs) :-
    linking_vars(List, Gn, Xs).

linking_vars_local(List, _, Xs) :- var(List), !, Xs = [].
linking_vars_local([V-_|List], ParentList, Xs) :-
    in_parent(ParentList, V), !,
    Xs = [V|Xs0],
    linking_vars_local(List, ParentList, Xs0).
linking_vars_local([_|List], ParentList, Xs) :-
    linking_vars_local(List, ParentList, Xs).

% TODO: linear search!
in_parent(List, _) :- var(List), !, fail.
in_parent([V0-_|List], V) :-
    ( V0 == V -> true
    ; in_parent(List, V)
    ).

%-----------------------------------------------------------------------------
%! ## Conj<->sequences
%
% Transform between conjunctions and lists. Expand into special forms
% for occurence recording and PA variable renaming.

:- use_module(library(lists), [append/3]).

% (encode -> as a list)
pseq_to_list('basiccontrol:->'(A,B), Seq) :- !,
    pconj_to_list(A,A2),
    pconj_to_list(B,B2),
    append(A2, ['\6\->'|B2], Seq).
pseq_to_list(A, Seq) :-
    pconj_to_list(A, Seq).

% (decode -> as a list)
list_to_pseq(Seq, G) :-
    append(A2, ['\6\->'|B2], Seq),
    !,
    list_to_pconj(A2,A),
    list_to_pconj(B2,B),
    G = 'basiccontrol:->'(A,B).
list_to_pseq(Seq, G) :-
    list_to_pconj(Seq,G).

% conjunction to list (splitting shpa)
pconj_to_list(A, Xs) :-
    pconj_to_list_(A, Xs, []).

pconj_to_list_(A, Xs, Xs0) :- var(A), !, Xs = [A|Xs0].
pconj_to_list_('hiord_rt:$pa_def'(PAMode, ShVs, PACode, V), Xs, Xs0) :- !,
    ( nonvar(ShVs), ShVs = -(NonShVs) ->
        % rename only NonShVs variables, mark shared as 'parent' to compute shared
        Shared = parent,
        rename_vars(NonShVs, PACode, PACode2)
    ; % variables should already be renamed by mexpand 
      PACode2 = PACode,
      Shared = vs(ShVs)
    ),
    % Note: straight_body replaces '\6\shpa_right_sh' by '\6\shpa_right'
    Xs = ['\6\shpa_left'(V), '\6\shpa_right_sh'(PAMode,Shared,PACode2)|Xs0]. % split PA for occurence record (support letrec)
pconj_to_list_('basiccontrol:,'(A,B), Xs, Xs0) :- !,
    pconj_to_list_(A, Xs, Xs1),
    pconj_to_list_(B, Xs1, Xs0).
pconj_to_list_('basiccontrol:true', Xs, Xs0) :- !, Xs = Xs0.
pconj_to_list_(A, [A|Xs], Xs).

% list to conjunction (merging shpa)
list_to_pconj([], 'basiccontrol:true') :- !.
list_to_pconj([X], X) :- !.
list_to_pconj(['\6\shpa_left'(V), '\6\shpa_right'(PAMode,Shared,PACode)|Xs], Y) :- !,
    list_to_pconj(['hiord_rt:$pa_def'(PAMode, Shared, PACode, V)|Xs], Y).
list_to_pconj([X|Xs], 'basiccontrol:,'(X,Y)) :- list_to_pconj(Xs, Y).

% ---------------------------------------------------------------------------

:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(sets), [ord_subtract/3]).

% TODO: move to lib?
% :- export(rename_vars/3).
% Copy X into Y, renaming variables Vs
rename_vars(Vs, X, Y) :-
    sort(Vs, Vs2),
    varset(X, XVs),
    ord_subtract(XVs, Vs2, Sh),
    copy_term(Sh-X, Sh-Y).

