% (included file)

% This code is included in internals.pl and in c_itf.pl
%
% It uses the following facts (defined either in builtin or in compiler):
%  imports/5    - MODULE imports from MODULE2 F/A, which resides in ENDMODULE
%  meta_args/2  - MODULE has meta declaration META
%  multifile/3  - MODULE defines multifile F/A
%  defines/3    - MODULE defines F/A
%  goal_trans/3 - MODULE defines goal translation G

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(meta_inc)).

:- use_module(engine(hiord_rt), [this_module/1]). % TODO: Do not use it here; use facts from parent

% Backwards compatibility
% body_expansion(A, M, QM, NA) :-
%         body_expansion(A, M, QM, compile, NA)

% EXPORTED
body_expansion(V, M, QM, Mode, NA) :- var(V), !,
    ( Mode = retract ->
        this_module(internals), NA = V
    ; Mode = assert, this_module(c_itf) ->
        fail
    ; body_expansion(hiord_rt:call(V), M, QM, Mode, NA)
    ).
body_expansion(QM:G, M, QM0, Mode, NG) :- !,
    ( atom(QM) ->
        body_expansion(G, M, QM, Mode, NG)
    ; body_expansion(hiord_rt:call(QM:G), M, QM0, Mode, NG)
    ).
body_expansion((A,B), M, QM, Mode, (NA,NB)):- ciaopp_expansion, !,
    body_expansion(A, M, QM, Mode, NA),
    body_expansion(B, M, QM, Mode, NB).
body_expansion((A;B), M, QM, Mode, (NA;NB)):- ciaopp_expansion, !,
    body_expansion(A, M, QM, Mode, NA),
    body_expansion(B, M, QM, Mode, NB).
body_expansion((A->B), M, QM, Mode, (NA->NB)):- ciaopp_expansion, !,
    body_expansion(A, M, QM, Mode, NA),
    body_expansion(B, M, QM, Mode, NB).
body_expansion((X^A), M, QM, Mode, (X^NA)):- ciaopp_expansion, !,
    body_expansion(A, M, QM, Mode, NA).
body_expansion((\+ A), M, QM, Mode, (\+ NA)):- ciaopp_expansion, !,
    body_expansion(A, M, QM, Mode, NA).
body_expansion(if(A,B,C), M, QM, Mode, if(NA,NB,NC)) :- ciaopp_expansion, !,
    body_expansion(A, M, QM, Mode, NA),
    body_expansion(B, M, QM, Mode, NB),
    body_expansion(C, M, QM, Mode, NC).
body_expansion(!, _M, _QM, _Mode, !):- ciaopp_expansion, !.
body_expansion(true,     _M, _QM, _Mode, true):- ciaopp_expansion, !.
body_expansion(A, M, QM, Mode, NC) :-
    current_fact(goal_trans(M,_,_)) ->
    expand_goal(A, M, QM, NB), !,
    body_expansion(NB, M, -, Mode, NC).
body_expansion(A, M, QM, Mode, NA) :-
    atom_expansion_add_goals(A, M, QM, Mode, A1, NA, A1).

% JF: todo: add a skip goal declaration?
expand_inside(','(inside,inside), basiccontrol).
expand_inside(';'(inside,inside), basiccontrol).
expand_inside('->'(inside,inside), basiccontrol).
%expand_inside('^'(?,inside), aggregates).
expand_inside('\\+'(inside), basiccontrol).
expand_inside('if'(inside,inside,inside), basiccontrol).

expand_goal(G, M, QM, NG) :-
    \+ ( functor(G, N, A), functor(Meta, N, A), expand_inside(Meta, _) ),
    ( QM = (-) -> QG = G ; QG = QM:G ),
    goal_trans(M, T, _),
      arg(1, T, QG),
      arg(2, T, NG),
      '$meta_call'(T), !.

atom_expansion_add_goals(V, _, _, _, _, _, _) :- var(V), !, fail.
atom_expansion_add_goals('$meta_call'(X), M, -, _Mode, 'hiord_rt:call'(X), G, G) :-
    accessible_in(M, hiord_rt, '$meta_call', 1), !.
atom_expansion_add_goals('$meta_exp'(Metatype, P, E), M, -, Mode, NCall, G, G) :-
    accessible_in(M, hiord_rt, '$meta_exp', 3), !,
    meta_expansion_arg1(P, Metatype, M, -, Mode, fail, NP, NCall, 'term_basic:='(NP,E)).
atom_expansion_add_goals(Call, M, QM, Mode, NCall, G, G) :-
    functor(Call, call, N), N > 1, % call/n
    ( mexpand_imports(M, _, call, 2, HM) -> HM = hiord_rt ),
    !,
    Call =.. [_, P| LAs],
    As =.. [''| LAs],
    N1 is N-1,
    meta_expansion_arg1(P, pred(N1), M, QM, Mode, true, NP, NCall, 'hiord_rt:call'(NP,As)).
atom_expansion_add_goals(A, M, QM, Mode, NA, G, G_) :-
    functor(A, F, N),
    atom_expansion(A, F, N, M, QM, A1, RM),
    possibly_meta_expansion(F, N, A1, M, RM, Mode, NA, G, G_).

accessible_in(M, M, _, _) :- !.
accessible_in(M, EM, F, A) :-
    mexpand_imports(M, _, F, A, EM).

atom_expansion(_, F, _, _, _, NA, error) :- number(F), !, NA = F.
atom_expansion(A, F, N, M, -, NA, RM) :- !,
    unqualified_atom_expansion(A, F, N, M, NA, RM).
atom_expansion(A, F, N, M, M, NA, RM) :- !,
    atom_expansion_here(A, F, N, M, NA),
    RM = M.
atom_expansion(A, F, N, M, QM, NA, RM) :-
    M = user(_), QM = user, !,
    atom_expansion_here(A, F, N, M, NA),
    RM = M.
atom_expansion(A, F, N, M, QM, NA, RM) :-
    ( mexpand_imports(M, QM, F, N, EM) -> true
    ; module_warning(not_imported(F, N, M, QM)),
      EM = '$bad_qualification$'
    ),
    module_concat(EM, A, NA),
    RM = EM.

% This is not recursive since we want the expansion not depending on the
% imports of the modules used
unqualified_atom_expansion(A, F, N, M, NA, RM) :-
    mexpand_multifile(M, F, N), !,
    module_concat(multifile, A, NA),
    RM = M,
    check_if_imported(M, F, N).
unqualified_atom_expansion(A, F, N, M, NA, RM) :-
    ( mexpand_defines(M, F, N) ->  % local defined have priority
          module_concat(M, A, NA),
          RM = M,
          check_if_imported(M, F, N)
    ; mexpand_imports_last(M, IM, F, N, EM) ->
          module_concat(EM, A, NA),
          RM = EM,
          check_if_reimported(M, IM, F, N, EM)
    ; ( M = user(_) -> true
      ; module_warning(not_defined(F, N, M))
      ),
      module_concat(M, A, NA),
      RM = M
    ).

:- if(defined(priority_first_import)). % First import has priority (old semantics)
mexpand_imports_last(M, IM, F, N, EM) :- mexpand_imports(M, IM, F, N, EM).
:- else.
% Last import has priority (new semantics)
% TODO:[oc-merge] fix: traverse modules in reverse order in modload.c instead

:- use_module(engine(internals), ['$setarg'/4]).

% Get last solution of imports (note that asserting imports in reverse
% order may break source manipulating tools)
mexpand_imports_last(M, IM, F, N, EM) :-
    LastImported = last_import(_,_),
    ( mexpand_imports(M, IM0, F, N, EM0),
        atom(IM0), atom(EM0), % TODO: bug if not atoms! needed for nb setarg here
        '$setarg'(1, LastImported, IM0, no),
        '$setarg'(2, LastImported, EM0, no),
        fail
    ; true
    ),
    LastImported = last_import(IM,EM).
:- endif.

check_if_imported(M, F, N) :- 
    ( \+ redefining(M, F, N),
      mexpand_imports(M, IM, F, N, _) ->
        module_warning(imported_needs_qual(F, N, IM))
    ; true
    ).

check_if_reimported(M, IM, F, N, EM) :-
    ( \+ redefining(M, F, N),
      mexpand_imports(M, IM0, F, N, EM0),
      EM0 \== EM ->
        module_warning(imported_needs_qual(F, N, IM0, IM))
    ; true
    ).

atom_expansion_here(A, F, N, M, NA) :-
    mexpand_multifile(M, F, N), !,
    module_concat(multifile, A, NA).
atom_expansion_here(A, F, N, M, NA) :-
    ( ( mexpand_defines(M, F, N) ; M = user(_) ) -> true
    ; module_warning(not_defined(F, N, M))
    ),
    module_concat(M, A, NA).

% Called from c_itf

% EXPORTED
possibly_meta_expansion(F, N, A1, M, RM, Mode, NA, G, G_) :-
    functor(Meta, F, N),
    % JF: This is a work around, not a good solution...
    ( expand_inside(Meta, RM) ->
        Primitive = true
    ; mexpand_meta_args(RM, Meta),
      Primitive = fail
    ), !,
    functor(A1, F_, N_),
    meta_expansion_args(1, N_, A1, M, Mode, Meta, Primitive, NAL, G, G_),
    NA =.. [F_|NAL].
possibly_meta_expansion(_F,_N, A1,_M,_RM,_Mode, A1, G, G). % No meta expansion

% EXPORT

% Minor bug fix: defined head_expansion/0 because run-time module
% expansion should not be done in the head of a clause. -- EMM
:- data head_expansion/0.

meta_expansion_args(N, Max,_G,_M,_Mode,_Meta,_Primitive, [], NG, NG) :-
    N > Max, !.
meta_expansion_args(N, Max, G, M, Mode, Meta, Primitive, AL, NG, R) :-
    arg(N, Meta, Type),
    arg(N, G, X),
    meta_expansion_arg(Type, X, M, Mode, Primitive, AL, AL_, NG, NG_),
    N1 is N+1,
    meta_expansion_args(N1, Max, G, M, Mode, Meta, Primitive, AL_, NG_, R).

meta_expansion_arg(?, X, _M, _Mode, _Primitive, [X|AL], AL, R, R) :- !.
meta_expansion_arg(addmodule(Type), X, M, Mode, Primitive, AL0, AL, NG, NG_) :- !,
    meta_expansion_arg(Type, X, M, Mode, Primitive, AL0, [M|AL], NG, NG_).
meta_expansion_arg(addterm(Type), X, M, Mode, Primitive, [NX,X|AL], AL, NG, NG_) :- !,
    meta_expansion_type_(Type, X, M, -, Mode, Primitive, NX, NG, NG_).
meta_expansion_arg(Type, X, M, Mode, Primitive, [NX|AL], AL, NG, NG_) :-
    meta_expansion_type_(Type, X, M, -, Mode, Primitive, NX, NG, NG_).

% EXPORTED
meta_expansion_type_(Type, X, M, QM, Mode, Primitive, NX, NG, NG_) :-
    ( Type = list(Type2), nonvar(X), ( X = [] ; X = [_|_] ) ->
        meta_expansion_list(X, Type2, M, QM, Mode, Primitive, NX, NG, NG_)
    ; meta_expansion_arg1(X, Type, M, QM, Mode, Primitive, NX, NG, NG_)
    ).

meta_expansion_list(X, _, _, _, _, _, X, R, R) :- var(X), !.
meta_expansion_list([], _, _,  _,  _, _, [], R, R) :- !.
meta_expansion_list([E|Es], Type, M, QM, Mode, Primitive, [NE|NEs], G, R) :- !,
    meta_expansion_arg1(E, Type, M, QM, Mode, Primitive, NE, G, G_),
    meta_expansion_list(Es, Type, M, QM, Mode, Primitive, NEs, G_, R).

meta_expansion_arg1(A, primitive(Type), M, QM, Mode, _Primitive, NA, G, R) :- !,
    % (enable primitive metatype)
    meta_expansion_arg1(A, Type, M, QM, Mode, true, NA, G, R).
meta_expansion_arg1(A, Type, M, QM, Mode, Primitive, NA, R, R) :-
    expand_meta(A, Type, M, QM, Mode, XA), !,
    term_to_meta_or_primitive(Primitive, XA, NA).
meta_expansion_arg1(X, Type, M, QM, Mode, Primitive, NX, G, R) :-
    runtime_module_expansion(G, Type, Mode, Primitive, M, QM, X, NX, R).

term_to_meta_or_primitive(true, T, T).
term_to_meta_or_primitive(fail, T, T) :- ciaopp_expansion, !.
term_to_meta_or_primitive(fail, T, NT) :- term_to_meta(T,NT).

% Called from internals
% Predicate fails if expansion has to be done at runtime
expand_meta(V,       inside,  M,  QM,    Mode, NA) :-
    ( var(V) ; nonvar(V), V = (_:_) ), !,
    expand_meta_of_type(inside, V, M, QM, Mode, NA).
expand_meta(V,        _Type, _M, _QM,   _Mode,_NA) :- var(V), !, fail.
expand_meta('$:'(X),  _Type, _M, _QM,   _Mode, X ). % to handle lists...
expand_meta(QM:A,      Type,  M, _OldQM, Mode, NA) :- !,
    atom(QM),
    expand_meta(A, Type, M, QM, Mode, NA).
expand_meta(A, Type, M, QM, Mode, NA) :-
    expand_meta_of_type(Type, A, M, QM, Mode, NA).

expand_meta_of_type(_, Goal, _M, _QM, _Mode, Goal):-
    var(Goal),
    head_expansion,
    !.
expand_meta_of_type(clause, C, M, QM,_Mode, NC) :- !,
    expand_clause(C, M, QM, assert, NC).
expand_meta_of_type(retracting_clause, C, M, QM,_Mode, NC) :- !,
    expand_clause(C, M, QM, retract, NC).
expand_meta_of_type(spec, S, M, QM,_Mode, NS):- !,
    spec_expansion(S, M, QM, NS).
expand_meta_of_type(fact, Atom, M, QM, Mode, NAtom):- !,
    atom_expansion_add_goals(Atom, M, QM, Mode, NAtom, no, no).
expand_meta_of_type(goal, Goal, M, QM, Mode, NGoal):- !,
    body_expansion(Goal, M, QM, Mode, NGoal).
expand_meta_of_type(inside, Goal, M, QM, Mode, NGoal):- !,
    body_expansion(Goal, M, QM, Mode, NGoal).
expand_meta_of_type(pred(0), Goal, M, QM, Mode, NGoal):- !,
    body_expansion(Goal, M, QM, Mode, NGoal).
expand_meta_of_type(pred(N), P, M, QM, Mode, NP):-
    integer(N),
    pred_expansion(P, N, M, QM, Mode, NP0),
    ( ciaopp_expansion, \+ P = {_ :- _}, \+ P = (_ :- _) -> % predicate abstractions won't work
        ( NP0 = 'PAEnv'(_,'PA'(_,_,NP1)) -> true
        ; NP0 = 'PA'(_,_,NP1) % TODO: deprecate PA without PAEnv
        ),
        functor(P, _, A),
        functor(NP1, NP1Name, _),
        functor(NP, NP1Name, A),
        mexpand__unify_args(1, A, NP, 1, NP1)
    ; NP = NP0
    ).

expand_clause((H:-B), M, QM, Mode, (NH:-NB)):- !,
    atom_expansion_add_goals(H, M, QM, Mode, NH, no, no),
    body_expansion(B, M, QM, Mode, NB).
expand_clause(H, M, QM, Mode, NH):- !,
    atom_expansion_add_goals(H, M, QM, Mode, NH, no, no).

spec_expansion(F/A, M, QM, NF/A) :-
    atom(F),
    integer(A), A >= 0, % TODO:[JF] static compile-time error otherwise?
    functor(G,F,A),
    atom_expansion(G, F, A, M, QM, NG, _),
    functor(NG,NF,A).

pred_expansion({H :- B}, N, M, QM, Mode, Term) :- !,
    pred_expansion_pa(H, B, N, M, QM, Mode, Term).
pred_expansion((H :- B), N, M, QM, Mode, Term) :- !,
    pred_expansion_pa(H, B, N, M, QM, Mode, Term).
% For higher-order terms, all variables are shared
pred_expansion(P, N, M, QM, Mode, 'PAEnv'(P,PA)) :-
    mexpand__missing_args(P, N, M, H, G),
    atom_expansion_add_goals(G, M, QM, Mode, NG, no, no),
    copy_term('PA'(P,H,NG),PA). % rename vars

pred_expansion_pa(Hh, B, N, M, QM, Mode, 'PAEnv'(ShVs,PA)) :-
    head_and_shvs(Hh, H, ShVs),
    check_pred(H, N, {Hh :- B}),
    body_expansion(B, M, QM, Mode, NB),
    ( nonvar(ShVs), ShVs = -(_) -> % negative sharing (requires fsyntaxplus package)
        PA = 'PA'(ShVs,H,NB) % (do not rename vars)
    ; copy_term('PA'(ShVs,H,NB),PA) % rename vars
    ).

% Given P (arity A), create G (arity A+N) and H goal (arity N), where
% H contains the N missing arguments, equivalent to the following
% pseudocode:
%
%   P = F(P1,P2,P3,...Pa)
%   H = ''(H1,H2,H3,...Hn)
%   G = F(P1,P2,...,Pa,H1,H2,H3,...,Hn)
%
% If N is 0, then G = P.
mexpand__missing_args(P, 0, _M, H, G) :- !, % (special case)
    H = '',
    G = P.
mexpand__missing_args(P, N, _M, H, G) :-
    % decompose input goal P (of arity A)
    nonvar(P), functor(P, F, A), atom(F),
    % create anonymous goal H (to store variables for missing N arguments)
    functor(H,'',N), % H: ''(H1,H2,H3,...Hn)
    % create new goal G with N+A arity
    T is N+A,
    functor(G, F, T), % G: F(G1,G2,G3,...Gan)
    %
    % Obtain G: F(H1,P1,P2,...,Pa,H2,H3,...,Hn)
    % (unify arguments 1..A of P with 1..A of G)
    % (P1 = G1, P2 = G2, ..., Pa = Ga)
    mexpand__unify_args(1, A, P, 1, G), % Unify pred args
    % (unify arguments 1..N of H with (A+1)..(A+N) of G)
    % (H2 = Ga1, H3 = Ga3, ... Hn = Gan)
    A1 is A+1,
    mexpand__unify_args(1, N, H, A1, G).

head_and_shvs((ShVs-> H), H, ShVs) :- !.
head_and_shvs(H, H, []).

check_pred(H, N, PredAbs) :-
    functor(H, F, A),
    ( F = '', ! ; module_warning(bad_pred_abs(PredAbs)) ),
    compare(R,A,N),
    ( R = (=) -> true
    ; R = (<) ->
        module_warning(short_pred_abs(PredAbs, N))
    ;%R = (>) ->
        module_warning(big_pred_abs(PredAbs, N))
    ).

mexpand__unify_args(I, N, _F, _A, _G) :- I > N, !.
mexpand__unify_args(I, N, F, A, G) :- 
    arg(I, F, X),
    arg(A, G, X),
    I1 is I+1,
    A1 is A+1,
    mexpand__unify_args(I1, N, F, A1, G).

runtime_module_expansion(G,_Type, retract,_Primitive,_M,_QM, X, X, G) :- !.
runtime_module_expansion(G,_Type,_Mode,_Primitive,_M,_QM, X, X, G) :-
    ciaopp_expansion, !. % no runtime exp.
runtime_module_expansion('basiccontrol:,'(P, R), Type,_Mode, Primitive, M, QM, X, NX, R) :- !,
    % This predicate is defined diff. in compiler.pl and builtin.pl
    uses_runtime_module_expansion,
    P = 'internals:rt_module_exp'(X, Type, M, QM, Primitive, NX).
runtime_module_expansion(G,_Type,_Mode,_Primitive,_M,_QM, X, X, G).
