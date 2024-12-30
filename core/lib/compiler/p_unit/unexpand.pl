:- module(unexpand, [
    transform_clause_list/3, 
    transform_head/3,
    transform_body/3,
    transform_assrt_body/3,
    unexpand_meta_calls/2,
    transform_lit/3,
    regenerate_unexpanded_data/1,
    add_head_unexpanded_data/3,
    transform_metapred/3
], [assertions, datafacts]).

%! \title Module unexpansion

% NOTE: make sure that this module is synchronized with the module
% expansion rules in mexpand.pl

:- use_module(library(formulae)).

% :- use_module(engine(internals),[term_to_meta/2]).
:- use_module(engine(runtime_control), [module_split/3]).

:- use_module(library(idlists), [subtract/3]).
:- use_module(library(lists), [append/3, length/2]).

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(compiler/p_unit/p_unit_basic), [type_of_goal/2, meta_to_list/2]).

:- use_module(library(compiler/p_unit/p_unit_db), [curr_file/2]).

% -------------------------------------------------------------------------

% Done with set_ciaopp_expansion(true)
%% :- doc(bug,"Maybe it all would be easier if we changed the meta-pred
%%      expansion...").
%% :- doc(bug,"Heads of exports and goals of imports which are meta-preds
%%      with addmodule must be un-expanded.").
:- doc(bug,"Having set_ciaopp_expansion(true), unexpand_meta_calls should not
    be needed, except for 'hiord_rt:call' instead of 'call'.").

% -------------------------------------------------------------------------

transform_clause_list([], _M, []).
transform_clause_list([clause(H, B)|C],M, [clause(HT, BT)|CR]) :-
    transform_clause(H, B, M, HT, BT),
    transform_clause_list(C, M, CR).

transform_clause(H, B, M, H1, B1):-
    transform_head(H, M, H1),
    unexpand_meta_calls(B, BT),
    transform_body(BT, M, B1),
    !.
transform_clause(H, B, _, H, B):-
    throw(unexpansion_fails(H,B)).

%transform_body(true, true).
% DTM: WITHOUT THIS clause we could have infinity loop!!!
transform_body(A, _, A) :-
    var(A), !.
transform_body(','(A,B), M, ','(AT,BT)) :- !,
    transform_body(A, M, AT),
    transform_body(B, M, BT).
transform_body('&'(A,B), M, '&'(AT,BT)) :- !,
    transform_body(A, M, AT),
    transform_body(B, M, BT).
% ACC: Can be removed if the output of the parallelizers is transformed
transform_body('->'(A,B), M, '->'(AT,BT)) :- !,
    transform_body(A, M, AT),
    transform_body(B, M, BT).
% DTM: This is necesary for assertion body disjunction 
transform_body(Disj, M, DO) :-
    Disj = ';'(A,_),
    list(A),
    !,
    transform_body_disj(Disj, M, DO).
transform_body(';'(A,B), M, ';'(AT,BT)) :-
    !,
    transform_body(A, M, AT),
    transform_body(B, M, BT).
transform_body('->'(A,B), M, '->'(AT,BT)) :-
    !,
    transform_body(A, M, AT),
    transform_body(B, M, BT).
transform_body(true(H), M, true(HT)) :- !, % Note: unqualified true/1
    transform_body(H, M, HT).
transform_body(H, M, HT) :-
    transform_lit(H, M, HT).
%transform_body(A, A).

transform_body_disj((A;B), M, (AT;BT)) :-
    !,
    transform_body_disj(A, M, AT),
    transform_body_disj(B, M, BT).
transform_body_disj(A, M, AT) :-
    list_to_conj(A, AC),
    transform_body(AC, M, ATM),
    list_to_conj(AT, ATM).

transform_lit(H, user(_), HT) :-
    transform_lit(H, user, HT0), !, HT = HT0.
transform_lit(H, CurrMod, HT) :-
    transform_metapred(H, CurrMod, HT0),
    transform_atom(HT0, CurrMod, HT).

transform_atom(HT0, CurrMod, HT) :-
    functor(HT0, F, N),
    atom(F),
    module_split(F, M, CT),
    !,
    simplify_qualify(M:CT, N, CurrMod, MCT),
    HT0 =.. [_|As],
    reconstruct(MCT, As, HT).
transform_atom(H, _CurrMod, H). % TODO: when does it happen?

reconstruct(M:FT, As, M:HT) :- !, HT =.. [FT|As].
reconstruct(FT, As, HT) :- HT =.. [FT|As].

transform_terms([], [], _M, []).
transform_terms([_|As0], [remove|Ts], M, As):-
    transform_terms(As0, Ts, M, As).
transform_terms([A0|As0], [T|Ts], M, [A|As]):-
    T \== ?,
    nonvar(A0), !,
    % JF: very dirty hack -- add extra arguments to unexpand the predicate properly...
    % EM: if it is a number, leave as is
    ( T = pred(ExtraArity), \+number(A0) ->
        add_extra_args(ExtraArity, A0, A1)
    ; A1 = A0
    ),
    transform_body(A1, M, A2),
    ( T = pred(ExtraArity) ->
        remove_extra_args(ExtraArity, A2, A)
    ; A = A2
    ),
    transform_terms(As0, Ts, M, As).
transform_terms([A|As0], [_|Ms], M, [A|As]):-
    transform_terms(As0, Ms, M, As).

% TODO: remove
%% :- pred count(L, B, C)
%% 
%% # "@var{C} is the number of times that @var{B} is in @var{L}.".
%% 
%% count([], _, 0).
%% count([A|As], A, C) :-
%%      !,
%%      C is C1 + 1,
%%      count(As, A, C1).
%% count([_|As], A, C) :-
%%      count(As, A, C).

% ---------------------------------------------------------------------------

:- pred transform_metapred(H, M, HT)
# "Remove arguments from @var{H} corresponding to @tt{addmodule}
arguments marked in @tt{meta_predicate} declaration. For example:

@begin{verbatim}
Program:
(...)
:- meta_predicate p(?, addmodule).

p(X) :- 
(...)

Top-Level:
?- transform_metapred('test:p'(X,caller), A).

A = 'test:p'(X) ? ;

no
?-
@end{verbatim}
".

transform_metapred(H, M, HT) :-
    ( % Get Meta for H
      functor(H, F, FArity),
      % NOTE:[JF] meta_pred(_,_) needs a special case for hiord_rt:call
      ( % when H (expanded) have one more argument (we must try this first!)
        FArity > 0, MetaArity_G is FArity - 1,
        functor(MetaG, F, MetaArity_G),
        \+ H = 'hiord_rt:call'(_,_) % (avoid special case that require all args)
      ; % when H (expanded) do not have more arguments (or dcase "B" below)
        MetaG = H
      ),
      type_of_goal(metapred(_Type,Meta),MetaG),
      functor(Meta, F, MetaArity),
      % TODO: this has problems, if program transformations work at
      % the expanded level, these extra arguments should be explicit
      % in the transformed (module expanded) representation
      %
      % When a metapredicate has addmodule option, 
      % two things can happen:
      % (A) The goal is read from source and expanded with one more
      %     argument.
      % (B) The goal is added by a program transformation (without the
      %     extra argument).
      ( FArity = MetaArity  ->  % case B
          Meta =.. [_|Metaterms]
      ; % case A
        meta_to_list(Meta, Metaterms)
      ),
      H =.. [F|A],
      transform_terms(A, Metaterms, M, A2)
      ->
        reconstruct(F, A2, HT)
    ; HT = H
    ).

% ---------------------------------------------------------------------------

add_extra_args(N, A0, A) :-
    A0 =.. [F|As0],
    length(Extra, N),
    append(As0, Extra, As), !,
    A =.. [F|As].

remove_extra_args(N, M:A0, M:A) :- !,
    remove_extra_args(N, A0, A).
remove_extra_args(N, A0, A) :-
    A0 =.. [F|As0],
    length(Extra, N),
    append(As, Extra, As0), !,
    A =.. [F|As].

% ---------------------------------------------------------------------------

% TODO: do not drop _M when we allow multifile predicates with non 'multifile:' qualifier
transform_head(H, _M, HT) :- % any M (could be multifile:)
    H =.. [F|A],
    module_split(F, _MIgnore, FT),
    !,
    HT =.. [FT|A].
transform_head(H, _M, H).

% ---------------------------------------------------------------------------
% JF {

:- pred unexpanded_import(Name, Arity, IM, Module).
:- data unexpanded_import/4.
:- pred unexpanded_defines(Name, Arity, Module).
:- data unexpanded_defines/3.

:- use_module(library(compiler/p_unit/p_unit_db), [current_itf/3]).
:- use_module(library(pathnames), [path_basename/2]).

regenerate_unexpanded_data(Mod) :-
    clean_unexpanded_data(Mod),
    generate_unexpanded_data(Mod).

generate_unexpanded_data(user(File)) :-
    !, %% DTM: To support user files
    path_basename(File, Module), % TODO: wrong! this is not a valid module name
    generate_unexpanded_data(Module).
generate_unexpanded_data(Module) :-
    % Generate inverse table for defines 
    current_itf(defines, MF, A),
    module_split(MF, Module, F),
    assertz_fact(unexpanded_defines(F, A, Module)), % TODO: missing Module? (for multimod)
    fail.
generate_unexpanded_data(Module) :-
    % Generate inverse table for imports
    ( current_itf(imports(Module,direct),Goal,_) % unexpand from direct imports
    ; current_itf(imports(Module,injected),Goal,_) % TODO: unexpand also from injected imports
    ),
    functor(Goal, MF, A),
    module_split(MF, IM, F),
    unreexport_if_needed(IM, F, A, Module, IM2),
    %
    \+ unexpanded_import(F, A, IM2, Module),
    assertz_fact(unexpanded_import(F, A, IM2, Module)),
    fail.
generate_unexpanded_data(_).

% Obtain the actual module IM from which the possibly reexported
% predicate EM:F/A is made visible in CurrMod
unreexport_if_needed(EM, F, A, CurrMod, IM) :-
    % TODO: see assert_itf(imports,M,F,A,r(IM,EM))! here F is unqualified intentionally!
    ( functor(G, F, A), current_itf(imports(CurrMod,_), G, r(IM,EM)) -> true ; IM=EM ).

clean_unexpanded_data(Mod) :-
    retractall_fact(unexpanded_defines(_, _, Mod)),
    retractall_fact(unexpanded_import(_, _, _, Mod)).

% Simplify qualification M:P (F/A). This removes the module qualification if
% it is not ambiguous. That is:
%
%  - F/A is defined in CurrMod and not imported from any other module
%  - F/A is not defined in CurrMod but imported from only one module

simplify_qualify('multifile':F, A, CurrMod, MF2) :- !,
    % TODO: currently 'multifile' does not fill unexpanded_defines(F, _A, _CurrMod) but we may change it
    ( unexpanded_import(F, A, _, CurrMod) -> 
        MF2 = 'multifile':F % distinguish from imported
    ; MF2 = F
    ).
simplify_qualify(M:F, A, CurrMod, MF2) :-
    unreexport_if_needed(M, F, A, CurrMod, M2),
    ( superfluous_qualify(M2, F, A, CurrMod) -> MF2 = F
    ; MF2 = (M2:F)
    ).

superfluous_qualify(M, F, A, CurrMod) :-
    ( unexpanded_defines(F, A, CurrMod) -> % defined in module (assume M=CurrMod)
        % ... and not imported from any module
        \+ unexpanded_import(F, A, _, CurrMod)
    ; unexpanded_import(F, A, M, CurrMod) -> % (not defined in CurrMod) imported from M
        % ... and not from any other module
        \+ (unexpanded_import(F, A, M2, CurrMod), M2 \== M)
    ; fail
    ).
% JF }

% %% Old version: includes redefining/1 decl for local qualification; disabled by now (JF)
% :- use_module(library(compiler/p_unit), [add_directive/1]).
% % superfluous_qualify(M, F, A) :-
% %       display(superfluous_qualify(M, F, A)), nl,
% %       functor(Goal, F, A),
% %       current_itf(imports, Goal, r(_,EM)),
% %       module_split(F, _, Pred),
% %       !,
% %       module_concat(EM, Pred, NF),
% %       superfluous_qualify(M, NF, A).
% superfluous_qualify(M, F, A) :-
%     ( unexpanded_defines(F, A) ->
%         % (defined in module)
%         (\+ unexpanded_import(F, A, _) ->
%             % ... and not imported from any module
%             true 
%         ;   % ... and imported from a module, BUT the
%             % qualification refers to the current module
%             curr_file(_, M),
%             % This is to avoid warning
%             add_directive(redefining(F/A)) % TODO: Hmmm avoid this!?
%         )
%     ; % (not defined in module and) imported only from one module
%         unexpanded_import(F, A, M),
%         \+ (unexpanded_import(F, A, M2), M2 \== M)
%     ).

% TODO: required by program replace when introducing new predicates (e.g., spec); do in a better way!
add_head_unexpanded_data(_, MF, A) :-
    module_split(MF, M, F),
    !,
    add_unexpanded_data(M, F, A).
add_head_unexpanded_data(M, F, A) :- % TODO: really allow unqualified names?
    add_unexpanded_data(M, F, A).

add_unexpanded_data(M, F, A) :-
    ( unexpanded_defines(F, A, M) -> true
    ; assertz_fact(unexpanded_defines(F, A, M))
    ).

% -------------------------------------------------------------------------

transform_assrt_body(Body, M, BodyT) :-
    assertion_body(Pred,Compat,Call,Succ,Comp,Comm,Body),
    transform_head(Pred,M,PredT),
    transform_assrt_field(Compat,M,CompatT),
    transform_assrt_field(Call,M,CallT),
    transform_assrt_field(Succ,M,SuccT),
    transform_assrt_field(Comp,M,CompT),
    assertion_body(PredT,CompatT,CallT,SuccT,CompT,Comm,BodyT).

transform_assrt_field([],_M,[]):- !.
transform_assrt_field([G|Gs],M,[GT|GTs]):- !,
    transform_assrt_field(G,M,GT),
    transform_assrt_field(Gs,M,GTs).
transform_assrt_field(G,M,GT):-
    unexpand_meta_calls(G,G1),
    transform_body(G1,M,GT).

% -------------------------------------------------------------------------

% %% --- is this necessary? (ciaopp_expansion)
% unexpand_meta_calls((rt_module_exp(T0,_,_,_,_,T),A0),A):- !,
%       unexpand_meta_term(T0,T),
%       unexpand_meta_calls(A0,A).
% %% --- is this necessary? (ciaopp_expansion)
% unexpand_meta_calls(rt_module_exp(T0,_,_,_,_,T),true):- !,
%       unexpand_meta_term(T0,T).
%jcf%
unexpand_meta_calls(A,call(A)):- var(A),!.
%jcf%
unexpand_meta_calls((A0,B0),(A,B)):- !,
    unexpand_meta_calls(A0,A),
    unexpand_meta_calls(B0,B).
unexpand_meta_calls('hiord_rt:call'(A,''(X)),call(A,X)):- !.
unexpand_meta_calls(\+(X),\+(X1)):- !,
    unexpand_primitive_meta_term(X,X1).
unexpand_meta_calls(if(X,Y,Z),if(X1,Y1,Z1)):- !,
    unexpand_primitive_meta_term(X,X1),
    unexpand_primitive_meta_term(Y,Y1),
    unexpand_primitive_meta_term(Z,Z1).
unexpand_meta_calls(A0,A):-
% not enough info in itf_db (for the imported metapreds)
    type_of_goal(metapred(_Type,Meta),A0), !,
    functor(A0,F,N),
    functor(A,F,N),
    meta_to_list(Meta, MetaL),
    unexpand_meta_terms(A0,N,MetaL,A).
unexpand_meta_calls(A,A).

unexpand_primitive_meta_term(X,X1):- var(X), !, X1=X.
unexpand_primitive_meta_term(call(X),X1):- !, X1=X.
unexpand_primitive_meta_term(X,X1):-
    unexpand_meta_term(X,X1).

% --- DTM: is Meta argument used???
unexpand_meta_terms(_,0,_Meta,_) :- !.
unexpand_meta_terms(A0,N,Meta,A):-
    N > 0,
    arg(N,A0,T0),
    arg(N,A,T),
    unexpand_meta_term(T0,T),
    N1 is N-1,
    unexpand_meta_terms(A0,N1,Meta,A).

unexpand_meta_term(T0,T):-
    nonvar(T0), !,
    ( meta_term_abstraction(T0,T2) -> true
    ; T2=T0
    ),
    unexpand_meta_calls_in_term(T2,T).
unexpand_meta_term(T,T).

unexpand_meta_calls_in_term(A0,A):-
    var(A0), !,
    A = A0.
unexpand_meta_calls_in_term((A0,B0),(A,B)):- !,
    unexpand_meta_calls_in_term(A0,A),
    unexpand_meta_calls_in_term(B0,B).
unexpand_meta_calls_in_term(A0,A):-
    unexpand_meta_calls(A0,A).

% TODO: fix PAEnv and PA support in CiaoPP
meta_term_abstraction('PAEnv'(_,PA),Prop):- !,
    meta_term_abstraction(PA,Prop).
meta_term_abstraction('PA'(_Term,Abs,Call),Prop):- !,
    Call=..[F|Args],
    Abs=..[''|NonArgs],
    subtract(Args,NonArgs,PropArgs),
    Prop=..[F|PropArgs].
meta_term_abstraction('$'(Term,_Call,_Meta),Term).

% Check if in assert_itf is reexported when doing transform_body
