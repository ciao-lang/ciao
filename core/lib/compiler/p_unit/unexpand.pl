:- module(unexpand, [
    module_spec/2,
    transform_clause_list/3, 
    transform_head/3,
    transform_body/3,
    transform_assrt_body/3,
    unexpand_meta_calls/2,
    transform_name/3,
    generate_unexpanded_data/1,
    clean_unexpanded_data/0, 
    add_head_unexpanded_data/1,
    unexpanded_import/3,
    transform_metapred/3
], [assertions, datafacts]).

% TODO: this is the inverse of mexpand.pl; synchronize! --JF

:- use_module(library(formulae)).

% :- use_module(engine(internals),[term_to_meta/2]).
:- use_module(engine(runtime_control), [module_split/3]).

:- use_module(library(idlists), [subtract/3]).
:- use_module(library(lists), [append/3, length/2]).

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(compiler/p_unit/p_unit_basic), [type_of_goal/2, meta_to_list/2]).

:- use_module(library(compiler/p_unit/p_unit_db), [curr_file/2]).
:- use_module(library(compiler/p_unit), [add_directive/1]).

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
transform_body(H, M, HT) :-
    transform_name(H, M, HT).
%transform_body(A, A).

transform_body_disj((A;B), M, (AT;BT)) :-
    !,
    transform_body_disj(A, M, AT),
    transform_body_disj(B, M, BT).
transform_body_disj(A, M, AT) :-
    list_to_conj(A, AC),
    transform_body(AC, M, ATM),
    list_to_conj(AT, ATM).


transform_name(H, user(_), HT) :-
    transform_name(H, user, HT),!.
transform_name(H, M, HT) :-
    transform_metapred(H, M, HT0),
    HT0 =.. [ F | A ],
    functor(HT0, F, N),
    transform_atom(F, N, M, FT),
    reconstruct(FT, A, HT).

reconstruct(M:FT, A, M:HT):- !,
    HT =.. [ FT | A ].
reconstruct(FT, A, HT):-
    HT =.. [ FT | A ].

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
    H =.. [F|A],
    functor(H, F, FArity),
    ( (type_of_goal(metapred(_Type,Meta),_),
       functor(Meta, F, MetaArity)),
      % this is something we have to keep in mind.
      % When a metapredicate has addmodule option, 
      % 2 things can happen:
      % (A) We read the goal from ciao, so ciao 
      %     expanded the goal, so the goal has more 
      %     arguments than metapredicate definition.
      % (B) A CiaoPP transformation added the goal,
      %     so the goal has the same number of 
      %     arguments than metapred declaration.
      (FArity = MetaArity  ->  % case B
          Meta =.. [ _ | Metaterms ]
      ;   % case A
          meta_to_list(Meta, Metaterms)
      ),
      transform_terms(A, Metaterms, M, A0)
      ->
          true
      ; A0 = A
   ),
    reconstruct(F, A0, HT).

% ---------------------------------------------------------------------------
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

% TODO: This code drops _M (because it can be 'multifile')
transform_head(H, _M, HT) :- % any M (could be multifile:)
    H =.. [F|A],
    module_split(F, _MIgnore, FT),
    !,
    HT =.. [FT|A].
transform_head(H, _M, H).

% Here we can translate from 'module:clause' to 'clause'

% TODO: make sure that we do not have problems with the '=:='/2 predicate
% TODO: _ModuleName unused!
transform_atom(F, A, _ModuleName, MCT) :-
    atom(F),
    module_split(F, Q, CT),
    !,
    transform_reexported_atom(F, A, Q, M),
    simplify_qualify(M:CT, A, MCT). % JF
transform_atom(F, _A, _ModuleName, F).

transform_reexported_atom(F, A, Q, M):-
    functor(Goal,F,A),
    type_of_goal(imported(Import),Goal),
    % Q is the one from which it is effectively imported
    module_spec(Import,Q),
    !,
    M=Q.
transform_reexported_atom(F, A, _Q, M):-
    functor(Goal,F,A),
    % it is probably reexported, take anyone:
    type_of_goal(imported(Import),Goal),
    !,
    module_spec(Import,M).
transform_reexported_atom(_F, _A, M, M).

% ---------------------------------------------------------------------------
% JF {

:- pred unexpanded_import(Name, Arity, Module).
:- data unexpanded_import/3.
:- pred unexpanded_defines(Name, Arity).
:- data unexpanded_defines/2.

:- use_module(library(compiler/p_unit/p_unit_db), [current_itf/3]).
:- use_module(library(pathnames), [path_basename/2]).

generate_unexpanded_data(user(File)) :-
    !, %% DTM: To support user files
    path_basename(File, Module),
    generate_unexpanded_data(Module).
generate_unexpanded_data(Module) :-
    % Generate inverse table for defines 
    current_itf(defines, MF, A),
    module_split(MF, Module, F),
    assertz_fact(unexpanded_defines(F, A)), % TODO: missing Module? (for multimod)
    fail.
generate_unexpanded_data(_) :-
    % Generate inverse table for imports
    type_of_goal(imported(_ISpec), Goal),
    %         module_spec(ISpec, IM),
    functor(Goal, MF, A),
    module_split(MF, IM, F),
    functor(GoalReexp, F, A),
    (current_itf(imports, GoalReexp, r(IM2,IM))-> true ; IM2=IM),
    \+ unexpanded_import(F, A, IM2),
    assertz_fact(unexpanded_import(F, A, IM2)),
    fail.
generate_unexpanded_data(_).

clean_unexpanded_data :-
    retractall_fact(unexpanded_defines(_, _)),
    retractall_fact(unexpanded_import(_, _, _)).

simplify_qualify('multifile':F, _A, F):-  !.
simplify_qualify(M:F, A, F) :-
    functor(Goal, F, A),
    (current_itf(imports, Goal, r(M2,M));M2=M),
    superfluous_qualify(M2, F, A), 
    !.
simplify_qualify(M:F, A, M2:F) :-
    functor(Goal, F, A),
    (current_itf(imports, Goal, r(M2,M));M2=M),
    !.
simplify_qualify(MF, _, MF).

% superfluous_qualify(M, F, A) :-
%       display(superfluous_qualify(M, F, A)), nl,
%       functor(Goal, F, A),
%       current_itf(imports, Goal, r(_,EM)),
%       module_split(F, _, Pred),
%       !,
%       module_concat(EM, Pred, NF),
%       superfluous_qualify(M, NF, A).
superfluous_qualify(M, F, A) :-
    ( unexpanded_defines(F, A) ->
        (\+ unexpanded_import(F, A, _) ->
            % defined in module and not imported from any module
            true 
        ;  
            % (defined in module and) imported from a module, BUT the
            % qualification refers to the current module
            curr_file(_, M),
            % This is to avoid warning
            add_directive(redefining(F/A)) % TODO: Hmmm avoid this!?
        )
    ; % (not defined in module and) imported only from one module
        unexpanded_import(F, A, M),
        \+ (unexpanded_import(F, A, M2), M2 \== M)
    ).
% JF }

% { DTM

:- pred add_unexpanded_data(Module, Functor, Arity, CurrModule) ::
    (atm(Module), atm(Fuctor), num(Arity))
   # "Add necessary internal data to do a correct unexpansion. User _do
   not have_ to use this predicate.".
% The same predicated as one imported
add_unexpanded_data(Module, Functor, Arity, _CurrModule) :-
    unexpanded_import(Functor, Arity, Module),
    !.
% The predicate is already defined in this module
add_unexpanded_data(Module, Functor, Arity, Module) :-
    unexpanded_defines(Functor, Arity),
    !.
% New Predicate defines in _this_ module
add_unexpanded_data(M, Functor, Arity, M) :- !,
    assertz_fact(unexpanded_defines(Functor, Arity)).
% New Predicate imported from other module
add_unexpanded_data(Module, Functor, Arity, _CurrModule) :-  !,
    assertz_fact(unexpanded_import(Functor, Arity, Module)).

% DTM }

% TODO: kludge; make sure that it is in some normal form before
:- pred add_head_unexpanded_data(Head)
# "@var{Head} can be Module:Functor(..), 'module:functor'(...) or
   funtor(...). In the latter case, the current module is used as the
   module containing the predicate. This predicate is only for
   internal use. It adds all the necessary information to make
   @pred{type_of_goal/2} and unexpansion process coherent.".

add_head_unexpanded_data(M:Head) :- !,
    functor(Head, F, A),
    % assert current_itf(defines, MF, A) ??
    curr_file(_, CM),
    add_unexpanded_data(M, F, A, CM).
add_head_unexpanded_data(MHead) :-
    MHead =.. [MF|As],
    module_split(MF, M, F),
    !,
    Head =.. [F|As],
    add_head_unexpanded_data(M:Head).
add_head_unexpanded_data(Head) :-
    curr_file(_, M),
    add_head_unexpanded_data(M:Head).

% ---------------------------------------------------------------------------

% TODO: use code from c_itf.pl
module_spec(Spec,M):-
    functor(Spec,_,1), !,
    arg(1,Spec,File),
    remove_slash(File,M).
module_spec(Spec,M):-
    atom(Spec),
    remove_slash(Spec,M).

remove_slash(Spec,M):-
    atom_codes(Spec,Codes),
    last_datum(Codes,T,T,Datum),
    atom_codes(M,Datum).

last_datum([],L,[],L).
last_datum([47|L],_,_,New):- !,
    last_datum(L,T,T,New).
last_datum([X|L],Old,Tail,New):- !,
    Tail=[X|NTail],
    last_datum(L,Old,NTail,New).

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
unexpand_meta_terms(_,0,_Meta,_).
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
