:- module(remdisj, [cleanup_remdisj/0, remove_disjunctions/10], [assertions, datafacts]).

:- use_module(library(compiler/p_unit/program_keys), [clause_key/2, last_clause/1, make_atom/2]).
:- use_module(library(compiler/p_unit), [new_predicate/3]).
:- use_module(library(compiler/p_unit/meta_call), [meta_call/1, process_meta_call/5]).
:- use_module(library(compiler/p_unit/clause_db), [clause_locator/2, add_clause_locator/2]).

:- use_module(library(lists), [length/2, append/3]).
:- use_module(library(sets), [merge/3, ord_intersection/3, ord_subtract/3]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(vndict), [prune_dict/3, sort_dict/2]).

:- data meta/1.

cleanup_remdisj:-
    retractall_fact(meta(_)).

%-------------------------------------------------------------------------
% remove disjunctions, local cut (if) and var goals
% input:  external format - full syntax - 
%         but unravelled parallel expressions and no '=>' and no cut in ';'
% output: external format - plain syntax (only ',' '&' and '\&')
%-------------------------------------------------------------------------
%% Input is a single clause and Id for what should be the following clause!
%% Output is a list of clauses

%% try to keep the same predicate wherever it is possible, also
%% preserve the order in doing this (do not interleave other preds. defs.)

%% you have to be sure that on the output each dictionary has only the
%% variables of the clause it refers to, and that each clause has its
%% own unique variables

remove_disjunctions(Cl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs) :-
    (Cl = (clause(_H,_B),Key),clause_locator(Key,Loc) -> true ; true),
    remove_disjunctions_loc(Cl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs,Loc).

remove_disjunctions_loc(Cl,D,NCls,NDs,NCls,NDs,Cls,Cls,Ds,Ds,_):- 
    var(Cl),
    var(D),
    !.
remove_disjunctions_loc(Cl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs,_) :- 
    Cl = (directive(G),Clid),!,
    NCls=[(directive(G),Clid)|TNCls],
    NDs=[D|TNDs],
    TCls = Cls,
    TDs = Ds.
remove_disjunctions_loc(Cl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs,_):- 
    Cl = (clause(H,B),Clid),
    var(B), !,
    NCl=(clause(H,call(B)),Clid),
    NCls=[NCl|TNCls],
    prune_dict_(NCl,D,ND),
    NDs=[ND|TNDs],
    TCls = Cls,
    TDs = Ds.
remove_disjunctions_loc(Cl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs,Loc):- 
    Cl = (clause(H,(B1->B2)),Clid),
    \+(B1=ask(_)),
    last_clause(Clid), !,
    NCl = (clause(H,(B1,!,B2)),Clid),
    remove_disjunctions_loc(NCl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs,Loc).
remove_disjunctions_loc(Cl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs,Loc):- 
    Cl = (clause(H,(B1->B2;B3)),Clid),
    last_clause(Clid), !,
    NCl1 = (clause(H,(B1,!,B2)),Clid),
    prune_dict_(NCl1,D,D1),
    remove_disjunctions_loc(NCl1,D1,NCls,NDs,TNCls1,TNDs1,Cls,TCls0,Ds,TDs0,Loc),
    clause_key(H,NClid),
    Cl2 = (clause(H,B3),NClid),
    add_clause_locator(NClid,Loc),
    copy_term(t(Cl2,D),t(NCl2,D2)),
    remove_disjunctions_loc(NCl2,D2,TNCls1,TNDs1,TNCls,TNDs,TCls0,TCls,TDs0,TDs,Loc).
remove_disjunctions_loc(Cl,D,NCls,NDs,TNCls,TNDs,Cls,TCls,Ds,TDs,Loc):- 
%  to keep local scope we need this and the change below for '->'  PBC
    Cl = (clause(H,(B1;B2)),Clid),
    \+ B1 = (_ -> _ ), !,
    NCl1 = (clause(H,B1),Clid),
    prune_dict_(NCl1,D,D1),
    remove_disjunctions_loc(NCl1,D1,NCls,NDs,TNCls1,TNDs1,Cls,TCls0,Ds,TDs0,Loc),
    clause_key(H,NClid),
    Cl2 = (clause(H,B2),NClid),
    add_clause_locator(NClid,Loc),
    copy_term(t(Cl2,D),t(NCl2,D2)),
    remove_disjunctions_loc(NCl2,D2,TNCls1,TNDs1,TNCls,TNDs,TCls0,TCls,TDs0,TDs,Loc).
remove_disjunctions_loc(Cl,D,[NCl|TNCls],[ND|TNDs],TNCls,TNDs,Cls,TCls,Ds,TDs,Loc):- 
    Cl = (clause(H,B),Clid),
    NCl = (clause(H,NB),Clid),
    remove_disj_from_body(B,H,D,NB,Clid,0,_,Cls,TCls,Ds,TDs,Loc),
    prune_dict_(NCl,D,ND).

%% Added "Rest" to collect variables appearing before and after the current
%% subgoal, so that new preds created have the minimum of variables possible,
%% in newsubg/7 (PBC)

remove_disj_from_body(P,_,_,call(P),_,In,In,Cls,Cls,Ds,Ds,_):-
    var(P), !.
remove_disj_from_body('hiord_rt:call'(P),Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc):-
    nonvar(P), !, % TODO: keep sync with mexpand semantics
    remove_disj_from_body(P,Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc).
remove_disj_from_body('hiord_rt:call'(P,Args),Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc):-
    nonvar(P), !, % TODO: keep sync with mexpand semantics
    P =.. PredL,
    Args =.. [_| ArgsL],
    append(PredL,ArgsL,FullPL),
    FullP =.. FullPL,
    remove_disj_from_body(FullP,Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc).
remove_disj_from_body(\+(P),Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc):- !,
    remove_disj_from_body(((P -> fail);true),Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc).
remove_disj_from_body(((P -> Q);R),Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc):- !,
    Out is In+1,
    newsubg(((P -> Q);R),Rest,NSg,_,Id,Out,'$disj'),
    clause_key(NSg,Clid1),
    Cl1 = (clause(NSg,(P,!,Q)),Clid1),
    add_clause_locator(Clid1,Loc),
    copy_term(t(Cl1,D),t(NCl1,D1)),
    remove_disjunctions_loc(NCl1,D1,Cls,Ds,TCls1,TDs1,NCls,TNCls,NDs,TNDs,Loc),
    clause_key(NSg,Clid2),
    Cl2 = (clause(NSg,R),Clid2),
    add_clause_locator(Clid2,Loc),
    copy_term(t(Cl2,D),t(NCl2,D2)),
    remove_disjunctions_loc(NCl2,D2,TCls1,TDs1,NCls,NDs,TNCls,TCls,TNDs,TDs,Loc).
remove_disj_from_body((P;Q),Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc):- !,
    Out is In+1,
    newsubg((P;Q),Rest,NSg,_,Id,Out,'$disj'),
    clause_key(NSg,Clid1),
    Cl1 = (clause(NSg,P),Clid1),
    add_clause_locator(Clid1,Loc),
    copy_term(t(Cl1,D),t(NCl1,D1)),
    remove_disjunctions_loc(NCl1,D1,Cls,Ds,TCls1,TDs1,NCls,TNCls,NDs,TNDs,Loc),
    clause_key(NSg,Clid2),
    Cl2 = (clause(NSg,Q),Clid2),
    add_clause_locator(Clid2,Loc),
    copy_term(t(Cl2,D),t(NCl2,D2)),
    remove_disjunctions_loc(NCl2,D2,TCls1,TDs1,NCls,NDs,TNCls,TCls,TNDs,TDs,Loc).
% ask which is like when:
%% remove_disj_from_body(((ask(C)->G)&),Rest,D,P1,Id,In,Out,Cls,TCls,Ds,TDs):-
%%      peel_meta_call(when(C,G),B,Args),
%%      build_meta_call(when/2,NB,Args,P1),
%%      remove_disj_from_meta(B,(Rest,Args),D,NB,Id,In,Out,Cls,TCls,Ds,TDs).
% ask which is not like when:
remove_disj_from_body((ask(C)->G),Rest,D,P1,Id,In,Out,Cls,TCls,Ds,TDs,Loc):-
    existentiate_constraint(C,Rest,C0),
    remove_disj_from_body((ask(C0),G),Rest,D,P1,Id,In,Out,Cls,TCls,Ds,TDs,Loc).
% concurrent goals:
%% remove_disj_from_body((G&),Rest,D,P1,Id,In,Out,Cls,TCls,Ds,TDs):-
%%      remove_disj_from_body(G,Rest,D,P1,Id,In,Out,Cls,TCls,Ds,TDs).
remove_disj_from_body((P -> Q),Rest,D,NSg,Id,In,Out,Cls,TCls,Ds,TDs,Loc):- !,
%  the only safe possibility is to create a new subgoal GPS 
%       remove_disj_from_body((P,!,Q),Rest,NSg,Id,In,Out).
    Out is In+1,
    newsubg((P -> Q),Rest,NSg,_,Id,Out,'$disj'),
    clause_key(NSg,Clid),
    Cl = (clause(NSg,(P,!,Q)),Clid),
    add_clause_locator(Clid,Loc),
    copy_term(t(Cl,D),t(NCl,ND)),
    remove_disjunctions_loc(NCl,ND,Cls,Ds,TCls0,TDs0,TCls0,TCls,TDs0,TDs,Loc).
remove_disj_from_body((P,Q),Rest,D,Plainconj,Id,In,Out,Cls,TCls,Ds,TDs,Loc):- !,
    remove_disj_from_body(P,(Rest,Q),D,P1,Id,In,O1,Cls,TCls0,Ds,TDs0,Loc),
    remove_disj_from_body(Q,(Rest,P),D,Q1,Id,O1,Out,TCls0,TCls,TDs0,TDs,Loc),
    add_conj(P1,Q1,Plainconj).
%% remove_disj_from_body((P&Q),Rest,D,Plainconj,Id,In,Out,Cls,TCls,Ds,TDs):- !,
%%      remove_disj_from_body(P,(Rest,Q),D,P1,Id,In,O1,Cls,TCls0,Ds,TDs0),
%%      remove_disj_from_body(Q,(Rest,P),D,Q1,Id,O1,Out,TCls0,TCls,TDs0,TDs),
%%      add_pconj(P1,Q1,Plainconj).
%% remove_disj_from_body((P\&Q),Rest,D,Plainconj,Id,In,Out,Cls,TCls,Ds,TDs):- !,
%%      remove_disj_from_body(P,(Rest,Q),D,P1,Id,In,O1,Cls,TCls0,Ds,TDs0),
%%      remove_disj_from_body(Q,(Rest,P),D,Q1,Id,O1,Out,TCls0,TCls,TDs0,TDs),
%%      add_ppconj(P1,Q1,Plainconj).
remove_disj_from_body(P,Rest,D,P1,Id,In,Out,Cls,TCls,Ds,TDs,Loc):-
    meta_call(P), !,
    process_meta_call(P,Bs,NoGList,NBs,P1),
    remove_disj_from_meta_list(Bs,(Rest,Bs,NoGList),D,NBs,Id,In,Out,Cls,TCls,Ds,TDs,Loc).
remove_disj_from_body(P,_,_,P,_,In,In,Cls,Cls,Ds,Ds,_):- !.  %true,!,etc

remove_disj_from_meta_list([],_,_,[],_,In,In,Cls,Cls,Ds,Ds,_).
remove_disj_from_meta_list([B|Bs],(Rest,[B|Bs],NGs),D,[NB|NBs],Id,In,Out,Cls,TCls,Ds,TDs,Loc):-
    remove_disj_from_meta(B,(Rest,Bs,NGs),D,NB,Id,In,O1,Cls,TCls0,Ds,TDs0,Loc),
    remove_disj_from_meta_list(Bs,(Rest,Bs,[B|NGs]),D,NBs,Id,O1,Out,TCls0,TCls,TDs0,TDs,Loc).

remove_disj_from_meta(X,_,_,NX,_,In,Out,Cls,TCls,Ds,TDs,_):-
    var(X), !,
    NX = X,
    Out = In,
    Cls=TCls,
    Ds=TDs.
remove_disj_from_meta(X,Rest,D,NX,Id,In,Out,Cls,TCls,Ds,TDs,Loc):-
    functor(X,F,2),
    ( F=',' ; F='=>' ; F='&' ; F=(\&) ), !,
    ( retract_fact(meta(N)) ->
      N1 is N+1
    ; N1 = 1
    ),
    asserta_fact(meta(N1)),
    newsubg(X,Rest,NX,_,Id,N1,'$meta'),
    remove_disj_from_body(X,Rest,D,NB,Id,In,Out,TCls0,TCls,TDs0,TDs,Loc),
    Cl=clause(NX,NB),
    clause_key(NX,Clid),
    add_clause_locator(Clid,Loc),
    prune_dict_(Cl,D,ND0),
    copy_term(t(Cl,ND0),t(NCl,ND)),
    Cls=[(NCl,Clid)|TCls0],
    Ds=[ND|TDs0].
remove_disj_from_meta(X,Rest,D,NX,Id,In,Out,Cls,TCls,Ds,TDs,Loc):-
    remove_disj_from_body(X,Rest,D,NX,Id,In,Out,Cls,TCls,Ds,TDs,Loc).

add_conj((X,Xs),Y,(X,Z)):- !,
    add_conj(Xs,Y,Z).
add_conj(X,Y,(X,Y)).

%% add_pconj((X&Xs),Y,(X&Z)):- !,
%%      add_pconj(Xs,Y,Z).
%% add_pconj(X,Y,(X&Y)).
%% 
%% add_ppconj((X\&Xs),Y,(X\&Z)):- !,
%%      add_ppconj(Xs,Y,Z).
%% add_ppconj(X,Y,(X\&Y)).

% make free variables explicit by existentiation

existentiate_constraint(C,Rest,NewC):-
    varset(Rest,Rv),
    existentiate_constraint0(C,Rv,NewC).
    
existentiate_constraint0((C0,C),Rv,(NewC0,NewC)):- !,
    existentiate_constraint0(C0,Rv,NewC0),
    varset(C,Cv),
    merge(Cv,Rv,Nv),
    existentiate_constraint0(C,Nv,NewC).
existentiate_constraint0(C,Rv,NewC):-
    varset(C,Cv),
    ord_subtract(Cv,Rv,Ev),
    existentiate_vars(Ev,C,NewC).

existentiate_vars([V|Vs],C,V^NewC):-
    existentiate_vars(Vs,C,NewC).
existentiate_vars([],C,C).

%-------------------------------------------------------------------------

newsubg(Sg,Rest,NSg,Vars,Id,Num,Atom):-
    varset(Sg,Sv),
    varset(Rest,Rv),
    ord_intersection(Sv,Rv,Vars),
    make_atom([Id,Atom,Num],Fooname),
    length(Vars,A),
    new_predicate(Fooname,A,Newname),
    NSg=..[Newname|Vars].

prune_dict_(Cl,D,ND):-
    prune_dict(Cl,D,ND0),
    sort_dict(ND0,ND).
