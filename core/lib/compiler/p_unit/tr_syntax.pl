:- module(tr_syntax, [cleanup_tr_syntax/0, traverse_clauses/4], [assertions, basicmodes, datafacts]).

:- use_module(library(compiler/p_unit/program_keys), [clause_key/2, last_clause/1, make_atom/2]).
:- use_module(library(compiler/p_unit/native), [native_builtin/2]).
:- use_module(library(compiler/p_unit), [new_predicate/3]).
:- use_module(library(compiler/p_unit/clause_db), [clause_locator/2, add_clause_locator/2]).
%
:- use_module(library(compiler/p_unit/meta_call), [meta_call/1, process_meta_call/5]).
%
:- use_module(library(lists), [member/2, length/2, append/3]).
:- use_module(library(sets), [merge/3, ord_intersection/3, ord_subtract/3]).
:- use_module(library(terms_vars), [varset/2]).

% TODO: refactor

% (See Attic/tr_syntax.pl for old support for CGE (conditional graph expressions))

:- doc(bug,"1. Make predicate change/2 disappear.").

% :- doc(bug,"2. Clauses result from remove cut are no processed by
%                 remove_disjuntions.").

% ---------------------------------------------------------------------------

:- data meta/1.
:- data cut/2.

:- doc(cleanup_tr_syntax,"Cleanups internal database.").
cleanup_tr_syntax:-
    retractall_fact(meta(_)),
    retractall_fact(cut(_,_)).

% ---------------------------------------------------------------------------

:- pred traverse_clauses(+Cls,+Ds,-NewCls,-NewDs) # "It traverses
   program clauses @var{Cls} suitably changing the format for the
   following step (analysis, annotation, etc).  In general, it will
   have to remove disjunctions, if-then-elses, remote cuts, etc.  In
   doing this, the old clauses will be transformed and new clauses
   (and thus new dictionaries) can be created.".

% This new clauses and dictionaries will be accumulated
%       in an incomplete list (NCls with tail TNCls and NDs with tail TNDs)
%       in order to include them (if possible) after the definition of the
%       predicate whose clauses have genereted them I.e. whenever the Id
%       (Functor/Arity) for the next clause would be different from the
%       previous traversed clause. Note that if a declarative appears in
%       the middle of different clauses belonging to the same predicate, or
%       the clauses do not appear together, the new clauses will not appear
%       at the end of the predicate.  Difference lists are used.

traverse_clauses(Cls,Ds,NewCls,NewDs):-
    traverse(Cls,Ds,NewCls0,NewDs),
    change(NewCls0,NewCls).

change([],[]).
change([(Cl,K)|P0s],[Cl:K|Ps]):-
    change(P0s,Ps).

clause_id(Cl:K,Cl,K).
clause_id((Cl,K),Cl,K).

traverse([Clause|Cls],Ds,NewCls,NewDs):-
    clause_id(Clause,Cl,K),
    clause_pred(Cl,Id),
    traverse_([(Cl,K)|Cls],Ds,Id,NewCls,NewDs,NCls,NCls,NDs,NDs).
traverse([],[],[],[]).

traverse_([],[],_,NewCls,NewDs,NCls,TCls,NDs,TDs):- !,
    NewCls = NCls,
    NewDs  = NDs,
    TCls = [],
    TDs  = [].
traverse_([(Cl,Clid)|Cls],[D|Ds],Id,NewCls,NewDs,NCls,TCls,NDs,TDs):-
    clause_pred(Cl,Id), !,
    traverse_clause((Cl,Clid),D,NewCls,NewDs,TNewCls,TNewDs,TCls,TNCls,TDs,TNDs),
    traverse_(Cls,Ds,Id,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs).
traverse_(Cls,Ds,_,NCls,NDs,NCls0,Cls,NDs0,Ds):-
    traverse(NCls0,NDs0,NCls,NDs).

clause_pred(directive(_),directive).
clause_pred(clause(Head,_),F/A):-
    functor(Head,F,A).

:- pred traverse_clause(+Cl,+D,-NewCls,-NewDs,-TNewCls,-TNewDs,-NCls,-TNCls,-NDs,-TNDs).

traverse_clause(Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs):-
    transform_remote_cut(Cl,D,NCl1,ND1,NCls,TNCls1,NDs,TNDs1),
    remove_disjunctions(NCl1, ND1, NewCls, NewDs, TNewCls, TNewDs,TNCls1, TNCls, TNDs1, TNDs).

% ===========================================================================
:- doc(section, "Add remote cut/choice").

%-------------------------------------------------------------------------
% add remote cuts
% input:  external format - full syntax - 
%         but unravelled parallel expressions and no '=>'
% output: external format - full syntax, but no cuts within disjunctions
%-------------------------------------------------------------------------

%% transform_remote_cut((Cl,Id),D,(NCl,Id),ND):-
%%      transform_remote_cut0(Cl,D,NH,NCl,ND), !,
%% transform_remote_cut(Cl,D,Cl,D,Cls,Cls,Ds,Ds).
%% 
%% transform_remote_cut0(directive(G),D,directive(G),D):- !.
%% transform_remote_cut0(clause(H,true),D,clause(H,true),D):- !.
%% transform_remote_cut0(clause(H,!),D,clause(H,!),D):- !.
%% transform_remote_cut0(clause(H,B),D,NH,clause(NH,NB),ND):-
%%      remote_cut(B,Name,Var,NB),
%%      cut_idiom_clause(Name,Var,H,D,NH,ND).

transform_remote_cut(Cl,D,(clause(H,NB),Id),ND,Cls,TCls,Ds,TDs):-
    nonvar(Cl),
    Cl = (clause(H,B),Id),
    ( clause_locator(Id,Loc) -> true ; fail),
    remote_cut(B,Name,Var,BodyCut),
    cut_idiom_clause(Name,Var,H,D,NH,NewD), !,
    choice_idiom_clause(Var,H,BodyCut,D,NH,NB,ND,NewCl,Loc),
%       make_state_consistent,                                %jcf-20.12.2004%
    Cls=[NewCl|TCls],
    Ds=[NewD|TDs].
transform_remote_cut(Cl,D,Cl,D,Cls,Cls,Ds,Ds).

cut_idiom_clause(Sw,Var,H,D,NH,ND):-
    nonvar(Sw), !,
    complete_dict_(D,Var,ND),
    choice_point_as_arg(H,Var,NH).
%% cut_idiom_clause(_Name,_Var,H,D,H,D).

choice_point_as_arg(Goal,Var,NGoal):-
    current_fact(cut(N0,List),Ref), !,
    functor(Goal,F,A),
    Cut=key(F/A,Term),
    ( member(Cut,List) ->
      copy_term(Term,cut(Goal,Var,NGoal))
    ; N is N0+1,
      functor(GGoal,F,A),
      cut_predicate_name(GGoal,N,GVar,NGGoal),
      Term=cut(GGoal,GVar,NGGoal),
      copy_term(Term,cut(Goal,Var,NGoal)),
      erase(Ref),
      asserta_fact(cut(N,[Cut|List]))
    ).
choice_point_as_arg(Goal,Var,NGoal):-
    functor(Goal,F,A),
    functor(GeneralGoal,F,A),
    Cut=cut(GeneralGoal,GVar,NGGoal),
    N=1,
    cut_predicate_name(GeneralGoal,N,GVar,NGGoal),
    copy_term(Cut,cut(Goal,Var,NGoal)),
    asserta_fact(cut(N,[key(F/A,Cut)])).

cut_predicate_name(Goal,N,Var,NGoal):-
    functor(Goal,F,A),
    make_atom([F,A,'$remote',N],Name0),
    A1 is A+1,
    new_predicate(Name0,A1,Name),
    Goal=..[F|Args],
    append(Args,[Var],NArgs),
    NGoal=..[Name|NArgs].

choice_idiom_clause(Var,H,BCut,D,NH,(Choice,NH),ND,(clause(NH,BCut),NId),Loc):-
    choice_idiom(Var,Choice),
    clause_key(NH,NId),
    complete_dict_(D,Var,NewD),
    prune_dict((H,NH,Var),NewD,ND),
    add_clause_locator(NId, Loc).

% :- use_module(library(compiler/p_unit/itf_db), [assert_itf_kludge/2]).
% make_state_consistent:-
%       IM = engine(internals),
%       choice_idiom(Var,Choice),
%       assert_itf_kludge(remote,imports(Choice,IM)),
%       cut_idiom(Var,Cut),
%       assert_itf_kludge(remote,imports(Cut,IM)).

%-------------------------------------------------------------------------

% no (remote) cut within &, \& or =>, please!

remote_cut(P,_,_,P):-
    var(P), !.
remote_cut((P,Q),Switch,Var,(R,S)):- !,
    remote_cut(P,Switch,Var,R),
    remote_cut(Q,Switch,Var,S).
remote_cut((P;Q),Switch,Var,S):- !,
    put_cut_to((P;Q),Switch,Var,S).
remote_cut((P->Q;R),Switch,Var,(P->S)):- !,
    put_cut_to((Q;R),Switch,Var,S).
remote_cut((P->Q),Switch,Var,(P->S)):- !,
    put_cut_to(Q,Switch,Var,S).
remote_cut(P,Switch,Var,P1):-
    meta_call(P), !,
    process_meta_call(P,Bs,_,NBs,P1),
    remote_cut_meta_list(Bs,Switch,Var,NBs).
remote_cut(P,_,_,P).

remote_cut_meta_list([],_,_,[]).
remote_cut_meta_list([B|Bs],Switch,Var,[NB|NBs]):-
    remote_cut_meta(B,Switch,Var,NB),
    remote_cut_meta_list(Bs,Switch,Var,NBs).

remote_cut_meta(B,_,_,NB):-
    var(B), !, NB=B.
remote_cut_meta(B,Switch,Var,NB):-
    remote_cut(B,Switch,Var,NB).

put_cut_to(P,_,_,P):-
    var(P), !.
put_cut_to(!,Switch,Var,Cut):- !,
    cut_idiom(Var,Cut),                   %% Var is new variable
    Switch='Choice_point'.                %%  and this is its name
put_cut_to((P,Q),Switch,Var,(R,S)):- !,
    put_cut_to(P,Switch,Var,R),
    put_cut_to(Q,Switch,Var,S).
put_cut_to((P;Q),Switch,Var,(R;S)):- !,
    put_cut_to(P,Switch,Var,R),
    put_cut_to(Q,Switch,Var,S).
put_cut_to((P->Q;R),Switch,Var,(S->U;V)):- !,
    put_cut_to(P,Switch,Var,S),
    put_cut_to(Q,Switch,Var,U),
    put_cut_to(R,Switch,Var,V).
put_cut_to((P->Q),Switch,Var,(R->S)):- !,
    put_cut_to(P,Switch,Var,R),
    put_cut_to(Q,Switch,Var,S).
put_cut_to(P,_,_,P).

% %-------------------------------------------------------------------------
% % add remote choices
% % input/output:  internal format - plain syntax
% %-------------------------------------------------------------------------
% 
% % adds needed clauses for "$choice" and renames predicate defis. affected
% 
% transform_remote_choice(Cls,Ds,NCls,NDs):-
% %     ( debugging(top) -> write(remote_entry) ; true ),
%       current_fact(cut(_,Cuts),Ref), !,
% %     ( debugging(top) -> write(remote_erase) ; true ),
%       erase(Ref),
% %     ( debugging(top) -> write(remote_2rename) ; true ),
%       choice_idiom_clauses_to_rename(Cuts,[],CutCls),
% %     ( debugging(top) -> write(remote_rename) ; true ),
%       choice_idiom_rename_clauses(Cls,Cuts,CutCls,Ds,NCls,TNCls,NDs,TNDs),
% %     ( debugging(top) -> write(remote_new) ; true ),
%       choice_idiom_new_clauses(Cuts,TNCls,[],TNDs,[]).
% transform_remote_choice(Cls,Ds,Cls,Ds).
% 
% choice_idiom_clauses_to_rename([],CutCls,CutCls).
% choice_idiom_clauses_to_rename([key(F/A,_)|Cuts],CutCls0,CutCls):-
%       functor(H,F,A),
%       clauses_keys(H,CutCls1),
%       merge(CutCls0,CutCls1,CutCls2),
%       choice_idiom_clauses_to_rename(Cuts,CutCls2,CutCls).
% 
% choice_idiom_rename_clauses([],_Cuts,_CCls,[],NCls,NCls,NDs,NDs).
% choice_idiom_rename_clauses([Cl|Cls],Cuts,CCls,[D|Ds],NCls,TCls,NDs,TDs):-
%       Cl=(clause(H,B),_),
%       functor(H,F,A),
%       member(key(F/A,Cut),Cuts), !,
%       copy_term(p(Cut,D,B),p(cut(H,Choice,NH),D,B)),
%       complete_dict_(D,Choice,ND),
%       clause_key(NH,Id),
%       NCls=[(clause(NH,B),Id)|Cls0],
%       NDs=[ND|Ds0],
%       choice_idiom_rename_clauses(Cls,Cuts,CCls,Ds,Cls0,TCls,Ds0,TDs).
% choice_idiom_rename_clauses([Cl|Cls],Cuts,CCls,[D|Ds],NCls,TCls,NDs,TDs):-
%       Cl=(clause(H,B),OldId),
%       ord_member(OldId,CCls), !,
%       clause_key(H,Id),
%       NCls=[(clause(H,B),Id)|Cls0],
%       NDs=[D|Ds0],
%       choice_idiom_rename_clauses(Cls,Cuts,CCls,Ds,Cls0,TCls,Ds0,TDs).
% choice_idiom_rename_clauses([C|Cls],Cs,CCs,[D|Ds],[C|NCls],TCls,[D|NDs],TDs):-
%       choice_idiom_rename_clauses(Cls,Cs,CCs,Ds,NCls,TCls,NDs,TDs).
% 
% is_remote_pred(H):-
%       functor(H,F,_),
%       name(F,String),
%       name('/$remote/',SubString),
%       substring(SubString,String).
% 
% substring([],_):- !.
% substring([S|SS],[S|More]):-
%       append(SS,_,More),!.
% substring(SS,[_|More]):-
%       substring(SS,More).
% 
% choice_idiom_new_clauses([],NCls,NCls,NDs,NDs).
% choice_idiom_new_clauses([Cut|Cuts],[(NCl,Id)|NCls],TCls,[D|NDs],TDs):-
%       Cut=key(_,cut(H,Var,Goal)),
%       choice_idiom(Var,Choice),
%       NCl=clause(H,(Choice,Goal)),
%       clause_key(H,Id),
%       complete_dict_(dic([],[]),NCl,D),
%       choice_idiom_new_clauses(Cuts,NCls,TCls,NDs,TDs).

%-----------------------------------------------------------------------------

cut_idiom(Var,G):- native_builtin(G,metacut(Var)), !.

choice_idiom(Var,G):- native_builtin(G,metachoice(Var)), !.

% ===========================================================================
:- doc(section, "Remove disjunctions").

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
    %
    ( retract_fact(meta(N)) ->
      N1 is N+1
    ; N1 = 1
    ),
    asserta_fact(meta(N1)),
    newsubg(X,Rest,NX,_,Id,N1,'$meta'),
    %
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

% ---------------------------------------------------------------------------

:- use_module(library(vndict), [complete_dict/3, prune_dict/3, sort_dict/2]).

complete_dict_(D0,Cl,ND):-
    sort_dict(D0,D),
    complete_dict(D,Cl,ND).

prune_dict_(Cl,D,ND):-
    prune_dict(Cl,D,ND0),
    sort_dict(ND0,ND).
