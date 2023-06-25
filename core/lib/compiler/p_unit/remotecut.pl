:- module(remotecut,[cleanup_remcut/0,transform_remote_cut/8],[assertions, datafacts]).

:- use_module(library(compiler/p_unit/program_keys), [clause_key/2, make_atom/2]).
:- use_module(library(compiler/p_unit/native),   [native_builtin/2]).
:- use_module(library(compiler/p_unit),   [new_predicate/3]).
:- use_module(library(compiler/p_unit/clause_db), [clause_locator/2, add_clause_locator/2]).

:- use_module(library(compiler/p_unit/meta_call), [meta_call/1, process_meta_call/5]).

:- use_module(library(lists),    [member/2, append/3]).
:- use_module(library(vndict),   [complete_dict/3, prune_dict/3, sort_dict/2]).

:- data cut/2.

cleanup_remcut:-
    retractall_fact(cut(_,_)).

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
% cut_idiom/2 + choice_idiom/2
% give the internal translations of remote cuts and choices depending on
% the target Prolog
%-----------------------------------------------------------------------------

% &-Prolog, SICStus0, SICStus2
% cut_idiom(Var,'SYSCALL'('$metacut'(Var))).
% SICStus3
% cut_idiom(Var,(prolog:'$metacut'(Var))).
% Ciao
cut_idiom(Var,G):- native_builtin(G,metacut(Var)), !.

% &-Prolog, SICStus0, SICStus2
% choice_idiom(Var,'SYSCALL'('$metachoice'(Var))).
% SICStus3
% choice_idiom(Var,(prolog:'$metachoice'(Var))).
% Ciao
choice_idiom(Var,G):- native_builtin(G,metachoice(Var)), !.

complete_dict_(D0,Cl,ND):-
    sort_dict(D0,D),
    complete_dict(D,Cl,ND).
