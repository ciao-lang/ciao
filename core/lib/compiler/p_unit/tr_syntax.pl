:- module(tr_syntax, [cleanup_tr_syntax/0, traverse_clauses/5],
    [assertions, basicmodes]).

:- use_module(library(compiler/p_unit/remotecut), [cleanup_remcut/0, transform_remote_cut/8]).
:- use_module(library(compiler/p_unit/remdisj), [cleanup_remdisj/0, remove_disjunctions/10]).
%:- use_module(library(compiler/p_unit/cge2ite),[cge_to_ite/2,unravel_cges/8]).
cge_to_ite(Cl,Cl).
unravel_cges(Cl,_D,_,Cl,NCls,NCls,NDs,NDs).

:- doc(bug,"1. Make predicate change/2 disappear.").

% :- doc(bug,"2. Clauses result from remove cut are no processed by
%                 remove_disjuntions.").

:- doc(cleanup_tr_syntax,"Cleanups internal database.").
cleanup_tr_syntax:-
    cleanup_remdisj,
    cleanup_remcut.

%-------------------------------------------------------------------------
:- pred traverse_clauses(+Cls,+Ds,+Flag,-NewCls,-NewDs)
    : member(Flag,[all,remove,unravel,unravel_remove])
   # "It traverses program clauses @var{Cls} suitably changing the format
      for the following step (analysis, annotation, etc).  In general, it
      will have to remove disjunctions, if-then-elses, nested CGEs, etc. 
      The particular format is indicated by @var{Flag}. In doing this, the 
      old clauses will be transformed and new clauses (and thus new 
      dictionaries) can be created.".
% This new clauses and dictionaries will be accumulated
%       in an incomplete list (NCls with tail TNCls and NDs with tail TNDs)
%       in order to include them (if possible) after the definition of the
%       predicate whose clauses have genereted them I.e. whenever the Id
%       (Functor/Arity) for the next clause would be different from the
%       previous traversed clause. Note that if a declarative appears in
%       the middle of different clauses belonging to the same predicate, or
%       the clauses do not appear together, the new clauses will not appear
%       at the end of the predicate.  Difference lists are used.

traverse_clauses(Cls,Ds,Flag,NewCls,NewDs):-
    traverse(Cls,Ds,Flag,NewCls0,NewDs),
    change(NewCls0,NewCls).

change([],[]).
change([(Cl,K)|P0s],[Cl:K|Ps]):-
    change(P0s,Ps).

clause_id(Cl:K,Cl,K).
clause_id((Cl,K),Cl,K).

traverse([Clause|Cls],Ds,Flag,NewCls,NewDs):-
    clause_id(Clause,Cl,K),
    clause_pred(Cl,Id),
    traverse_([(Cl,K)|Cls],Ds,Id,Flag,NewCls,NewDs,NCls,NCls,NDs,NDs).
traverse([],[],_Flag,[],[]).

traverse_([],[],_,_F,NewCls,NewDs,NCls,TCls,NDs,TDs):- !,
    NewCls = NCls,
    NewDs  = NDs,
    TCls = [],
    TDs  = [].
traverse_([(Cl,Clid)|Cls],[D|Ds],Id,F,NewCls,NewDs,NCls,TCls,NDs,TDs):-
    clause_pred(Cl,Id), !,
    traverse_clause(F,(Cl,Clid),D,NewCls,NewDs,TNewCls,TNewDs,TCls,TNCls,
                                                                TDs,TNDs),
    traverse_(Cls,Ds,Id,F,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs).
traverse_(Cls,Ds,_,F,NCls,NDs,NCls0,Cls,NDs0,Ds):-
    traverse(NCls0,NDs0,F,NCls,NDs).

clause_pred(directive(_),directive).
clause_pred(clause(Head,_),F/A):-
    functor(Head,F,A).

:- pred traverse_clause(+Flag,+Cl,+D,
                   -NewCls,-NewDs,-TNewCls,-TNewDs,-NCls,-TNCls,-NDs,-TNDs)
   # "Transforms the format of a given clause depending on Flag.".

traverse_clause(all,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs):-
    unravel_cges(Cl,D,non,NCl0,NCls,TNCls0,NDs,TNDs0),
    cge_to_ite(NCl0,NCl00),
    transform_remote_cut(NCl00,D,NCl1,ND1,TNCls0,TNCls1,TNDs0,TNDs1),
    remove_disjunctions(NCl1, ND1, NewCls, NewDs, TNewCls, TNewDs,
                         TNCls1, TNCls, TNDs1, TNDs).

traverse_clause(remove,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs):-
    remove_disjunctions(Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,
                                                              NDs,TNDs).
traverse_clause(unravel,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,NDs,TNDs):-
    unravel_cges(Cl,D,':',NCl,NCls,TNCls,NDs,TNDs),
    NewCls = [NCl|TNewCls],
    NewDs = [D|TNewDs].
traverse_clause(unravel_remove,Cl,D,NewCls,NewDs,TNewCls,TNewDs,NCls,TNCls,
                                                               NDs,TNDs):-
    unravel_cges(Cl,D,':',NCl,NCls,TNCls0,NDs,TNDs0),
    remove_disjunctions(NCl,D,NewCls,NewDs,TNewCls,TNewDs,TNCls0,TNCls,
                                                              TNDs0,TNDs).
