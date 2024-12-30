:- module(program_keys,
    [ cleanup_program_keys/0,
      % keys
      clause_key/2,
      clauses_keys/2,
      clause_number/2,
      first_key/2,
      last_clause/1,
      null_directive_key/1,
      make_atom/2, % (it was in s_simpspec.pl)
      % program rewriting % TODO: rewritting should not be here (clause_db)?
      inverse_rewrite_source_program/2,
      inverse_rewrite_source_program_cl/2,
      inverse_rewrite_source_body/2,
%         rewrite_source_program/2,
      rewrite_source_all_clauses/2,
      rewrite_source_clause/3,
      rewrite_cls/2,
      % decoding keys
      decode_litkey/5,
      decode_clkey/4,
      decode_entrykey/4,
      decode_predkey/3,
      is_clkey/1,
      %is_predkey/1,
      is_litkey/1,
      is_entrykey/1,
      is_directive/3,
      is_clause/4,
      orig_clause_id/2,
      % basic
      lit_ppkey/3,
      clid_of_atomid/2,
      % types
      clause/1,
      clausebody/1,
      literal_ppkey/1,
      litkey/1,
      clkey/1,
      predkey/1
    ],
    [assertions, basicmodes, regtypes, datafacts]).

:- doc(module,"This module handles the format of the structures used to
   represent the program. It is a list of clauses, where each clause is
   identified by a key, and the clause body is a conjunction of literals,
   where each literal is also identified by a key.

   When creating a new clause for a predicate @tt{H} in the program, the 
   following goal will give you a clause in the correct format:
   @begin{verbatim}
clause_key(H,ClId), rewrite_source_clause(clause(H,Body),ClId,Clause), 
   @end{verbatim}
").

:- use_module(library(lists), [append/3, length/2]).
:- use_module(engine(messages_basic), [message/2]).

:- use_module(library(compiler/p_unit/prednames), [orig_pred_name/2]).
:- use_module(library(compiler/p_unit/p_unit_basic), [type_of_goal/2, meta_to_list/2]).

%----------------------------------------------------------------------------

% :- use_package(andprolog).
% TODO: copied from andprolog_ops.pl (fixme)
:- op(950, xfy, [&]).

%----------------------------------------------------------------------------

:- doc(bug,"1. decode_litkey/5 and decode_clkey/4 should be programmed efficiently.").
:- doc(bug,"2. Code must be cleaned up.").
:- doc(bug,"4. PRED_N and the meta-terms should not be done here.").
:- doc(bug,"5. Revise first_key for the treatment of meta preds.").
:- doc(bug,"6. This is not true with user files:
    if a goal is a meta-predicate, its meta-arguments are terms of
    the form @tt{'$'(Term,Body,Type)},").
%% Module a:
%% clause('a:p',('a:a':'a:p/0/1/1',
%%               'aggregates:findall'(_1897,$('a:b','a:b':'a:p/0/1/3',goal),
%%                                    _1899):'a:p/0/1/2'))
%% User file:
%% clause('user:p',('user:a':'user:p/0/1/1',
%%               'aggregates:findall'(_1913,'user:b',_1915):'user:p/0/1/2'))

%% %----------------------------------------------------------------------------
%% % these should be obsolete by now:
%% 
%% :- prop idclause(CL) # "@var{CL} is a @tt{(clause,id)} pair.".
%% 
%% %idclause((clause(H,_B),Id)):- clause_id(H,Id).
%% idclause( (directive(_B),Id) ) :- null_directive_key(Id).
%% idclause( (directive(_B),Id) ) :- null_assertion_key(Id).
%% idclause( (directive(_B),Id) ) :- assertion_key(Id).

:- doc(hide,null_directive_key/1).
:- regtype null_directive_key/1.
null_directive_key((0,0)).

%% :- regtype null_assertion_key/1.
%% 
%% null_assertion_key((0,1)).
%% 
%% :- regtype assertion_key/1.
%% 
%% assertion_key((K,N)):- atomic(K), number(N).

% directive_key((K,N),K,N).

:- doc(doinclude,body/1).
:- prop body/1.
:- doc(body(Body),"@var{Body} is a conjunction of simple goals:
    @includedef{body/1}").
body((G,B)):-
    cgoal(G),
    body(B).
body(G):-
    cgoal(G).

%---------------------------------------------------------------------------
% The keys

%%%%%%%%%%%%%%%%%%%%%%%% program_keys_basic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(bug, "This structure is too complex to be handled easily,
    it mixes several formats... Why not to use only one like the
    one defined by the first clause?  Tip: To define it, look for the
    predicate rewrite_source_body/5 in program_keys.pl. -- EMM").

:- pred lit_ppkey/3 :: literal_ppkey * cgoal * litkey.
lit_ppkey(Lit:PPKey,  Lit, PPKey) :- !.
lit_ppkey(!,          !,   noinfo) :- !.
lit_ppkey(builtin(B), B,   noinfo) :- !.
lit_ppkey(Lit,        Lit, noinfo) :- warn_nokey(Lit).

warn_nokey(Lit) :- message(warning, ['This literal: ', ''(Lit), ' does not have a key']).

:- pred clid_of_atomid(+AtomId, -ClId) :: atm * atm
    # "Returns in @var{ClId} the clause identifier corresponding to
      @var{AtomId} goal identifier.".
clid_of_atomid(GoalKey, ClauseKey) :-
    atom_codes(GoalKey, Gs),
    append(Cs, [0'/|GoalId], Gs),
    number_codes(_, GoalId),
    atom_codes(ClauseKey, Cs).

%---------------------------------------------------------------------------
% Type definition

:- regtype litkey(Key)
# "@var{Key} is an atom that uniquely identifies a program point
       of a program clause body literal.".
litkey(Key) :- atm(Key).

:- regtype clkey(Id)
    # "@var{Id} is an atom that uniquely identifies a program clause.".
clkey(Clid) :- atm(Clid).

:- regtype predkey(Id)
    # "@var{Id} is an atom that uniquely identifies a program predicate.".
predkey(Clid) :- atm(Clid).

%----------------------------------------------------------------------------
% The type of the program clauses

:- regtype clause/1.
:- doc(clause/1, "Defined as: @includedef{clause/1}").
clause(clause(Head, Body) :Clid) :-
    cgoal(Head),
    clausebody(Body),
    clkey(Clid).
clause(directive(Body) :Clid) :-
    clausebody(Body),
    drid(Clid).

:- doc(doinclude, drid/1).
:- regtype drid(Id)
    # "@var{Id} is a number that uniquely identifies a program directive.".
drid(Clid) :- nnegint(Clid).

:- prop clausebody/1.
:- doc(clausebody(Body), "@var{Body} is a conjunction of simple goals;
    if a goal is a meta-predicate, its meta-arguments are terms of
    the form @tt{'$'(Term,Body,Type)}, where @tt{Term} is the original
    goal term, @tt{Type} the type of meta-term as in the meta_predicate
    directive, and, if @tt{Type} corresponds to an executable form of
    meta-term, @tt{Body} is the @tt{clausebody} that corresponds to
    @tt{Term}.
    @includedef{clausebody/1}").

clausebody(L) :-
    literal_ppkey(L).
clausebody((L, B)) :-
    literal_ppkey(L),
    clausebody(B).

:- regtype literal_ppkey/1.
literal_ppkey(Lit:PPKey) :-
    cgoal(Lit),
    litkey(PPKey).
literal_ppkey(builtin(B) :noinfo) :-
    cgoal(B).
literal_ppkey(!).

% ----------------------------------------------------------------------
% Key state

:- export(counter/3).
:- data counter/3.
:- data decode_clkey_/4.

:- doc(cleanup_program_keys/0,"Cleanups internal database.").
cleanup_program_keys :-
    retractall_fact(counter(_,_,_)),
    retractall_fact(decode_clkey_(_,_,_,_)).

%% directive_inccounter( Val) :-
%%         ( retract_fact(counter( 0 , OldVal,_)) -> 
%%        Val is OldVal + 1
%%      ; Val = 1 ),
%%         asserta_fact(counter( 0, Val, Val)).

% TODO: rename by new_clause_key (it increments clause counter)
:- pred clause_key(H,ClId) : head * var => clkey(ClId)
    # "@var{ClId} is the clause key that an additional clause of
       predicate @var{H} should have.".
%% clause_key(0,Clid):-
%%      clause_count(directive,Clid).
%% clause_key(1,Clid):-
%%      clause_count(assertion,Clid).
clause_key(H,Clid):-
    predkey_from_sg(H, Key),
    clause_count(Key,Clid).

clause_count(Key,Clid):-
    ( retract_fact(counter(Key, OldVal,_)) -> 
      Val is OldVal + 1
    ; Val = 1 ),
    make_atom_internal([Key,Val],Clid),
    asserta_fact( counter(Key, Val, Clid) ).

clause_number(H,N):-
    functor(H,F,A),
    make_atom_internal([F,A],Key),
    current_fact( counter(Key, N, _) ).

:- regtype head(Head) # "@var{Head} is a clause head identifier.".
:- doc(head/1,"Defined as @includedef{head/1}").
:- doc(doinclude,head/1).

head(0).
head(1).
head(X):- cgoal(X).

:- pred clauses_keys(H,ClIds) : cgoal * var => cgoal * list(clkey)
    # "@var{ClIds} are the clause keys of all clauses already known
       for predicate @var{H}.".
clauses_keys(H,Keys):-
    predkey_from_sg(H, Key),
    current_fact( counter(Key, N, _) ),
    clauses_keys_(Key,0,N,Keys).

clauses_keys_(_Key,N0,N,CutCls):- N0>N, !,
    CutCls=[].
clauses_keys_(Key,N0,N,[Id|CutCls]):-
    make_atom_internal([Key,N0],Id),
    N1 is N0+1,
    clauses_keys_(Key,N1,N,CutCls).

:- pred last_clause(ClId) : clkey(ClId)
    # "@var{ClId} is the clause key of the last known clause of
       the predicate to which it corresponds.".
last_clause(Clid):-
    counter(_, _,Clid).

%----------------------------------------------------------------------------
% Rewrite source program, putting in ':' to all goals
% input:  folded format - plain syntax - with '\&'
% output: internal format - plain syntax - with '\&'
%----------------------------------------------------------------------------
% rewrite_source_program( Cls, NCls ) :-
%       cleanup_program_keys,
%       rewrite_source_program_(Cls,NCls).
% 
% rewrite_source_program_([],[]).
% rewrite_source_program_([clid(Cl,Loc)|Cls],[clid(NCl,Clid,Loc)|NCls]):-
%       rewrite_source_program_3(Cl,Clid,NCl),
%       rewrite_source_program_(Cls,NCls).
% 
% rewrite_source_program_3(directive(D),Clid,directive(D)):- !,
%       directive_inccounter( Clid ).
% %     clause_key(0,Clid).
% rewrite_source_program_3(clause(H,true),Clid,clause(H,true)):- !,
%       clause_key(H,Clid).
% rewrite_source_program_3(clause(H,!),Clid,clause(H,!)):- !,
%       clause_key(H,Clid).
% rewrite_source_program_3(clause(H,B),Clid,clause(H,NB)):- !,
%       clause_key(H,Clid),
%       rewrite_source_body(B,Clid,0,_,NB).

:- doc(rewrite_source_all_clauses(Clauses,NewClauses),
      "@var{NewClauses} is the result of applying the format
      transformation to the list of clauses @var{Clauses}.").
% TODO: this creates keys! assert_program?
rewrite_source_all_clauses([],[]).
rewrite_source_all_clauses([clause(H,B)|Clauses],[NCl:ClId|NewClauses]):-
    clause_key(H,ClId), 
    rewrite_source_clause(clause(H,B),ClId,NCl),
    rewrite_source_all_clauses(Clauses,NewClauses).

:- prop headcl/1.
headcl(0).
headcl(H) :- cgoal(H).

:- pred rewrite_source_clause(Clause0,ClId,Clause)
    : clause * clkey * var => clause * clkey * clause
    # "@var{Clause} is an structure representing the clause 
      @var{Clause0} identified by @var{ClId} (does nothing for directives).".
rewrite_source_clause(Cl, _Clid, Cl):- Cl = directive(_), !.
rewrite_source_clause(clause(H,B), Clid, clause(H,NB)) :-
    rewrite_source_body(B,Clid,0,_,NB).

rewrite_source_body(A,Clid,N,N1,AK):-
    var(A), !,
    rewrite_source_atom(A,Clid,N,N1,AK).
rewrite_source_body((A,B),K,N0,N,(NA,NB)):- !,
    rewrite_source_body(A,K,N0,N1,NA),
    rewrite_source_body(B,K,N1,N,NB).
rewrite_source_body(!,_,N,N,!):- !.
rewrite_source_body(builtin(B),_,N,N,builtin(B):noinfo):- !.
rewrite_source_body(A,Clid,N,N1,AK):-
    type_of_goal(metapred(_Type,Meta),A), !,
    functor(A,F,Args),
    functor(B,F,Args),
    rewrite_source_atom(B,Clid,N,N0,AK),
    meta_to_list(Meta,MetaL),
    meta_calls(A,0,Args,MetaL,Clid,N0,N1,B).
rewrite_source_body(A:K,_Clid,N,N,A:K):- !.
rewrite_source_body(A,Clid,N,N1,AK):-
    rewrite_source_atom(A,Clid,N,N1,AK).

%rewrite_source_atom(A:_,Clid,N,N1,A:Key):-
%       N1 is N+1,
%       make_atom_internal([Clid,N1],Key).
rewrite_source_atom(A,Clid,N,N1,A:Key):-
    N1 is N+1,
    make_atom_internal([Clid,N1],Key).

meta_calls(_,Args,Args,_Meta,_Clid,N,N,_):- !.
meta_calls(A,Arg0,Args,[M1|Meta],Clid,N0,N,B):-
    Arg1 is Arg0+1,
    arg(Arg1,A,A1),
%       arg(Arg1,Meta,M1),
    ( meta_term(M1,A1,Meta1,goal)
    -> rewrite_source_body(Meta1,Clid,N0,N1,A2),
       B1='$'(A1,A2,goal)
     ; B1=A1,
       N1=N0
    ),
    arg(Arg1,B,B1),
    meta_calls(A,Arg1,Args,Meta,Clid,N1,N,B).

% This may add variables (spec and pred(N)), which are not in the
% dictionaries of the clauses!!!
meta_term(goal,B,B,goal).
meta_term(:,B,B,goal).
meta_term(clause,D,D,data).
meta_term(fact,D,D,data).
meta_term(spec,Term,G,goal):-
    ( nonvar(Term), Term=F/A, nonvar(F), nonvar(A)
      -> functor(G,F,A)
       ; true % G a variable 
    ).
meta_term(pred(N),Term,G,goal):-
    ( nonvar(Term), number(N)
    -> length([A|L],N),
       Term=..[F|As],
       append([A|As],L,Args),
       G=..[F|Args]
     ; true % G a variable
    ).

%----------------------------------------------------------------------------
% Rewrite source program, by just taking out ':'
% input:  internal format - full syntax
% output: folded format - full syntax
%----------------------------------------------------------------------------
% THIS_ONE_NEEDS_REVISION
:- doc(hide,inverse_rewrite_source_program/2).

inverse_rewrite_source_program([],[]) :- !.
inverse_rewrite_source_program([Cl:_|Cls],[NCl|NCls]):-
    !,
    inverse_rewrite_source_program_cl(Cl,NCl),
    inverse_rewrite_source_program(Cls,NCls).

inverse_rewrite_source_program_cl(directive(D),directive(D)) :- !.
inverse_rewrite_source_program_cl(clause(H,true),clause(H,true)):- !.
inverse_rewrite_source_program_cl(clause(H,!),clause(H,!)):- !.
inverse_rewrite_source_program_cl(clause(H,B),clause(H,NB)):-
    !,
    inverse_rewrite_source_body(B,NB).
    
inverse_rewrite_source_body((A,B),(NA,NB)):-
    !,
    inverse_rewrite_source_body(A,NA),
    inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body((A&B),(NA&NB)):-
    inverse_rewrite_source_body(A,NA),
    inverse_rewrite_source_body(B,NB).
%% inverse_rewrite_source_body((A\&B),(NA\&NB)):-
%%      inverse_rewrite_source_body(A,NA),
%%      inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body((A->B;C),(NA->NB;NC)):-
    !,
    inverse_rewrite_source_body(A,NA),
    inverse_rewrite_source_body(B,NB),
    inverse_rewrite_source_body(C,NC).
inverse_rewrite_source_body((A->B),(NA->NB)):-
    !,
    inverse_rewrite_source_body(A,NA),
    inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body((A=>B),(NA=>NB)):-
    !,
    inverse_rewrite_source_body(A,NA),
    inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body(!,!):- !.
inverse_rewrite_source_body(\+($(A,_,_)):_,\+(A)):- !.
%inverse_rewrite_source_body('hiord_rt:call'($(A,_,_)):_,A):- !.
inverse_rewrite_source_body('hiord_rt:call'($(A,_,_)):_,NBody):- !,
    NBody = 'hiord_rt:call'(A).
inverse_rewrite_source_body('hiord_rt:call'($(A,_,_),Args):_,NBody):- !,
    NBody = 'hiord_rt:call'(A,Args).
inverse_rewrite_source_body('aggregates:bagof'(Var,$(A,_,_),List):_Id,NBody):-!,
    NBody = 'aggregates:bagof'(Var,A,List).
inverse_rewrite_source_body('aggregates:setof'(Var,$(A,_,_),List):_Id,NBody):-!,
    NBody = 'aggregates:setof'(Var,A,List).
inverse_rewrite_source_body('aggregates:findall'(Var,$(A,_,_),List):_Id,NBody):-!,
    NBody = 'aggregates:findall'(Var,A,List).
inverse_rewrite_source_body(A:_,A):- !.
inverse_rewrite_source_body(A,A):- !,
    throw(error(cannot_process(A), inverse_rewrite_source_body/2)).

%% inverse_rewrite_source_body(ground(L),ground(L)):- !.
%% inverse_rewrite_source_body(indep(L),indep(L)):- !.

%---------------------------------------------------------------------------
% TODO: it may not work if predicates contain '/' characters, fix!

:- pred is_litkey(Id):: atm 
# "Succeeds if @var{Id} is a valid atom identifier. The predicate name
  must not contain '/' characters.".
is_litkey(Id):-
    decode_litkey(Id,_N,_A,_C,_G).

:- pred is_clkey(Id):: atm
# "Succeeds if @var{Id} is a valid clause identifier. The predicate
  name must not contain '/' characters.".

is_clkey(Id):-
    \+ decode_litkey(Id,_,_,_,_),
    decode_clkey(Id,_N,_A,_C).

:- pred is_predkey(Id):: atm
# "Succeeds if @var{Id} is a valid predicate identifier. The predicate
  name must not contain '/' characters.".
is_predkey(Id):-
    \+ decode_litkey(Id,_,_,_,_),
    \+ decode_clkey(Id,_N,_A,_C),
    decode_predkey(Id,_,_).

:- pred is_entrykey(Id):: atm
# "Succeeds if @var{Id} is a valid @em{entry point} identifier
  (predkey for exported predicates, clkey for entry assertions)".

is_entrykey(Id):- is_clkey(Id), !.
is_entrykey(Id):- is_predkey(Id).

%---------------------------------------------------------------------------
% Management of internal keys
%---------------------------------------------------------------------------
% These should become obsolote:

first_key(((!),Right),Key):-!, % Added by PLG 11-Oct-04
    first_key(Right,Key).
first_key((_:noinfo,Right),Key):-!,
    first_key(Right,Key).
first_key((Left,_Right),Key):-
    first_key(Left,Key).
first_key((Left & _Right),Key):-
    first_key(Left,Key).
%% first_key((Left \& _Right),Key):-
%%      first_key(Left,Key).
first_key(_:noinfo,_):-!,
    fail.
first_key((_:Key),Key).

:- doc(hide,rewrite_cls/2).

rewrite_cls([clid(Cl,_,_)|Cls0],[Cl|Cls]):-
    rewrite_cls(Cls0,Cls).
rewrite_cls([],[]).

%-------------------------------------------------------------------%
% the next two predicates are necessary to get the information not  %
% directly available stored in the keys for goals and clauses       % 
%-------------------------------------------------------------------%

:- pred decode_predkey(PredId,F,A) :: predkey * atm * num
    # "@var{PredId} identifies the predicate @var{F}/@var{A}.".
decode_predkey(Key, _, _):- var(Key), !, throw(error(bad_key, decode_predkey/3)).
decode_predkey(Key, N, A):-
    unpack_id(Key, List),
    append(Name, [LA], List), !,
    unpack_id(N, Name),
    name(A, LA).

decode_entrykey(Clid,N,A,C):-
    decode_clkey(Clid,N,A,C).

% TODO: Improved performance by caching, implement in C
:- pred decode_clkey(ClId,F,A,C) :: clkey * atm * num * num
    # "@var{ClId} identifies the @var{C}th clause of predicate
      @var{F}/@var{A}.".
decode_clkey(Clid,N,A,C):-
    decode_clkey_(Clid,N0,A0,C0), !,
    N = N0, A = A0, C = C0.
decode_clkey(Clid,N,A,C):-
    decode_clid(Clid,N0,A0,C0),
    assertz_fact(decode_clkey_(Clid,N0,A0,C0)),
    N = N0, A = A0, C = C0.

decode_clid(Clid,N,A,C):-
    unpack_id(Clid, List),
    append(Name, [LA, LC], List), !,
    unpack_id(N, Name),
    name(A,LA),
    name(C,LC).

:- pred decode_litkey(AtId,F,A,C,G) :: litkey * atm * num * num * num
    # "@var{AtId} identifies the @var{G}th literal of the body of the
      @var{C}th clause of predicate @var{F}/@var{A}.".
decode_litkey(Atom,N,A,C,G):-
    unpack_id(Atom, List),
    append(Name, [LA, LC,LG], List), !,
    unpack_id(N, Name),
    name(A,LA),
    name(C,LC),
    name(G,LG).

is_directive(directive(Body):Clid, Body, Clid).
is_clause(clause(Head,Body):Clid, Head, Body, Clid).

:- doc(orig_clause_id(ClId,Orig_ClId), "@var{Orig_ClId} is the
      clause identifier resulting from replacing the name of a newly
      generated predicate with the name of its corresponding predicate
      in the original program. ").
orig_clause_id(ClId,Orig_ClId):-
    decode_clkey(ClId,Pred,Arity,Clause),
    orig_pred_name(Pred,Orig_Pred),
    make_atom_internal([Orig_Pred,Arity,Clause],Orig_ClId).

:- pred unpack_id(Atom,IdList) :: atm * list(string)
# "Given an atom @var{String} (which contains 0'/ characters) it
returns a list of substrings of @var{String} contained between 0'/
characters.".

% An atom is given...
unpack_id(A, B) :-
    atom(A),
    atom_codes(A, AA),
    unpack_id_(AA, B),
    !.
% The atom is the output
unpack_id(A, B) :-
    unpack_id_(AA, B),
    !,
    atom_codes(A, AA).

unpack_id_([], [[]]).
unpack_id_([0'/|R], [[]|L]) :- !,
    unpack_id_(R, L).
unpack_id_([A|R], [[A|AA]|As]) :-
    unpack_id_(R, [AA|As]).

:- pred make_atom_internal(AtomNumberList,Id) :: list(constant) * atm
# "@var{Id} is the atom obtained from contatenating each element of
    @var{AtomNumberList} with '/' between elements.".
% TODO: which implementation is better?
make_atom_internal(AtomList, Key) :-
    namel_2_atoml(AtomList, StrList),
    unpack_id(Key, StrList).

namel_2_atoml([], []).
namel_2_atoml([A|AA], [S|SS]) :-
    name(A, S),
    namel_2_atoml(AA, SS).

% (it was in s_simpspec.pl)
% TODO: review, merge with make_atom_internal/2?
make_atom(X,Y):-
    make_name(X,Z),
    name(Y,Z).

make_name([X],X1) :-
    !,
    name(X,X1).
make_name([X|Y],Z) :-
    name(X,X1),
    make_name(Y,Y1),
    append(X1,[0'/|Y1],Z).

% ------------------------------------------------------------
% Generating keys
:- export(get_predkey/3).
:- pred get_predkey(F,A,Key) : atm * atm * var => predkey(Key)
    #"@var{Key} is the key corresponding to predicate @var{F}/@var{A}".
get_predkey(F,A,Key) :-
    make_atom_internal([F,A],Key).

:- export(get_predkeys/2).
:- pred get_predkeys(Preds,Keys) : list(Preds) => list(predkey, Keys)
    #"@var{Keys} is the list of atoms obtained from @var{Preds}.".
get_predkeys([],[]).
get_predkeys([(N/A)|Preds],[Key|Keys]):-
    get_predkey(N,A,Key),
    get_predkeys(Preds,Keys).

% ------------------------------------------------------------
:- export(predkey_from_sg/2).
:- pred predkey_from_sg(Sg, Key) : term * var => predkey(Key)
    #"@var{Key} is the key corresponding to functor @var{Sg}".
predkey_from_sg(Sg, Key) :-
    functor(Sg, F, A),
    make_atom_internal([F,A], Key), !. % IG make_atom may leave choicepoints

:- export(get_clkey/4).
:- pred get_clkey(F,A,Cl,ClKey) : atm * atm * atm * var
    => clkey(ClKey)
    #"@var{ClKey} is the key corresponding to clause
    @var{F}/@var{A}/@var{Cl}".
get_clkey(F,A,Cl,ClKey) :-
    make_atom_internal([F,A,Cl],ClKey).

:- export(get_litkey/5).
:- pred get_litkey(F,A,Cl,Lit,ClKey) : atm * atm * atm * atm * var
    => litkey(ClKey)
    #"@var{ClKey} is the key corresponding to clause
    @var{F}/@var{A}/@var{Cl}/@var{Lit}".
get_litkey(F,A,Cl,Lit,ClKey) :-
    make_atom_internal([F,A,Cl,Lit],ClKey).
