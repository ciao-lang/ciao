:- module(traits_tr, [traits_sent/3, traits_goal/3], [assertions, datafacts]).

:- doc(title, "Translation for traits modules").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements a simple compilation scheme for
   traits using multifile predicates.

   It produces the following elements:
   @begin{itemize}
   @item remember trait predicate obligations
   @item add discontiguous/multifile clauses for trait hooks
   @item translate trait goal calls to trait hook calls
   @item add trait hook clauses for each impl
   @end{itemize}
").

:- doc(bug, "Document syntax for default predicate definitions").
:- doc(bug, "Document that trait defs can be discontiguous").

% TODO: allow assertions
% TODO: allow :- pred gadget.p1. syntax (if gadget is declared as trait)
% TODO: allow impl with []
% TODO: allow { ... } for decls.

% TODO: more syntactic sugar:
%
%   - Automatic lookup for trait based on pred name
%       (given :- impl(gadget, datum2/2))
%       datum2(_,_).p1 ===> (datum2(_,_) as gadget).p1
%   - Modules implementing traits
%       :- impl(gadget).  ===> :- impl(gadget,<modulename>)
%   - For modules implementing traits better add the trait explicitly?
%       gadget.p1 :- true.

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [member/2, append/3]).

:- include(library(traits/traits_ops)).

% ---------------------------------------------------------------------------
% Database of traits (per module M)

% trait_def(Trait, M): Trait defined
:- data trait_def/2.
% trait_spec(Trait, M, F, A): pred spec F/A for trait
:- data trait_spec/4.
% trait_defimpl(Trait, M, SelfVar, Head, Body): default clause for trait
:- data trait_defimpl/5.
% trait_impl(Trait, M, SelfFA): SelfFA implements Trait in M
:- data trait_impl/3.
% defined_impl(F,A,M): F/A defined in M
:- data defined_impl/3.

% ---------------------------------------------------------------------------

traits_sent(C, Cs, Mod) :-
    treat_sent(C, Cs, Mod).

traits_goal(G, G2, _Mod) :-
    mhead(G, GSelf, Trait, G1),
    trait_head(G1, GSelf, Trait, G2).

% ---------------------------------------------------------------------------

% trait_head(+Head, +SelfAny, +Trait, -TraitHead):
%   TraitHead is the Head for the trait clause
%   (that redirects to the implementation given by SelfAny).
trait_head(Head, SelfAny, Trait, TraitHead) :-
    Head =.. [F|Args],
    mangle_hook(Trait, F, NF),
    TraitHead =.. [NF, SelfAny|Args].

% ---------------------------------------------------------------------------

treat_sent(0, _, Mod) :- !,
    clean_db_pass1(Mod).
treat_sent(end_of_file, Cs2, Mod) :- !,
    emit_sents(Mod, Cs),
    clean_db_pass1(Mod),
    % TODO: This should be automatic
    append(Cs, [end_of_file], Cs2). % (allow other translations)
treat_sent((:- Decl), Cs, Mod) :- !,
    treat_decl(Decl, Cs, Mod).
treat_sent(C, Cs, Mod) :-
    norm_clause(C, H, Body),
    treat_clause(H, Body, Cs, Mod).

norm_clause((H0 :- B0), H, B) :- !, H = H0, B = B0.
norm_clause(H, H, true).

treat_decl(trait(Trait, Decls), Cs, Mod) :-
    add_trait(Trait, Decls, Cs, Mod).
treat_decl(impl(Trait, SelfFA0), Cs, Mod) :-
    add_impl(Trait, SelfFA0, Cs, Mod).

% ---------------------------------------------------------------------------
% Declare the trait Trait

add_trait(Trait, Decls, Cs, Mod) :-
    ( check_trait_decls(Decls) -> true
    ; fail % TODO: keep error?
    ),
    % define trait
    add_trait_def(Trait, Mod),
    % store declarations
    ( % (failure-driven loop)
      member(Decl, Decls),
        add_trait_decl(Decl, Trait, Mod),
        fail
    ; true
    ),
    % create hook decls
    findall(HeadF/HeadA, trait_spec(Trait, Mod, HeadF, HeadA), Decls2),
    hook_decls(Decls2, Trait, Cs).

add_trait_def(Trait, Mod) :-
    ( current_fact(trait_def(Trait, Mod)) -> true % already defined
    ; assertz_fact(trait_def(Trait, Mod))
    ).

% Check that the input list is a correct list of predicate/functor specifiers F/A
check_trait_decls([]).
check_trait_decls([FA|FAs]) :-
    check_trait_decl(FA),
    check_trait_decls(FAs).

check_trait_decl(_). % TODO: check?

check_spec(FA) :-
    ( nonvar(FA), FA = F/A, atom(F), integer(A) -> true
    ; message(error, ['Incorrect predicate or functor specification ', ~~(FA)]),
      fail
    ).

add_trait_decl(F/A, Trait, Mod) :- !,
    add_trait_spec(F, A, Trait, Mod).
add_trait_decl(DefC, Trait, Mod) :- !, % (default implementation)
    norm_clause(DefC, Head0, Body),
    nonvar(Head0),
    ( Head0 = (SelfVar.Head) -> true
    ; Head0 = Head % no SelfVar
    ),
    functor(Head, HeadF, HeadA),
    add_trait_spec(HeadF, HeadA, Trait, Mod),
    assertz_fact(trait_defimpl(Trait, Mod, SelfVar, Head, Body)).

add_trait_spec(F, A, Trait, Mod) :-
    ( current_fact(trait_spec(Trait, Mod, F, A)) -> true
    ; assertz_fact(trait_spec(Trait, Mod, F, A))
    ).

% Add (internal) declarations for hook clauses for trait Trait
hook_decls([], _Trait, []).
hook_decls([FA|FAs], Trait, Cs) :-
    hook_decl(FA, Trait, Cs, Cs2),
    hook_decls(FAs, Trait, Cs2).

hook_decl(FA, Trait, Cs, Cs2) :-
    FA = F/A,
    !,
    A1 is A + 1,
    mangle_hook(Trait, F, NF),
    Cs = [(:- discontiguous(NF/A1)),
          (:- multifile(NF/A1))
          |Cs2].
hook_decl(_FA, _Trait, Cs, Cs). % TODO: check?

% ---------------------------------------------------------------------------
% Declare a trait Trait implementation for SelfFA0

add_impl(Trait, SelfFA0, Cs, Mod) :-
    ( atom(SelfFA0) -> SelfFA = SelfFA0/0 ; SelfFA = SelfFA0 ),
    check_spec(SelfFA),
    check_trait(Trait, Mod),
    findall(HeadF/HeadA, trait_spec(Trait, Mod, HeadF, HeadA), Decls),
    ( current_fact(trait_impl(Trait, Mod, SelfFA)) -> true % already defined
    ; assertz_fact(trait_impl(Trait, Mod, SelfFA)),
      hook_clauses(Trait, Decls, SelfFA, Cs)
    ).

% check_trait(+Trait, +Mod): Check that Trait is defined
check_trait(Trait, Mod) :-
    ( atom(Trait), current_fact(trait_def(Trait, Mod)) -> true
    ; message(error, ['Trait ', ~~(Trait), ' is undefined']),
      fail % TODO: keep error?
    ).

% Insert hook clauses for Trait implementation for SelfFA
hook_clauses(Trait, Decls, SelfFA, Cs) :-
    % atom(Trait), % (already checked)
    % check_spec(SelfFA), % (already checked)
    SelfFA = SelfF/SelfA,
    hook_clauses_(Decls, SelfF, SelfA, Trait, Cs).

hook_clauses_([], _SelfF, _SelfA, _Trait, []).
hook_clauses_([HeadF/HeadA|Preds], SelfF, SelfA, Trait, [C|Cs]) :-
    hook_clause(SelfF, SelfA, HeadF, HeadA, Trait, C),
    hook_clauses_(Preds, SelfF, SelfA, Trait, Cs).

% ---------------------------------------------------------------------------

% A trait impl clause is of the form:
%   (SelfAny as Trait).Head :- Body
% where SelfAny is a structure or a variable. When SelfAny is a
% variable, the first unification of Body must be of the form
% SelfAny=<structure>.
treat_clause(Head, Body, Cs, Mod) :-
    mhead(Head, SelfAny, Trait, Head0),
    !,
    ( var(SelfAny) ->
        SelfVar = SelfAny,
        ( scan_selfunif(Body, SelfVar, SelfStr) ->
            true
        ; message(error, ['Unbound self in trait implementation: ', ~~(Trait)]),
          fail  % TODO: keep error?
        )
    ; SelfStr = SelfAny % (SelfVar not needed)
    ),
    treat_mclause(SelfVar, SelfStr, Trait, Head0, Body, Cs, Mod).

treat_mclause(SelfVar, SelfStr, Trait, Head0, Body, Cs, Mod) :-
    impl_head(Head0, SelfVar, SelfStr, Trait, ImplHead, SelfVarNeeded),
    add_defined_impl(ImplHead, Mod),
    ( SelfVarNeeded = no ->
        Cs = [(ImplHead :- Body)]
    ; Cs = [(ImplHead :- SelfVar = SelfStr, Body)]
    ).

% Scan leftmost body literal to get a SelfVar=SelfStr unification
% TODO: merge with index scanning
scan_selfunif(Body, SelfVar, SelfStr) :-
    leftmost_lit(Body, L),
    ( L = (A=B) ; L = (B=A) ),
    A==SelfVar,
    nonvar(B),
    !,
    SelfStr=B.

leftmost_lit(A, _) :- var(A), !, fail.
leftmost_lit((A,_), X) :- leftmost_lit(A,X).
leftmost_lit(X, X).

add_defined_impl(ImplHead, Mod) :-
    functor(ImplHead, F, A),
    ( current_fact(defined_impl(F, A, Mod)) ->
        true
    ; assertz_fact(defined_impl(F, A, Mod))
    ).

% hook_clause(+StrF, +StrA, +HeadF, +HeadA, +Trait, -C):
%   Creates a hook clause for predicate HeadF/HeadA
%   between the trait Trait and its implementation for
%   functor StrF/StrA.

hook_clause(StrF, StrA, HeadF, HeadA, Trait, C) :-
    functor(SelfStr, StrF, StrA),
    functor(Head, HeadF, HeadA),
    impl_head(Head, SelfVar, SelfStr, Trait, ImplHead, SelfVarNeeded),
    trait_head(Head, SelfVar, Trait, TraitHead),
    ( SelfVarNeeded = no -> % TODO: not needed for indexing; better for some CiaoPP uses
        SelfVar = SelfStr,
        C = (TraitHead :- ImplHead)
    ; C = (TraitHead :- SelfVar = SelfStr, ImplHead)
    ).

% ---------------------------------------------------------------------------

% Name mangling for hook clauses
mangle_hook(Trait, F, NF) :-
    atom_concat([Trait, '.', F], NF).

% Name mangling for impl clauses
mangle_impl(SelfF, SelfA, Trait, F, NF) :-
    ( SelfA = 0 -> % TODO: use just one mangling?
        atom_concat(['<', SelfF, ' as ', Trait, '>', '.', F], NF)
    ; atom_number(SelfA2, SelfA),
      atom_concat(['<', SelfF, '/', SelfA2, ' as ', Trait, '>', '.', F], NF)
    ).

% ---------------------------------------------------------------------------

% impl_head(+Head, +SelfVar, +SelfStr, +Trait, -ImplHead, -SelfVarNeeded):
%   Obtain the implementation head for a given functor,
%   trait, and predicate head:
%
%    - Head: predicate head
%    - SelfVar: the self variable (non-instantiated)
%    - SelfStr: the self functor (instantiated)
%    - Trait: the trait name
%    - ImplHead: the Head for the implementation clause
%    - SelfVarNeeded: ImplHead uses the self variable

impl_head(Head, SelfVar, SelfStr, Trait, ImplHead, SelfVarNeeded) :-
    functor(SelfStr, SelfF, SelfA),
    Head =.. [F|Args],
    mangle_impl(SelfF, SelfA, Trait, F, NF),
    insert_self(SelfVar, SelfStr, Args, NArgs, SelfVarNeeded),
    ImplHead =.. [NF|NArgs].

% ---------------------------------------------------------------------------

% Insert self in arguments if needed:
%  - if SelfStr is atomic, nothing is added
%  - if SelfStr is unary, data is "unboxed"
%  - otherwise, SelfVar is added

insert_self(SelfVar, SelfStr, Args, NArgs, SelfVarNeeded) :-
    ( atomic(SelfStr) ->
        % No SelfData is needed
        NArgs = Args,
        SelfVarNeeded = no
    ; % Obtain SelfData
      ( functor(SelfStr, _, 1) -> % Data just one arg
          arg(1, SelfStr, SelfData),
          SelfVarNeeded = no
      ; % whole Self is data
        SelfData = SelfVar,
        SelfVarNeeded = yes
      ),
      insert_selfdata(Args, SelfData, NArgs)
    ).

% Insert SelfData (second arg there are 1 or more, for better indexing)
insert_selfdata(Args, SelfData, NArgs) :-
    ( Args = [Arg1|Args2] -> NArgs = [Arg1, SelfData|Args2]
    ; NArgs = [SelfData]
    ).

% ---------------------------------------------------------------------------

clean_db_pass1(Mod) :-
    retractall_fact(trait_def(_, Mod)),
    retractall_fact(trait_spec(_, Mod, _, _)),
    retractall_fact(trait_defimpl(_, Mod, _, _, _)),
    retractall_fact(trait_impl(_, Mod, _)),
    retractall_fact(defined_impl(_,_,Mod)).

% ---------------------------------------------------------------------------

% TODO: Give errors on instantiation issues, when we are sure we have "(_ as _)._"
% (SelfAny can be SelfVar or SelfStr)
mhead(QModHead, SelfAny, Trait, Head) :-
    nonvar(QModHead), QModHead = (QMod.Head),
    nonvar(QMod), QMod = (SelfAny as Trait),
    atom(Trait),
    nonvar(Head).

% ---------------------------------------------------------------------------

emit_sents(Mod, Cs) :-
    findall(C, emit_sent(C, Mod), Cs).

emit_sent(C, Mod) :-
    % Add default implementations for all non-implemented preds
    trait_impl(Trait, Mod, SelfFA),
    SelfFA=StrF/StrA,
    functor(SelfStr, StrF, StrA),
    trait_spec(Trait, Mod, HeadF, HeadA),
    functor(Head, HeadF, HeadA),
    \+ pred_has_impl(SelfStr, Trait, Head, Mod),
    %
    get_defimpl(SelfStr, Trait, Head, Mod, C). % (nondet)
emit_sent(end_of_file, _Mod).

% (SelfStr as Trait).Head is defined
pred_has_impl(SelfStr, Trait, Head, Mod) :-
    impl_head(Head, _SelfVar, SelfStr, Trait, ImplHead, _SelfVarNeeded),
    functor(ImplHead, ImplF, ImplA),
    defined_impl(ImplF, ImplA, Mod).

% Get default implementation clause(s) C for (SelfStr as Trait).Head
get_defimpl(SelfStr, Trait, Head, Mod, C) :-
    trait_defimpl(Trait, Mod, SelfVar, Head, Body), % (nondet)
    Head2 = ((SelfVar as Trait).Head),
    Body2 = (SelfVar=SelfStr, Body),
    treat_clause(Head2, Body2, Cs2, Mod),
    member(C, Cs2).

