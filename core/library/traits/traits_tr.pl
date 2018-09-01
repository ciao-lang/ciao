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

:- doc(bug, "Default trait predicate definitions are still not supported").

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
:- use_module(library(terms), [atom_concat/2]).

:- include(library(traits/traits_ops)).

% ---------------------------------------------------------------------------
% Database of traits

:- data trait_def/3.

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
treat_sent(end_of_file, Cs, Mod) :- !,
	clean_db_pass1(Mod),
	% TODO: This should be automatic
	Cs = [end_of_file]. % (allow other translations)
treat_sent((:- Decl), Cs, Mod) :- !,
	treat_decl(Decl, Cs, Mod).
treat_sent((H :- Body), Cs, Mod) :- !,
	treat_clause(H, Body, Cs, Mod).
treat_sent(H, Cs, Mod) :-
	treat_clause(H, true, Cs, Mod).

% Declare the trait Trait
treat_decl(trait(Trait, Decls), Cs, Mod) :-
	( check_specs(Decls) -> true
	; fail % TODO: keep error?
	),
	hook_decls(Decls, Trait, Cs),
	assertz_fact(trait_def(Trait, Mod, Decls)).
% Implement Trait for SelfFA
treat_decl(impl(Trait, SelfFA0), Cs, Mod) :-
	( atom(SelfFA0) -> SelfFA = SelfFA0/0 ; SelfFA = SelfFA0 ),
	check_spec(SelfFA),
	( atom(Trait), current_fact(trait_def(Trait, Mod, Decls)) -> true
	; message(error, ['Trait ', ~~(Trait), ' is undefined and cannot be implemented for ', ~~(SelfFA)]),
	  fail % TODO: keep error?
	),
	hook_clauses(Trait, Decls, SelfFA, Cs).

% Check that the input list is a correct list of predicate/functor specifiers F/A
check_specs([]).
check_specs([FA|FAs]) :-
	check_spec(FA),
	check_specs(FAs).

check_spec(FA) :-
	( nonvar(FA), FA = F/A, atom(F), integer(A) -> true
	; message(error, ['Incorrect predicate or functor specification ', ~~(FA)]),
	  fail
	).

% ---------------------------------------------------------------------------

% Add (internal) declarations for hook clauses for trait Trait
hook_decls([], _Trait, []).
hook_decls([FA|FAs], Trait, Cs) :-
	hook_decl(FA, Trait, Cs, Cs2),
	hook_decls(FAs, Trait, Cs2).

hook_decl(FA, Trait, Cs, Cs2) :-
	% check_spec(FA), % (already checked)
	FA = F/A,
	A1 is A + 1,
	mangle_hook(Trait, F, NF),
	Cs = [(:- discontiguous(NF/A1)),
	      (:- multifile(NF/A1))
	      |Cs2].

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

treat_clause(Head, Body, Cs, _Mod) :-
	mhead(Head, SelfStr, Trait, Head0),
	nonvar(SelfStr), % TODO: allow binding SelfVar, picking unif from head guard?
	!,
	impl_head(Head0, SelfVar, SelfStr, Trait, ImplHead, SelfVarNeeded),
	( SelfVarNeeded = no ->
	    Cs = [(ImplHead :- Body)]
	; Cs = [(ImplHead :- SelfVar = SelfStr, Body)]
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
	retractall_fact(trait_def(_, Mod, _)).

% ---------------------------------------------------------------------------

% TODO: Give errors on instantiation issues, when we are sure we have "(_ as _)._"
% (SelfAny can be SelfVar or SelfStr)
mhead(QModHead, SelfAny, Trait, Head) :-
	nonvar(QModHead), QModHead = (QMod.Head),
	nonvar(QMod), QMod = (SelfAny as Trait),
	atom(Trait),
	nonvar(Head).

