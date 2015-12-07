:- module(data_facts, [
        asserta_fact/1, asserta_fact/2, assertz_fact/1, assertz_fact/2,
        current_fact/1, current_fact/2, retract_fact/1, retractall_fact/1,
        current_fact_nb/1, retract_fact_nb/1,
        close_predicate/1, open_predicate/1,
        set_fact/1, erase/1],
        [assertions, nortchecks, isomodes]).

:- doc(title,"Fast/concurrent update of facts").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"@cindex{data predicate}Prolog implementations
    traditionally implement the concept of @concept{dynamic
    predicate}s: predicates which can be inspected or modified at
    run-time, adding or deleting individual clauses.  The power of
    this feature comes at a cost: as new clause bodies can be
    arbitrarily added to the program, new predicate calls can arise
    which are not 'visible' at compile-time, thus complicating global
    analysis and optimization of the code.  But it is the case that
    most of the time what the programmer wants is simply to store
    data, with the purpose of sharing it between search branches,
    predicates, or even execution threads.  In Ciao the concept of
    data predicate serves this purpose: a data predicate is a
    predicate composed exclusively by facts, which can be inspected,
    and dynamically added or deleted, at run-time.  Using data
    predicates instead of normal dynamic predicates brings benefits in
    terms of speed, but above all makes the code much easier to
    analyze automatically and thus allows better
    optimization.@cindex{concurrent predicate} @cindex{data
    declaration}

    Also, a special kind of data predicates exists, @index{concurrent
    predicates}, which can be used to communicate/synchronize among
    different execution threads (see @ref{Low-level
    concurrency/multithreading primitives}).

    Data predicates must be declared through a @decl{data/1}
    declaration.  Concurrent data predicates must be declared through
    a @decl{concurrent/1} declaration.

").

:- doc(doinclude,data/1).

:- true decl data(Predicates) : sequence_or_list(predname)
        # "Defines each predicate in @var{Predicates} as a @concept{data
          predicate}.  If a predicate is defined data in a file, it must
          be defined data in every file containing clauses for that
          predicate. The directive should precede all clauses of the
          affected predicates.  This directive is defined as a prefix
          operator in the compiler.".

:- doc(doinclude,concurrent/1).

:- true decl concurrent(Predicates) : sequence_or_list(predname)
        # "Defines each predicate in @var{Predicates} as a
          @concept{concurrent predicate}.  If a predicate is defined
          concurrent in a file, it must be defined concurrent in every
          file containing clauses for that predicate. The directive
          should precede all clauses of the affected predicates.  This
          directive is defined as a prefix operator in the compiler.".

:- use_module(engine(internals), [
        term_to_meta/2, 
        '$compile_term'/2,'$current_clauses'/2,'$inserta'/2,'$insertz'/2,
        '$ptr_ref'/2,'$current_instance'/5,'$instance'/3,'$erase'/1,
        '$close_predicate'/1, '$open_predicate'/1, '$unlock_predicate'/1]).

:- primitive_meta_predicate(asserta_fact(fact)).
:- primitive_meta_predicate(asserta_fact(fact,-)).
:- primitive_meta_predicate(assertz_fact(fact)).
:- primitive_meta_predicate(assertz_fact(fact,-)).
:- primitive_meta_predicate(current_fact(fact)).
:- primitive_meta_predicate(current_fact(fact,-)).
:- primitive_meta_predicate(retract_fact(fact)).
:- primitive_meta_predicate(retractall_fact(fact)).
:- primitive_meta_predicate(current_fact_nb(fact)).
:- primitive_meta_predicate(retract_fact_nb(fact)).
:- primitive_meta_predicate(open_predicate(fact)).
:- primitive_meta_predicate(close_predicate(fact)).
:- meta_predicate set_fact(fact).

:- doc(asserta_fact(Fact), "@var{Fact} is added to the corresponding
   @concept{data predicate}.  The fact becomes the first clause of the
   predicate concerned.").

:- pred asserta_fact(+callable).

asserta_fact(Fact) :-
	meta_asserta_fact(Fact).

meta_asserta_fact(Fact) :-
        '$compile_term'([Fact|'basiccontrol:true'], Ptr),
	'$current_clauses'(Fact, Root),
	'$inserta'(Root, Ptr).

:- doc(asserta_fact(Fact,Ref), "Same as @pred{asserta_fact/1},
   instantiating @var{Ref} to a unique identifier of the asserted
   fact.").


:- pred asserta_fact(+callable,-reference).

asserta_fact(Fact, Ref) :-
        '$compile_term'([Fact|'basiccontrol:true'], Ptr),
	'$current_clauses'(Fact, Root),
	'$inserta'(Root, Ptr),
        '$ptr_ref'(Ptr, Ref).

:- doc(assertz_fact(Fact), "@var{Fact} is added to the corresponding
   @concept{data predicate}.  The fact becomes the last clause of the
   predicate concerned.").


:- pred assertz_fact(+callable) => callable.

assertz_fact(Fact) :-
        '$compile_term'([Fact|'basiccontrol:true'], Ptr),
	'$current_clauses'(Fact, Root),
	'$insertz'(Root, Ptr).

:- doc(assertz_fact(Fact,Ref), "Same as @pred{assertz_fact/1},
   instantiating @var{Ref} to a unique identifier of the asserted
   fact.").


:- pred assertz_fact(+callable,-reference).

assertz_fact(Fact, Ref) :-
        '$compile_term'([Fact|'basiccontrol:true'], Ptr),
	'$current_clauses'(Fact, Root),
	'$insertz'(Root, Ptr),
        '$ptr_ref'(Ptr, Ref).

:- doc(current_fact(Fact), "Gives on backtracking all the facts
   defined as data or concurrent which unify with @var{Fact}.  It is
   faster than calling the predicate explicitly, which do invoke the
   meta-interpreter.  If the @var{Fact} has been defined as concurrent
   and has not been @concept{closed}, @pred{current_fact/1} will wait
   (instead of failing) for more clauses to appear after the last clause
   of @var{Fact} is returned.").

%% Current fact: if no clause is available, or if it is possible to 
%% determine that no matching exists, , $current_instance leaves
%% the predicate unlocked.  If the predicate is called, then it is left 
%% locked while the clause is being executed.


:- pred current_fact(+callable) => callable.

current_fact(Fact) :-
	'$current_clauses'(Fact, Root),
	'$current_instance'(Fact, ThisIsTrue, Root, _, block), this_is_true(ThisIsTrue),
        '$unlock_predicate'(Root).

:- doc(current_fact_nb(Fact), "Behaves as @pred{current_fact/1} but
   a fact is never waited on even if it is @concept{concurrent} and
   non-closed.").


:- pred current_fact_nb(+callable) => callable.

current_fact_nb(Fact) :-
	'$current_clauses'(Fact, Root),
	'$current_instance'(Fact, ThisIsTrue, Root, _, no_block), this_is_true(ThisIsTrue),
        '$unlock_predicate'(Root).

:- doc(current_fact(Fact,Ref), "@var{Fact} is a fact of a
   @concept{data predicate} and @var{Ref} is its reference identifying
   it uniquely.").


:- pred current_fact(+callable,-reference) # "Gives on backtracking all
   the facts defined as data which unify with @var{Fact}, instantiating
   @var{Ref} to a unique identifier for each fact.".


:- pred current_fact(?callable,+reference) # "Given @var{Ref}, unifies
   @var{Fact} with the fact identified by it.".

current_fact(Fact, Ref) :-
	'$ptr_ref'(Ptr, Ref), !,
	'$instance'(Fact, ThisIsTrue, Ptr), this_is_true(ThisIsTrue).
current_fact(Fact, Ref) :-
	'$current_clauses'(Fact, Root),
	'$current_instance'(Fact, ThisIsTrue, Root, Ptr, no_block), this_is_true(ThisIsTrue),
        '$ptr_ref'(Ptr, Ref),
        '$unlock_predicate'(Root).

% JF,TODO: remove
this_is_true(ThisIsTrue) :- ( ThisIsTrue = true -> true ; ThisIsTrue = 'basiccontrol:true' -> true ; fail ).

:- doc(retract_fact(Fact), "Unifies @var{Fact} with the first
   matching fact of a @concept{data predicate}, and then erases it.  On
   backtracking successively unifies with and erases new matching facts.
   If @var{Fact} is declared as @concept{concurrent} and is
   non-@concept{closed}, @pred{retract_fact/1} will wait for more
   clauses or for the closing of the predicate after the last matching
   clause has been removed.").


:- pred retract_fact(+callable) => callable. 

retract_fact(Fact) :-
	'$current_clauses'(Fact, Root),
        '$current_instance'(Fact, ThisIsTrue, Root, Ptr, block), this_is_true(ThisIsTrue),
	'$erase'(Ptr),
        '$unlock_predicate'(Root).

:- doc(retract_fact_nb(Fact), "Behaves as @pred{retract_fact/1}, but
   never waits on a fact, even if it has been declared as
   @concept{concurrent} and is non-@concept{closed}.").


:- pred retract_fact_nb(+callable) => callable.

retract_fact_nb(Fact) :-
	'$current_clauses'(Fact, Root),
        '$current_instance'(Fact, ThisIsTrue, Root, Ptr, no_block), this_is_true(ThisIsTrue),
	'$erase'(Ptr),
        '$unlock_predicate'(Root).

:- doc(retractall_fact(Fact), "Erase all the facts of a
   @concept{data predicate} unifying with @var{Fact}.  Even if all facts
   are removed, the predicate continues to exist.").


:- pred retractall_fact(+callable) => callable. 

retractall_fact(Fact) :-
	meta_retractall_fact(Fact).

meta_retractall_fact(Fact) :-
	'$current_clauses'(Fact, Root),
        '$current_instance'(Fact, ThisIsTrue, Root, Ptr, no_block), this_is_true(ThisIsTrue),
	'$erase'(Ptr),
        '$unlock_predicate'(Root),
	fail.
meta_retractall_fact(_).

:- doc(close_predicate(Pred), "@cindex{closed} Changes the behavior
   of the predicate @var{Pred} if it has been declared as a
   @concept{concurrent predicate}: calls to this predicate will fail
   (instead of wait) if no more clauses of @var{Pred} are available.").


:- pred close_predicate(+callable) => callable.

close_predicate(Fact):-
        '$current_clauses'(Fact, Root),
        '$close_predicate'(Root).

:- doc(open_predicate(Pred), "Reverts the behavior of
   @concept{concurrent predicate} @var{Pred} to waiting instead of
   failing if no more clauses of @var{Pred} are available.").


:- pred open_predicate(+callable) => callable.

open_predicate(Fact):-
        '$current_clauses'(Fact, Root),
        '$open_predicate'(Root).

:- doc(set_fact(Fact), "Sets @var{Fact} as the unique fact of the
   corresponding @concept{data predicate}.").


:- pred set_fact(+callable) => callable.

set_fact(Fact) :-
        term_to_meta(Fact_t, Fact),
        functor(Fact_t, F, A),
        functor(Template, F, A),
        meta_retractall_fact(Template),
	meta_asserta_fact(Fact_t).

:- doc(erase(Ref), "Deletes the clause referenced by @var{Ref}.").


:- pred erase(+reference) => reference + native.

erase(Ref) :-
	'$ptr_ref'(Ptr, Ref),
	'$erase'(Ptr).

% :- doc(doinclude, fact/1).
% 
% :- true prop fact(F) + regtype
%    # "@var{F} is a fact (an atom or a structure).".
% 
% fact(F) :- callable(F).

:- doc(doinclude, reference/1).
:- export(reference/1).
:- true prop reference(R) + regtype # "@var{R} is a reference of a
dynamic or data clause.".

reference('$ref'(_,_)).
