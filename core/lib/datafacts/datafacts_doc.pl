:- use_package(assertions).
:- doc(nodoc,assertions).
:- doc(nodoc,assertions_basic).

:- doc(title,"Fast/concurrent update of facts").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").

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
    a @decl{concurrent/1} declaration. The allowed operations on data
    predicates are defined in @lib{datafacts_rt}.
").

