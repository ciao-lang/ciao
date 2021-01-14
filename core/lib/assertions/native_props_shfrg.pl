% (included file)
:- doc(section, "Sharing/aliasing, groundness").

:- export(mshare/1). % TODO: Read as possibly_share
:- doc(mshare(X), "@var{X} contains all @index{sharing sets}
   @cite{jacobs88,abs-int-naclp89} which specify the possible variable
   occurrences in the terms to which the variables involved in the
   clause may be bound. Sharing sets are a compact way of representing
   groundness of variables and dependencies between variables. This
   representation is however generally difficult to read for
   humans. For this reason, this information is often translated to
   @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties,
   which are easier to read.").

:- prop mshare(X) + (native(sharing(X)), no_rtcheck)
   # "The sharing pattern for the variables in the clause is @tt{@var{X}}.".

% TODO: describe valid mshare like in constraint/1 (e.g. list of lists)?
:- if(defined(optim_comp)).
:- '$props'(mshare/1, [impnat=indefinable]).
:- else.
:- impl_defined(mshare/1).
:- endif.

% Note: obsolete property, assertion normalizer expands to mshare/2
% internally. Kept only for backwards compatibility. % TODO: expand to
% mshare/2 also in program-point assertions


:- export(mshare/2).

:- prop mshare(Xs,Xss) + (native(sharing(Xs,Xss)), no_rtcheck)
# "The sharing pattern for the variables @tt{@var{Xs}} is @tt{@var{Xss}}.".

:- if(defined(optim_comp)).
:- '$props'(mshare/2, [impnat=indefinable]).
:- else.
:- impl_defined(mshare/2).
:- endif.

% --------------------------------------------------------------------------
% Amadeo
:- export(indep/2).
:- prop indep(X, Y) + native(indep([[X, Y]]))
   # "@var{X} and @var{Y} do not have variables in common.".

indep(A, B) :-
    mark(A, Ground), % Ground is var if A ground
    nonvar(Ground), % If 1st argument was ground, no need to proceed
    marked(B), !,
    fail.
indep(_, _).

mark('$$Mark', no) :- !. % Mark the variable, signal variable found
mark(Atom,     _) :- atomic(Atom), !.
mark(Complex,  GR) :- mark_(Complex, 1, GR).

mark_(Args, Mth, GR) :-
    arg(Mth, Args, ThisArg), !,
    mark(ThisArg, GR),
    Nth is Mth+1,
    mark_(Args, Nth, GR).
mark_(_, _, _).

marked(Term) :-
    functor(Term, F, A),
    ( A > 0, !, marked_(Term, 1)
    ; F = '$$Mark' ).

marked_(Args, Mth) :-
    arg(Mth, Args, ThisArg), !,
    ( marked(ThisArg) % TODO: missing cut?
    ; Nth is Mth+1,
      marked_(Args, Nth)
    ).

% --------------------------------------------------------------------------
% Amadeo
:- export(indep/1).
:- prop indep(X) + native(indep(X))
   # "The variables in the the pairs in @tt{@var{X}} are pairwise
   independent.".

indep([]).
indep([[X, Y]|L]) :- indep(X, Y), indep(L).

% --------------------------------------------------------------------------
% TODO: Duplicated name w.r.t. covered/1 in nonfailure.
:- export(covered/2).
:- doc(covered(X, Y), "All variables occuring in @var{X} occur also
   in @var{Y}. Used by the non-strict independence-based annotators.").

:- prop covered(X, Y) + native # "@var{X} is covered by @var{Y}.".

:- if(defined(optim_comp)).
:- '$props'(covered/2, [impnat=indefinable]).
:- else.
:- impl_defined(covered/2).
:- endif.

% --------------------------------------------------------------------------
:- export(linear/1).
:- doc(linear(X), "@var{X} is bound to a term which is linear,
   i.e., if it contains any variables, such variables appear only once
   in the term. For example, @tt{[1,2,3]} and @tt{f(A,B)} are linear
   terms, while @tt{f(A,A)} is not.").

:- prop linear(X) + native
   # "@var{X} is instantiated to a linear term.".

:- if(defined(optim_comp)).
:- '$props'(linear/1, [impnat=indefinable]).
:- else.
:- impl_defined(linear/1).
:- endif.

% --------------------------------------------------------------------------
:- export(nonground/1).
:- prop nonground(X) + native(not_ground(X))
   # "@tt{@var{X}} is not ground.".

:- if(defined(optim_comp)).
:- '$props'(nonground/1, [impnat=indefinable]).
:- else.
% TODO: use rtc_impl!
nonground(X) :- \+ ground(X).
:- endif.

% --------------------------------------------------------------------------
% Jorge 
:- export(clique/1).
:- doc(clique(X), "@var{X} is a set of variables of interest, much the
   same as a sharing group but @var{X} represents all the sharing groups in
   the powerset of those variables. Similar to a sharing group, a clique is
   often translated to @prop{ground/1}, @prop{indep/1}, and @prop{indep/2}
   properties.").

:- prop clique(X) + (native(clique(X)), no_rtcheck)
   # "The clique sharing pattern is @tt{@var{X}}.".

:- if(defined(optim_comp)).
:- '$props'(clique/1, [impnat=indefinable]).
:- else.
:- impl_defined(clique/1).
:- endif.

% --------------------------------------------------------------------------
% Jorge 
:- export(clique_1/1).
:- doc(clique_1(X), "@var{X} is a set of variables of interest, much
   the same as a sharing group but @var{X} represents all the sharing
   groups in the powerset of those variables but disregarding the
   singletons. Similar to a sharing group, a clique_1 is often translated
   to @prop{ground/1}, @prop{indep/1}, and @prop{indep/2} properties.").

:- prop clique_1(X) + (native(clique_1(X)), no_rtcheck)
   # "The 1-clique sharing pattern is @tt{@var{X}}.".

:- if(defined(optim_comp)).
:- '$props'(clique_1/1, [impnat=indefinable]).
:- else.
:- impl_defined(clique_1/1).
:- endif.
