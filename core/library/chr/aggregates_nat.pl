:- module(aggregates_nat, [
        setof/3,
        bagof/3,
        findall/3,
        findall/4,
        findnsols/4,
        findnsols/5,
 	(^)/2
        ],
	[assertions,nortchecks,isomodes,nativeprops,hiord]).

:- use_module(library(sort)).
:- use_module(library(lists), [length/2]).
%:- use_module(engine(hiord_rt),[call/1]).
:- use_module(engine(internals), ['$setarg'/4]).

:- set_prolog_flag(multi_arity_warnings, off).

:- data solution/1.

:- on_abort(retractall_fact(solution(_))).

:- doc(title,"All solutions predicates (No Attributes Version)").

:- doc(module,"@begin{alert}This is a copy of @lib{library(aggregates)} that
   ignores attributes of attributes variables. @end{alert} ").

:- doc(bug, "unify with @lib{library(aggregates)} and dynamic
    predicates and @pred{copy_term/2}.").

:- doc(setof(Template, Generator, Set), "Finds the @var{Set} of
    instances of the @var{Template} satisfying @var{Generator}.  The set
    is in ascending order (see @ref{Comparing terms} for a definition of
    this order) without duplicates, and is non-empty.  If there are no
    solutions, @tt{setof} fails.  @tt{setof} may succeed in more than
    one way, binding free variables in @var{Generator} to different
    values. This can be avoided by using existential quantifiers on the
    free variables in front of @var{Generator}, using @pred{^/2}. For
    example, given the clauses:
@begin{verbatim}
father(bill, tom).
father(bill, ann).
father(bill, john).
father(harry, july).
father(harry, daniel).
@end{verbatim}

The following query produces two  alternative solutions via backtracking:
@begin{verbatim}
?- setof(X,father(F,X),Sons).

F = bill,
Sons = [ann,john,tom] ? ;

F = harry,
Sons = [daniel,july] ? ;

no
?- 
@end{verbatim}").


:- true comp setof(X, Y, Z) + native(findall(X,Y,Z)).

:- pred setof(@term, +callable, ?list) + iso.


:- meta_predicate setof(?,goal,?).

%% This predicate is defined on p51 of the Dec-10 Prolog manual.

setof(Template, Filter, Set) :-
        bagof(Template, Filter, Bag),
        sort(Bag, Set).

:- doc(bagof(Template, Generator, Bag), "Finds all the instances of
   the @var{Template} produced by the @var{Generator}, and returns them
   in the @var{Bag} in the order in which they were found.  If the
   @var{Generator} contains free variables which are not bound in the
   @var{Template}, it assumes that this is like any other Prolog
   question and that you want bindings for those variables.  This can be
   avoided by using existential quantifiers on the free variables in
   front of the @var{Generator}, using @pred{^/2}.").


:- true comp bagof(X, Y, Z) + native(findall(X,Y,Z)).

:- pred bagof(@term, +callable, ?list) + iso.

:- meta_predicate bagof(?,goal,?).

%   bagof records three things under the key '.':
%       the end-of-bag marker          -
%       terms with no free variables   -Term
%       terms with free variables   Key-Term
%   The key '.' was chosen on the grounds that most people are unlikely
%   to realise that you can use it at all, another good key might be ''.
%   The original data base is restored after this call, so that setof
%   and bagof can be nested.  If the Generator smashes the data base
%   you are asking for trouble and will probably get it.
%   The second clause is basically just findall, which of course works in
%   the common case when there are no free variables.

bagof(Template, Generator, Bag) :-
        free_variables(Generator, Template, [], Vars),
        Vars \== [], !,
        (   length(Vars, N),
            N =< 255 ->
              Key =.. [.|Vars]
        ;   Key = .(Vars)
        ),
        save_solutions(Key-Template, Generator),
        list_key_solutions(OmniumGatherum),
        keysort(OmniumGatherum, Gamut), !,
        concordant_subset(Gamut, Key, Answer),
        Bag = Answer.
bagof(Template, Generator, Bag) :-
        findall(Template, Generator, Bag),
        Bag \== [].

:- doc(findall(Template, Generator, List), "A special case of bagof,
     where all free variables in the @var{Generator} are taken to be
     existentially quantified. Faster than the other aggregation
     predicates.").

:- true pred findall(@term, +callable, ?list)
	+ (iso, native, not_fails, is_det).

:- meta_predicate findall(?,goal,?).

%%  It is described in Clocksin & Mellish on p152.  The code they give has
%%  a bug (which the Dec-10 bagof and setof predicates share) which
%%  this has not.

findall(Template, Generator, List) :-
        save_solutions(-Template, Generator),
        list_solutions(List, []).

:- pred findall(@term, +callable, ?term, ?term)
   # "As @pred{findall/3}, but returning in @var{Tail} the tail of
     @var{List} (findall(@var{Template}, @var{Generator}, @var{List}, @var{Tail})).".

:- meta_predicate findall(?,goal,?,?).


findall(Template, Generator, List, Tail) :-
        save_solutions(-Template, Generator),
        list_solutions(List, Tail).

:- doc(findnsols(N,Template,Generator,List),
     "As @pred{findall/3}, but generating at most @var{N} solutions of
     @var{Generator}.  Thus, the length of @var{List} will not be
     greater than @var{N}.  If @var{N}=<0, returns directly an empty
     list.  This predicate is especially useful if @var{Generator} may
     have an infinite number of solutions.").

 :- pred findnsols(+int,@term,+callable,?list).

:- meta_predicate findnsols(?,?,goal,?).

findnsols(N,E,P,L) :-
        N > 0, !,
        NSol=n(0),
        save_n_solutions(NSol,N,-E,P),
        list_solutions(L,[]).
findnsols(_,_,_,[]).

:- doc(findnsols(N,Template,Generator,List,Tail),
     "As @pred{findnsols/4}, but returning in @var{Tail} the tail of
     @var{List}.").

:- pred findnsols(+int,@term,+callable,?,?).

:- meta_predicate findnsols(?,?,goal,?,?).

findnsols(N,E,P,L,T) :-
        N > 0, !,
        NSol=n(0),
        save_n_solutions(NSol,N,-E,P),
        list_solutions(L,T).
findnsols(_,_,_,T,T).

:- meta_predicate save_n_solutions(?,?,?,goal).
save_n_solutions(NSol, N, Template, Generator) :-
        asserta_fact_nat(solution('-')),
        call(Generator),
        asserta_fact_nat(solution(Template)),
        NSol = n(M),
        M1 is M+1,
        '$setarg'(1, NSol,M1,true),
        M1 = N -> fail.
save_n_solutions(_,_,_,_).

:- pred save_solutions(Template, Generator)

   # "Enumerates all provable instances of the @var{Generator} and
     records the associated @var{Template} instances.  Neither
     argument ends up changed.".
:- meta_predicate save_solutions(?,goal).

save_solutions(Template, Generator) :-
        asserta_fact_nat(solution('-')),
	call(Generator),
        asserta_fact_nat(solution(Template)),
        fail.
save_solutions(_,_).

:- pred list_solutions(List, Tail)

   # "Pulls all the @var{Template} instances out of the data base into
      @var{List}".

list_solutions(List, Tail) :-
        retract_fact(solution(Term)), !,
        list_solutions(Term,Tail,List).

list_solutions('-',L,L) :- !.
list_solutions(-Term,Sofar,List) :-
        retract_fact(solution(NewTerm)), !,
        list_solutions(NewTerm, [Term|Sofar],List).

:- pred list_key_solutions(List)

   # "Pulls all the Key-Term instances out of the data base into
     @var{List}".

%%   Note that asserting something into the data base and pulling it out
%%   again renames all the variables; to counteract this we use variables_of
%%   to enforce canonical names on all variables occurring in the Keys.
%%   This replacement must be done _before_ the keysort.
%%   The public domain version instead plugged in the original global variables
%%   whenever a solution didn't bind the corresponding variable.  My solution
%%   takes care of the case when a global variable is bound but not ground.
%%   Alternatively, canonical names could be plugged into entire Keys-Terms.
%%   I'm not sure it's a good idea, so I'm not doing it now.

list_key_solutions(List) :-
        retract_fact(solution(Term)), !,
        list_key_solutions(Term,_KeyVars,[],List).

list_key_solutions('-',_,L,L).
list_key_solutions(Term,KeyVars,SoFar,Total) :-
        Term = Key-_,
        variables_of(Key, KeyVars, KeyVars, _),
        retract_fact(solution(NewTerm)), !,
        list_key_solutions(NewTerm,KeyVars,[Term|SoFar],Total).


variables_of(T, Vars, S0, S) :-
        var(T), !, variable_in_list(T, Vars, S0, S).
variables_of(T, Vars, S0, S) :-
        functor(T, _, N),
        variables_of(N, T, Vars, S0, S).

variables_of(0, _, _, S0, S) :- !, S0=S.
variables_of(N, T, Vars, S0, S) :-
        arg(N, T, A),
        variables_of(A, Vars, S0, S1),
        M is N-1,
        variables_of(M, T, Vars, S1, S).

variable_in_list(T, Vars, S0, S) :-
        Vars==S0, !, S0=[T|S].
variable_in_list(T, [V|Vars], S0, S) :-
        (   T==V -> S0=S
        ;   variable_in_list(T, Vars, S0, S)
        ).

:- pred concordant_subset(Kvpair, Key, Val) : (keylist(Kvpair), list(Val))

   # "Takes a list of @var{Key-Val} pairs which has been keysorted to bring
     all the identical keys together, and enumerates each different
     Key and the corresponding lists of values.".

concordant_subset([Key-Val|Rest], Clavis, Answer) :-
        concordant_subset(Rest, Key, List, More),
        concordant_subset(More, Key, [Val|List], Clavis, Answer).


:-  pred concordant_subset(Rest, Key, List, More)

    # "Strips off all the Key-Val pairs from the from of Rest, putting
      the Val elements into List, and returning the left-over pairs,
      if any, as More.".

concordant_subset([Key-Val|Rest], Clavis, [Val|List], More) :-
        Key == Clavis,
        !,
        concordant_subset(Rest, Clavis, List, More).
concordant_subset(More, _, [], More).


:- pred concordant_subset/5

   # "Tries the current subset, and if that doesn't work if backs up
     and tries the next subset.  The first clause is there to save a
     choice point when this is the last possible subset.".

concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
        concordant_subset(More, Clavis, Answer).



:- pred free_variables(Generator, Template, OldList, NewList)

   # "In order to handle variables properly, we have to find all the
      universally quantified variables in the @var{Generator}.  All
      variables as yet unbound are universally quantified, unless

       a)  they occur in the @var{Template}

       b)  they are bound by X^P, setof, or bagof

    free_variables(Generator, Template, OldList, NewList) finds this
    set, using @var{OldList} as an accumulator.".

free_variables(Term, Bound, VarList, [Term|VarList]) :-
        var(Term),
        term_is_free_of(Bound, Term),
        list_is_free_of(VarList, Term),
        !.
free_variables(Term, _, VarList, VarList) :-
        var(Term),
        !.
free_variables(Term, Bound, OldList, NewList) :-
        explicit_binding(Term, Bound, NewTerm, NewBound),
        !,
        free_variables(NewTerm, NewBound, OldList, NewList).
free_variables(Term, Bound, OldList, NewList) :-
        functor(Term, _, N),
        free_variables(N, Term, Bound, OldList, NewList).

free_variables(0, _, _, VarList, VarList) :- !.
free_variables(N, Term, Bound, OldList, NewList) :-
        arg(N, Term, Argument),
        free_variables(Argument, Bound, OldList, MidList),
        M is N-1,
        free_variables(M, Term, Bound, MidList, NewList).


:- pred explicit_binding/4

   # "Explicit_binding checks for goals known to existentially
      quantify one or more variables.  In particular \+ is quite
      common.".

explicit_binding(X, _, _, _) :- var(X), !, fail. % space saver
explicit_binding('basiccontrol:\\+'(_), Bound,     fail,     Bound    ).
explicit_binding('aggregates_nat:^'(Var,Goal), Bound, Goal,	    Bound+Var).
explicit_binding('aggregates_nat:setof'(Var,Goal,Set), Bound,
                                  Goal-Set, Bound+Var).
explicit_binding('aggregates_nat:bagof'(Var,Goal,Bag), Bound,
                                  Goal-Bag, Bound+Var).
explicit_binding('aggregates_nat:findall'(_,_,Bag), Bound,
                                  Bag,      Bound    ).


term_is_free_of(Term, Var) :-
	var(Term), !,
	Term \== Var.
term_is_free_of(Term, Var) :-
	functor(Term, _, N),
	term_is_free_of(N, Term, Var).

term_is_free_of(0, _, _) :- !.
term_is_free_of(N, Term, Var) :-
	arg(N, Term, Argument),
	term_is_free_of(Argument, Var),
	M is N-1,
	term_is_free_of(M, Term, Var).


list_is_free_of([], _).
list_is_free_of([Head|Tail], Var) :-
	Head \== Var,
	list_is_free_of(Tail, Var).

:- pred '^'(X,P) : (var(X), callable(P)) 
# "Existential quantification: @var{X} is existentially quantified in
   @var{P}. E.g., in @tt{A^p(A,B)}, @tt{A} is existentially
   quantified.  Used only within @concept{aggregation predicates}. In
   all other contexts, simply, execute the procedure call @var{P}.".

:- meta_predicate('^'(?, goal)).
:- true comp (_X^Y) + native(call(Y)).
%% %%% Was as follows when in builtin.pl:
%% (X^Y) :- undefined_goal((X^Y)).
(_X^Y) :- call(Y).

:- use_module(engine(internals), [
        term_to_meta/2, 
        '$compile_term'/2,'$current_clauses'/2,'$inserta'/2]).

:- meta_predicate(asserta_fact_nat(fact)).
asserta_fact_nat(X):-
%	trace(call(asserta_fact(X))),
	copy_term_nat(X, '$:'(X_)),
	meta_asserta_fact_nat(X_), 
%	trace(exit(asserta_fact(X))),
	fail.
asserta_fact_nat(_).
	
meta_asserta_fact_nat(Fact) :-
        '$compile_term'([Fact|'basiccontrol:true'], Ptr),
        '$current_clauses'(Fact, Root),
        '$inserta'(Root, Ptr).


%:- use_module(library(write)).
%trace(X):-
%	write_term(user_error, X, []), 
%	nl(user_error).

