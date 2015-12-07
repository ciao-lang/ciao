:- module(when, [when/2,wakeup_exp/1],[]).

:- use_package(assertions).
:- use_package(isomodes).

:- doc(title,"Delaying predicates (when)").
:- doc(author, "Manuel Carro").
:- doc(author, "Remy Haemmerle").
:- doc(copyright, "CLIP -- UPM").

:- doc(summary,"This library offers an implementation of
@pred{when/2}.  It is based on the use of attributed variables
@cite{holzbaur-plilp92,holzbaur-phd}.").

:- doc(module,"

@pred{when/2} delays a predicate until some condition in its variable
is met.  For example, we may want to find out the maximum of two
numbers, but we are not sure when they will be instantiated.  We can
write the standard @pred{max/3} predicate (but changing its name to
@pred{gmax/3} to denote that the first and second arguments must be
ground) as

@begin{verbatim}
gmax(X, Y, X):- X > Y, !.
gmax(X, Y, Y):- X =< Y.
@end{verbatim}

and then define a 'safe' @pred{max/3} as 


@begin{verbatim}
max(X, Y, Z):-
        when((ground(X),ground(Y)), gmax(X, Y, Z)).
@end{verbatim}

which can be called as follows:

@begin{verbatim}
?- max(X, Y, Z) , Y = 0, X = 8.

X = 8,
Y = 0,
Z = 8 ? 

yes
@end{verbatim}

Alternatively, @pred{max/3} could have been defined as 

@begin{verbatim}
max(X, Y, Z):-
        when(ground((X, Y)), gmax(X, Y, Z)).
@end{verbatim}

with the same effects as above.  More complex implementations are
possible.  Look, for example, at the @tt{max.pl} implementation under
the @tt{when} library directory, where a @pred{max/3} predicate is
implemented which waits on all the arguments until there is enough
information to determine their values:

@begin{verbatim}
?- use_module(library(when/max)).

yes
?- max(X, Y, Z), Z = 5, Y = 4.

X = 5,
Y = 4,
Z = 5 ? 

yes
@end{verbatim}

").

% Comment to use the new implementation
:- compilation_fact(when__old).

:- if(defined(when__old)).
:- include(library(when/when__old)).
:- else. %% if(defined(when__old)).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Experimental support for multi-attributes. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This module implements an experimental supports of Ciao's multi-
% attributes interface.  By default the multi-attributes support is
% deactivated. Defined the compilation fact when__use_multi_attributes
% to enable it.
%
% Within the module the predicates my_attach_attribute/2,
% my_update_attribute/2, and my_get_attribute/2 are bridge predicates
% to the actual attribute accessors (those defined in
% engine(attributes) for the mono-attribute interface and those
% defined in library(attr/attr_rt) for the multi-attribute one).
% Similarly the predicate my_verify_attribute/2 and
% my_combine_attributes/2 is the maximal common factorizable parts
% between the mono-attribute and multi-attributes interface hooks.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Uncomment to use multi-attributes interface
% :- compilation_fact(when__use_multi_attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% :- use_module(engine(internals)).
%% :- use_module(library(lists), [append/3]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(sets), 
        [
            insert/3, 
            ord_union/3, 
            ord_delete/3, 
            ord_intersection_diff/4
        ]).

:- meta_predicate when(?, goal).

:- pred when(WakeupCond, Goal) : wakeup_exp * callable 
# 
"Delays / executes @var{Goal} according to @var{WakeupCond}
given.  The @var{WakeupCond}s now acceptable are @tt{ground(T)}
(@pred{Goal} is delayed until @tt{T} is ground), @tt{nonvar(T)}
(@pred{Goal} is delayed until @tt{T} is not a variable), and
conjunctions and disjunctions of conditions:

@includedef{wakeup_exp/1}

@pred{when/2} only fails it the @var{WakeupCond} is not legally
formed.  If @var{WakeupCond} is met at the time of the call no delay
mechanism is involved --- but there exists a time penalty in the
condition checking.

In case that an instantiation fires the execution of several
predicates, the order in which these are executed is not defined.".




 %% When/2 must check if the condition is met, and call the Goal
 %% directly in that case.
 %% To Do: reorder condition so that every variable is tested only once.
 %% Will probably wait for a BDD to do that.

when(Condition, Goal):-
        Condition \== true,
        simplify(Condition, Simplified),
        (
            Simplified = true ->
            call(Goal)
        ;
 %% The attribute of a variable is a list of attributed variables;
 %% each of them has an attribute with the real expression and goal.
 %% This final attribute is '$when'(Expression, Goal, VarToLink, ListOfLinked).
 %% VarToLink is the variable of which this structure is attribute
 %% (i.e., the variable which appears in the list of variables), and
 %% ListOfLinked is the list of variables which appear in Expression,
 %% i.e., the list of variables which can possibly trigger the
 %% execution of Goal.
            varset(Simplified, AllVars),
            NewAttribute = '$when'(Simplified, Goal, VarToLink, AllVars),
            my_attach_attribute(VarToLink, NewAttribute),
            add_when_to_var_list(AllVars, VarToLink)
        ).


:- true prop wakeup_exp(T) + regtype
   # "@var{T} is a legal expression for delaying goals.".

wakeup_exp(ground(_)).
wakeup_exp(nonvar(_)).
wakeup_exp((C1, C2)):- wakeup_exp(C1), wakeup_exp(C2).
wakeup_exp((C1; C2)):- wakeup_exp(C1), wakeup_exp(C2).


 %% Add the attributed variable to the list hanging from the var. with delay

add_when_to_var_list([], _VarToLink).
add_when_to_var_list([Var|Vars], VarToLink):-
        (
            my_get_attribute(Var, '$attvarlist'(Var, AttVarSet)) -> 
            insert(AttVarSet, VarToLink, NewAttVarSet),
            my_update_attribute(Var, '$attvarlist'(Var, NewAttVarSet))
        ;
            my_attach_attribute(Var, '$attvarlist'(Var, [VarToLink]))
        ),
        add_when_to_var_list(Vars, VarToLink).


my_verify_attribute(Attr, _Value):-
	Attr = '$attvarlist'(_Var, AttVarSet),
        update_ind_vars(AttVarSet, Goals),
        execute_goals(Goals).



 %% A variable has been modified.  Therefore, several expressions have
 %% changed and we must check them and possibly call several goal.  We
 %% have an invariant to maintain: every expression has associated the
 %% set of vars appearing in it, and there is an (indirect) link from
 %% every of those variables to the expression; this implies adding
 %% and removing links.  When an expression becomes true we can remove
 %% the associated indirect variable.

 %% This is a straightforward algorithm which can be improved.

update_ind_vars([], []).
update_ind_vars([IndVar|IndVars], Goals):-
        my_get_attribute(IndVar, '$when'(Exp, Goal, _, VarList)),
        simplify(Exp, SimpExp),
 %% We could generate NewVarSet from the new Value and the old Exp,
 %% taking into account what has changed, but this is much simpler.
        varset(SimpExp, NewVarSet),
        NewAttribute = '$when'(SimpExp, Goal, IndVar, NewVarSet),
        my_update_attribute(IndVar, NewAttribute),
 %% Find out which variables have been added and which variables have
 %% been removed.  Some variables in VarSet could not be variables any
 %% longer, so the order is not meaningful: they have to be sorted again.
        sort(VarList, VarSet),
        ord_intersection_diff(VarSet, NewVarSet, Inters, ToRemove),
        ord_intersection_diff(NewVarSet, VarSet, Inters, ToAdd),
        remove_indvar(ToRemove, IndVar),
        add_indvar(ToAdd, IndVar),
 %% Call goals whose associated condition became true
        (
            SimpExp = true ->
            Goals = [Goal|RestGoals]
        ;
            Goals = RestGoals
        ),
        update_ind_vars(IndVars, RestGoals).



 %% Remove an indirect variable from the attribute list of a set of
 %% variables.  The point is that some variables of these list may not
 %% be variables any longer.

remove_indvar([], _IndVar).
remove_indvar([V|Vs], IndVar):-
        (
            var(V) ->
            my_get_attribute(V, '$attvarlist'(V, AttVarSet)),
            ord_delete(AttVarSet, IndVar, NewAttVarSet),
            (
                NewAttVarSet = [] ->
                my_detach_attribute(V)
            ;
                my_update_attribute(V, '$attvarlist'(V, NewAttVarSet))
            )
        ;
            true
        ),
        remove_indvar(Vs, IndVar).


add_indvar([], _IndVar).
add_indvar([V|Vs], IndVar):-
        (
            my_get_attribute(V, '$attvarlist'(V, AttVarSet)) ->
            insert(AttVarSet, IndVar, NewAttVarSet),
            my_update_attribute(V, '$attvarlist'(V, NewAttVarSet))
        ;
            my_attach_attribute(V, '$attvarlist'(V, [IndVar]))
        ),
        add_indvar(Vs, IndVar).


execute_goals([]).
execute_goals([G|Gs]):-
        call(G),
        execute_goals(Gs).
            

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Simply add the goals and conditions hanging from the two
 %% variables.  It boils down to the performing the union of the sets
 %% of variables which act as indirect pointers, plus making sure that
 %% the sets of variables associated to every expression is still a set.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_combine_attributes(Attr1, Attr2) :- 
	Attr1 = '$attvarlist'(V1, List1),  Attr2 = '$attvarlist'(_V2, List2),
        ord_union(List1, List2, FinalList),
        my_update_attribute(V1, '$attvarlist'(V1, FinalList)),
 %% We have to check whether there are repeated variables in the back
 %% link list of each attribute.  Think of, for example, 
 %% when((ground(U); nonvar(V)), t1), U = V.
        reduce_to_set(FinalList).


reduce_to_set([]).
reduce_to_set([IndVar|IndVars]):-
        my_get_attribute(IndVar, '$when'(Exp, Goal, _, BackList)),
 %% Do we need to make it here?  Or can we delay until we need it to
 %% be a set?
        sort(BackList, SortedBackList),
        my_update_attribute(IndVar, '$when'(Exp, Goal, IndVar, SortedBackList)),
        reduce_to_set(IndVars).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Simplify conditions.  We *never* return false: a condition can
 %% only be simplified if (parts of) it can be evaluates to true.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify(true, true).
simplify(ground(Term), true):- ground(Term), !.
simplify(ground(Term), NewTerm):-
        varset(Term, AllVars),
        conjunction(AllVars, NewTerm).
 %%
 %% The code below seems to be slower.  Test with more benchmarks.
 %%
 %% simplify(ground(Term), Simplified):-
 %%         varset(Term, AllVars),
 %%         (
 %%             AllVars = [] ->
 %%             Simplified = true
 %%         ;
 %%             conjunction(AllVars, Simplified)
 %%         ).
simplify(nonvar(Term), true):- nonvar(Term), !.
simplify(nonvar(Term), nonvar(Term)).

 %% Evaluate conjunctions and disjunctions.

simplify((C1, C2), Result):-
        simplify(C1, R1), !,
        simplify(C2, R2), !,
        and(R1, R2, Result).

simplify((C1; C2), Result):-
        simplify(C1, R1), !,
        (
            R1 = true ->
            Result = true
        ;
            simplify(C2, R2), !,
            or(R1, R2, Result)
        ).


conjunction([G], ground(G)):- !.
conjunction([V|Vs], (ground(V), Rest)):- 
        conjunction(Vs, Rest).


and(true, true, true):- !.
and(C1, true, C1):- !.
and(true, C2, C2):- !.
and(C1, C2, (C1, C2)):- !.

or(true, true, true):- !.
or(_C1, true, true):- !.
or(true, _C2, true):- !.
or(C1, C2, (C1; C2)):- !.



:- if(defined(when__use_multi_attributes)).

:- use_package(attr).

attr_unify_hook(Attr1, Other):-
	(
	    nonvar(Other) ->
	    my_verify_attribute(Attr1, Other)
	;
	    get_attr_local(Other, Attr2) ->
	    my_combine_attributes(Attr1, Attr2)
	;
	    put_attr_local(Other, Attr1)
	).

my_attach_attribute(V, Attr):-
	put_attr_local(V, Attr).

my_update_attribute(V, Attr):-
	put_attr_local(V, Attr).

my_get_attribute(V, Attr):-
	get_attr_local(V, Attr).

my_detach_attribute(V):-
	 del_attr_local(V).
        

:- else. %% if(defined(when__use_multi_attributes)). 

:- use_module(engine(attributes)).

:- doc(hide,verify_attribute/2).

:- multifile verify_attribute/2.

verify_attribute(Attr, Value):-
	Attr = '$attvarlist'(Var, _AttVarSet),
        detach_attribute(Var),
        Var = Value,
	my_verify_attribute(Attr, Value).

:- doc(hide,combine_attributes/2).

:- multifile combine_attributes/2.
 
combine_attributes(Attr1, Attr2):-
	Attr1 = '$attvarlist'(V1, _List1),  Attr2 = '$attvarlist'(V2, _List2),
        detach_attribute(V1),
        detach_attribute(V2),
        V1 = V2,
	my_combine_attributes(Attr1, Attr2).
 
my_attach_attribute(V, Attr):-
	attach_attribute(V, Attr).

my_update_attribute(V, Attr):-
	update_attribute(V, Attr).

my_get_attribute(V, Attr):-
	get_attribute(V, Attr).

my_detach_attribute(V):-
	detach_attribute(V).

:- endif. %% if(defined(when__use_multi_attributes)).

:- doc(bug, "Redundant conditions are not removed.").
:- doc(bug, "Floundered goals are not appropriately printed.").

:- endif. %% if(defined(when__old)). 
 