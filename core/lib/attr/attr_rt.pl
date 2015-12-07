:- module(attr_rt, [], [assertions, fsyntax, dcg]).

:- doc(title, "Attributed Variables Runtime").
%:- doc(copyright,"@include{DocCopyright.lpdoc}").

:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "Christian Holzbaur").
:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Carro").

:- doc(summary, "Attributed Variables manipulation predicates").

:- doc(module, "This module provides a means to handle attributed
 variables. Note that attributes are private to the module from which
 the predicates implemented by @lib{attr_rt} module are called.  See
 package @lib{attr} for more details about attributed variables.").

% ---------------------------------------------------------------------------

:- use_module(engine(attributes), [attach_attribute/2, 
                                   get_attribute/2, 
				   detach_attribute/1]).

:- use_module(engine(internals), ['$setarg'/4]).
 
% Uncomment to use C version of the multi-attributes accessors.
% Multi-attributes accessors must be enable in the engine (See
% attr_defs.h)
% :- compilation_fact(use_fast_multiattr).

% NOTE: In this module setarg/4 is used in a safe way.  Indeed, to
% avoid unsafe semantics, we have to ensure that the word modifed by
% setarg/4 is never a variable.
% ---------------------------------------------------------------------------

:- export(attvar/1).
:- pred attvar(Var) # "Succeeds if Term is an attributed variable". 

attvar(Var):-
	attributes:get_attribute(Var, att(_, _, As)), As \== [].

% ---------------------------------------------------------------------------

:- pred put_attr_local(Var, Value) # "If @var{Var} is a variable or
attributed variable, set its attribute to @var{Value}. If an attribute
is already associated with @var{Var}, the old value is
replaced. Backtracking will restore the old value (i.e., an attribute
is a mutable term. See also library @lib{mutables}). This predicate raises a 
representation error if @var{Var} is not a variable and a type error
if @var{Module} is not an atom.".

% (version with implicit module)
:- if(defined(optim_comp)). % TODO: 'addmodule' missing in optim_comp
:- '$context'(put_attr_local/2, module).
:- export(put_attr_local/2).
put_attr_local(Var, Value) :- '$module'(M), put_attr(Var, M, Value).
:- else.
:- export(put_attr_local/2).
:- export(put_attr_local/3).
:- doc(hide, put_attr_local/3).
:- meta_predicate put_attr_local(addmodule, ?).
put_attr_local(Var, M, Value) :- put_attr(Var, M, Value).
:- endif.

:- doc(hide, put_attr_local/3).

:- export(put_attr/3).

:- if(defined(use_fast_multiattr)).
:- impl_defined(put_attr/3).
:- else.
put_attr(_Var, Module, _Value) :-
	var(Module), 
	throw(error(instantiation_error, put_attr/3-2)).
put_attr(Var, user(_), Value):-!,
	put_attr(Var, user, Value).
put_attr(Var, Module, Value):-
	(
	    nonvar(Var) -> 
	    throw(error(uninstantiation_error(Var), put_attr/3-1))
	;
	    \+ atom(Module) ->
	    throw(error(type_error(atom, Module), put_attr/3-2))
	;
	    attributes:get_attribute(Var, As) ->
	    % Has already an attrbuted
	    As = att(_, _, Next),
	    internal_insert_attr(Next, As, Module, Value)
	;
	    % Hos no attributes
	    attributes:attach_attribute(Var, att(Var, false, att(Module, Value, [])))
	).

internal_insert_attr([], As, Module, Value):- !,
	'$setarg'(3, As, att(Module, Value, []), on). 
internal_insert_attr(Next, As, Module, Value):-
	Next = att(M, _, NextNext),
	(
	    Module @> M ->
	    internal_insert_attr(NextNext, Next, Module, Value) 
	;
	    M == Module -> 
	    %% Note: setarg(2, Next, Value, on) is not _safe_ 
            %% since Value can be variable.
            %% Then replace the all cell.
	    '$setarg'(3, As, att(M, Value, NextNext), on)  % backtrackable
	;
            %% insert a cell
	    '$setarg'(3, As, att(Module, Value, Next), on)  % backtrackable
	).
:-endif.	

% ---------------------------------------------------------------------------

:- pred get_attr_local(Var, Value) # "Request the current value for
the attribute associted to @var{Var}. If @var{Var} is not an attributed
variable or the named attribute is not associated to @var{Var} this
predicate fails silently.".

% (version with implicit module)
:- if(defined(optim_comp)). % TODO: 'addmodule' missing in optim_comp
:- '$context'(get_attr_local/2, module).
:- export(get_attr_local/2).
get_attr_local(Var, Value) :- '$module'(M), get_attr(Var, M, Value).
:- else.
:- export(get_attr_local/2).
:- export(get_attr_local/3).
:- doc(hide, get_attr_local/3).
:- meta_predicate get_attr_local(addmodule, ?).
get_attr_local(Var, Module, Value) :- get_attr(Var, Module, Value).
:- endif.

:- export(get_attr/3).

:- if(defined(use_fast_multiattr)).
:- impl_defined(get_attr/3).
:- else.
%:- doc(hide, get_attr/3).
get_attr(_, Module, _) :-
	var(Module), 
	throw(error(instantiation_error, get_attr/3-2)).
get_attr(Var, user(_), Value):-!,
	get_attr(Var, user, Value).
get_attr(Var, Module, Value) :-
	(
	    \+ atom(Module) ->
	    throw(error(type_error(atom, Module), get_attr/3-2))
	;
	    attributes:get_attribute(Var, Attr), 
	    Attr = att(_, _, As),
	    internal_get_attr(As, Module, Value)
	).
	
internal_get_attr(att(M, V, Next), Module, Value):-
	(
	    Module @> M -> 
	    internal_get_attr(Next, Module, Value)
	;
	    M == Module,
	    V = Value 
	).
:-endif.

% ---------------------------------------------------------------------------

:- pred del_attr_local(Var) # "If @var{Var} has an attribute, deletes it,
    otherwise succeeds without side-effect.".

% (version with implicit module)
:- if(defined(optim_comp)). % TODO: 'addmodule' missing in optim_comp
:- '$context'(del_attr_local/1, module).
:- export(del_attr_local/1).
del_attr_local(Var) :- '$module'(M), del_attr(Var, M).
:- else.
:- export(del_attr_local/1).
:- export(del_attr_local/2).
:- doc(hide, del_attr_local/2).
:- meta_predicate del_attr_local(addmodule).
del_attr_local(Var, Module) :- del_attr(Var, Module).
:- endif.

:- export(del_attr/2).
%:- doc(hide, del_attr/2).

:- if(defined(use_fast_multiattr)).
:- impl_defined(del_attr/2).
:- else.
del_attr(_Var, Module):-
	var(Module), 
	throw(error(instantiation_error, del_attr/2-2)).
del_attr(Var, user(_)):-!,
	del_attr(Var, user).
del_attr(Var, Module):-
	(
	    nonvar(Var) ->
	    throw(error(uninstantiation_error(Var), del_attr/3-1))
	;
	    \+ atom(Module) ->
	    throw(error(type_error(atom, Module), del_attr/3-2))
	;
	    attributes:get_attribute(Var, As),
	    As = att(_, _, Next), 
	    internal_del_attr(Next, As, Module), !
	;
	    true
	).

internal_del_attr(Next, As, Module):-
	Next = att(M, _, NextNext),
	(
	    Module @> M -> 
	    internal_del_attr(NextNext, Next, Module)
	;
	    M == Module ->
	    '$setarg'(3, As, NextNext, on)  % backtrackable 
	;
	    true
	).
:- endif.

% ---------------------------------------------------------------------------

% :- export(type_attr_local/2).
% :- export(type_attr_local/3).
% :- doc(hide, type_attr_local/2).
% :- doc(hide, type_attr_local/3).

% :- meta_predicate type_attr_local(addmodule, ?).
% type_attr_local(Var, Mod, Type) :- type_attr(Var, Mod, Type).

% :- doc(hide, type_attr/3).
% :- export(type_attr/3).

% :- if(defined(use_fast_multiattr)).
% :- impl_defined(type_attr/3).
% :- else.
% type_attr(_, Mod, _) :-
% 	var(Mod), 
% 	throw(error(instantiation_error, type_attr/3-2)).
% type_attr(Var, user(_), Value):-!,
% 	type_attr(Var, user, Value).
% type_attr(X, Mod, T):-
% 	type(X, T_),
% 	(
% 	    T_ = attv ->
% 	    (
% 		get_attr(X, Mod, _) ->
% 		T = attv 
% 	    ;
% 		T = var
% 	    )
% 	;
% 	    T = T_
% 	).
% :- endif.

% ---------------------------------------------------------------------------

:- export(get_attrs/2).
:- doc(hide, get_attrs/2).
:- pred get_attrs(Var, Attributes) # "Get all attributes of
@var{Var}. Attributes is a term of the form @tt{att(Module, Value,
MoreAttributes)}, where @var{MoreAttributes} is for the next attribute.".

get_attrs(X, As):-
	attributes:get_attribute(X, att(_, _, As)).

% ---------------------------------------------------------------------------

:- export(put_attrs/2).
:- doc(hide, put_attrs/2).
:- pred put_attrs(Var, Attributes) # "Set all attributes of
@var{Var}. See @tt{get_attrs/2} for a description of
@var{Attributes}.".

put_attrs(X, As):-
	(
	    attributes:get_attribute(X, As_) ->
	    As_ = att(_, _, _), 
	    '$setarg'(3, As, As, on)  % Backtrackable 
	;
	    attributes:attach_attribute(X, att(X, false, As))
	).

% ---------------------------------------------------------------------------

:- export(del_attrs/1).
:- doc(hide, del_attrs/1).
:- pred del_attrs(Var) # "If Var is an attributed variable, delete all
its attributes. In all other cases, this predicate succeeds without
side-effects.".

del_attrs(Var):-
	(
	    attributes:get_attribute(Var, As_),
	    As_ = att(_, _, As), 
	    As \== [] ->
	    '$setarg'(3, As_, [], on)  % Backtrackable 
	;
	    true
	).

% ---------------------------------------------------------------------------

%%% term_attrs/2 %%% 

:- export(attvarset/2).
:- pred attvarset(X, Vars) # 

"@var{AttVars} is a list of all attributed variables in Term and its
attributes. I.e., @tt{attvarset/2} works recursively through
attributes. This predicate is Cycle-safe. The goal
term_attvars(Term,[]) is optimized to be an efficient test that
@var{Term} has no attributes. I.e., scanning the term is aborted after
the first attributed variable is found. ".


attvarset(X, Vars):- 
	(
	    Vars == [] ->
	    attvarset_(X, [], [])        % Optimized call in case of nil list
	;
	    attvarset_(X, Vars_, []),    % Call attvarset_/3 with a fresh 2nd arg
	    untag(Vars_),                % Untag attributed variables 
	    Vars = Vars_                 % 
	).

% Important. The calls to attvaset_/3 should respect the following
% convention: Vars (2nd argument) should be either:
%   - a free variable 
%   - or a nil list 
attvarset_(X, Vars, Tail) :-
        var(X), !,                           % X is var
	(
	    attributes:get_attribute(X, A),  % X is attr variable
            A = att(_, false, As) ->         % X is untagged 
	    Vars = [X|Tail0],                % Vars is not nil 
	    % tag variable
	    '$setarg'(2, A, true, true),     % then tag X (unbacktrble setarg)
	    attvarset_(As, Tail0, Tail)      % recursive call on attributes
	;
	    Vars = Tail
	).
attvarset_([H|T], Vars, Tail) :- !,
        attvarset_(H, Vars,  Tail0),
        attvarset_(T, Tail0, Tail).
attvarset_(Term, Vars, Tail) :-
        functor(Term, _, A),
        go_inside(A, Term, Vars, Tail).

go_inside(0, _, Tail, Tail) :- !.
go_inside(N, T, Bag,  Tail) :-
        Nth is N-1,
        arg(N, T, ARG),
        attvarset_(ARG, Bag, Tail0),
        go_inside(Nth, T, Tail0, Tail).


untag([]).
untag([H|T]):-
	attributes:get_attribute(H, A),	
	'$setarg'(2, A, false, true),        % then tag X (unbacktrble setarg)
	untag(T).

% ---------------------------------------------------------------------------
%%% copy_term/3 %%% 

:- export(copy_term/3).
:- pred	copy_term(Term, Copy, Gs)  # 

"Creates a regular term @var{Copy} as a copy of @var{Term} (without any
attributes), and a list @var{Gs} of goals that when executed reinstate all
attributes onto Copy. The nonterminal @tt{attribute_goal/1}, as defined
in the modules the attributes stem from, is used to convert attributes
to lists of goals.".

copy_term(Term, Copy, Gs) :-
	attvarset(Term, Vs),
	(   
	    Vs == []
	->  
	    Gs = [],
	    copy_term(Term, Copy)
        ; 
	    attvars_residuals(Vs, Gs_, []),
	    delete_attributes(Vs), 
	    assertz_fact('copy of'(Term-Gs_)), 
	    fail
	;
	    retract_fact('copy of'(Copy-Gs))
	).

:- data 'copy of'/1.

% :- export(copy_term_nat/2).
% :- pred copy_term_nat(Term, Copy) # 
	
%"@var{Copy} is a renaming ot @var{Term} such that brand new variables
%have been substituted for all variables in @var{Term} (as
%@tt{term_basic:copy_term/2}). However if any of the variables of @var{Term} have
%attributes, the copied variables will be replaced by an fresh variables.".

% copy_term_nat(Term, _Copy):-
% 	attvarset(Term, Vs),
% 	delete_attributes(Vs), 
% 	assertz_fact('copy of'(Term)), 
% 	fail.
% copy_term_nat(_Term, Copy):-
% 	retract_fact('copy of'(Copy)).

% ---------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attributed variable hooks %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Unify Hooks %%%
	
:- doc(hide, 'attr_rt:unify_hook'/3). 		     
:- multifile 'attr_rt:unify_hook'/3.

:- doc(hide, '$verify_attributes_loop'/2).
%:- export('$verify_attributes_loop'/2).
% (invoked from engine/attributed_variables.pl)
:- multifile '$verify_attributes_loop'/2.
	
'$verify_attributes_loop'(att(Module, Value, Next), Other):-
	'attr_rt:unify_hook'(Module, Value, Other), 
	'$verify_attributes_loop'(Next, Other).
'$verify_attributes_loop'([], _).

%%% Portray Hooks %%% 

:- use_module(library(format), [format/2]).

:- doc(hide, portray_attribute/2).
:- doc(hide, 'attr_rt:portray_hook'/3).

:- multifile portray_attribute/2.
:- multifile 'attr_rt:portray_hook'/3.

portray_attribute(att(_, _, As), Var):-
	format("{", []),
	portray_attrs(As, Var),
	format("}", []).

portray_attrs([], _).
portray_attrs(att(Name, Value, Rest), Var) :-
	portray_attr(Name, Value, Var),
	(   
	    Rest == [] -> 
	    true
	;   format(", ", []),
	    portray_attrs(Rest, Var)
	).

portray_attr(Name, Value, Var) :-
	(
	    'attr_rt:portray_hook'(Name, Value, Var)
	->
	    true
	;   
	    format("~w = ...", [Name])
	).

% ---------------------------------------------------------------------------

%%% Residual Goals Hooks %%% 

:- doc(hide, 'attr_rt:attribute_goals'/4).
:- multifile 'attr_rt:attribute_goals'/4.

:- export(attvars_residuals/3).
:- doc(hide, attvars_residuals/3).

attvars_residuals([]) --> [].
attvars_residuals([V|Vs]) -->
	(   { get_attrs(V, As) }
	->  attvar_residuals(As, V)
	;   []
	),
	attvars_residuals(Vs).

attvar_residuals([], _) --> [].
attvar_residuals(att(Module,Value,As), V) -->
	(   
	    { nonvar(V) } ->
	    % a previous projection predicate could have instantiated
	    % this variable, for example, to avoid redundant goal
	    []
	;   
	    'attr_rt:attribute_goals'(Module, V) ->
	    []
	;
	    [~term_to_goal(put_attr(V, Module, Value))]
	),
	attvar_residuals(As, V).

delete_attributes([]).
delete_attributes([V|Vs]) :-
	detach_attribute(V),
	delete_attributes(Vs).

:- meta_predicate term_to_goal(goal, ?).
term_to_goal(X,X).

% ---------------------------------------------------------------------------

%%% Dump contsraints hook %%% 

:- doc(hide, 'attr_rt:dump_constraints'/5).
:- multifile 'attr_rt:dump_constraints'/5.

:- doc(hide,  dump_constraints/3).
:- multifile dump_constraints/3. 

dump_constraints(Term, Copy, Constraints):-
	attvarset(Term, List), 
	attmod_set(List, [], Atts),
	dump_constraints_loop(Atts, Term, Copy, _, Constraints, []).

:- pred dump_constraints_loop(+Modules, +Term, -Copy, -Flag, Cs, Cs0) :
	list(atom(Modules)).

% Flag (the 4th argument) is instantiated if one of the
% 'attr_rt:dump_constraints' succeeds.  Otherwise we make the global
% dump_constraints fails. That gives a change to a mono-attributes
% version of the hook to succeeds.
dump_constraints_loop([], _Term, _Copy, Flag) --> 
	{ nonvar(Flag) }.
dump_constraints_loop([H|T], Term, Copy, Flag) -->
	('attr_rt:dump_constraints'(H, Term, Copy) -> {Flag=true} ; []),
	dump_constraints_loop(T, Term, Copy, Flag).

:- use_module(library(sets), [merge/3]).

:- pred attmod_set(+AttVars, +Acc, -Attrs) : (list(AttVars), list(Acc),
list(Attr)) # "unifies @var{Attrs} with the ordered list of all modules
in which the attributed variables of @var{AttVars} have an attribute
plus the module occuring in @var{Acc}.  @var{Acc} is assume to be
ordered according @tt{@>/2}.". 

% Note: the predicate has been programme without particular care of
% complexity.
attmod_set([], Attrs, Attrs).
attmod_set([H|T], Acc, Attrs):-
	attributes:get_attribute(H, att(_, _, As)), 
	att_str_to_list(As, HAttrs),
	merge(HAttrs, Acc, Acc_), 
	attmod_set(T, Acc_, Attrs).

att_str_to_list([], []).
att_str_to_list(att(K, _, As), [K|T]):-
	att_str_to_list(As, T).


