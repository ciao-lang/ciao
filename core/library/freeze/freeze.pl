:- module(freeze, [freeze/2, frozen/2], [assertions, nortchecks]).

:- doc(title,"Delaying predicates (freeze)").
:- doc(author,"Remy Haemmerle").
:- doc(author,"Manuel Carro").
:- doc(author,"Daniel Cabeza").

:- doc(module,"This library offers a simple implementation of
   @pred{freeze/2}, @pred{frozen/2},
   etc. @cite{Prologii,MU-Prolog,naish:nu-prolog,Carlsson} based on
   the use of attributed variables
   @cite{holzbaur-plilp92,holzbaur-phd}.").

% Comment to use the new implementation
:- compilation_fact(freeze__old).

:- if(defined(freeze__old)).
:- include(library(freeze/freeze__old)).
:- else. %% if(defined(freeze__old)).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Experimental support for multi-attributes. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This module implements an experimental supports of Ciao's multi-
% attributes interface.  By default the multi-attributes support is
% deactivated. Defined the compilation fact freeze__use_multi_attributes
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
% :- compilation_fact(freeze__use_multi_attributes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementation based on Holzbauer's examples
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(engine(internals)).

%% :- ensure_loaded(library(attrdecl)).

:- pred freeze(X, Goal) : callable(Goal) 

# "If @var{X} is free delay @var{Goal} until @var{X} is
   non-variable.".

:- meta_predicate freeze(?, goal).
:- meta_predicate frozen(?, goal).

freeze(X, Goal) :-
        (
            var(X) -> 
            my_attach_attribute( V, '$frozen_goals'(V,Goal)),
            X = V
        ;
            call(Goal)
        ).

my_verify_attribute('$frozen_goals'(_Var, Goal), _Value):-
	call(Goal).

my_combine_attributes('$frozen_goals'(V1, G1), '$frozen_goals'(_V2, G2)):-
	term_to_meta(T1, G1),
        term_to_meta(T2, G2),
        term_to_meta((T1,T2), G),
        my_update_attribute(V1, '$frozen_goals'(V1, G)).

:- pred frozen(X, Goal) => callable(Goal) # "@var{Goal} is currently delayed
   until variable @var{X} becomes bound.".

frozen(Var, Goal):-
	var(Var), 
        (
	    my_get_attribute(Var, '$frozen_goals'(_, Goal)), !
	;
	    Goal = true
	).

:- if(defined(freeze__use_multi_attributes)).

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

:- else.

:- use_module(engine(attributes)).
 
:- doc(hide, verify_attribute/2).

:- multifile verify_attribute/2.

verify_attribute(Attr, Value):-
	Attr = '$frozen_goals'(Var, _Goal),
        detach_attribute(Var),
        Var = Value, 
        my_verify_attribute(Attr, Value).

:- doc(hide,combine_attributes/2).

:- multifile combine_attributes/2.

combine_attributes(Attr1, Attr2):-
	Attr1 = '$frozen_goals'(V1, _G1), 
	Attr2 = '$frozen_goals'(V2, _G2),
        detach_attribute(V2),
        V1 = V2,
	my_combine_attributes(Attr1, Attr2).

my_attach_attribute(V, Attr):-
	attach_attribute(V, Attr).

my_update_attribute(V, Attr):-
	update_attribute(V, Attr).

my_get_attribute(V, Attr):-
	get_attribute(V, Attr).

:- endif. %% if(defined(freeze__use_multi_attributes)).

:- endif. %% if(defined(freeze__old)). 