% :- module(freeze, [freeze/2, frozen/2], [assertions, nortchecks]).

% :- doc(title,"Delaying predicates (freeze)").
% :- doc(author,"Remy Haemmerle").
% :- doc(author,"Manuel Carro").
% :- doc(author,"Daniel Cabeza").

% :- doc(module,"This library offers a simple implementation of
%    @pred{freeze/2}, @pred{frozen/2},
%    etc. @cite{Prologii,MU-Prolog,naish:nu-prolog,Carlsson} based on
%    the use of attributed variables
%    @cite{holzbaur-plilp92,holzbaur-phd}.").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementation based on Holzbauer's examples
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(engine(internals)).
:- use_module(engine(attributes)).

%% :- ensure_loaded(library(attrdecl)).

:- pred freeze(X, Goal) : callable(Goal) 

# "If @var{X} is free delay @var{Goal} until @var{X} is
   non-variable.".

:- meta_predicate freeze(?, goal).
:- meta_predicate frozen(?, goal).

freeze(X, Goal) :-
        (
            var(X) -> 
            attach_attribute( V, '$frozen_goals'(V,Goal)),
            X = V
        ;
            call(Goal)
        ).

:- doc(hide,verify_attribute/2).

:- multifile verify_attribute/2.

verify_attribute('$frozen_goals'(Var, Goal), Value):-
        detach_attribute(Var),
        Var = Value, 
        call(Goal).

:- doc(hide,combine_attributes/2).

:- multifile combine_attributes/2.

combine_attributes('$frozen_goals'(V1, G1), '$frozen_goals'(V2, G2)):-
        detach_attribute(V1),
        detach_attribute(V2),
        V1 = V2,
        term_to_meta(T1,G1),
        term_to_meta(T2,G2),
        term_to_meta((T1,T2),G),
        attach_attribute(V1, '$frozen_goals'(V1, G)).


:- pred frozen(X, Goal) => callable(Goal) # "@var{Goal} is currently delayed
   until variable @var{X} becomes bound.".

frozen(Var, Goal):-
	var(Var), 
        (
	    get_attribute(Var, '$frozen_goals'(_, Goal)), !
	;
	    Goal = true
	).
