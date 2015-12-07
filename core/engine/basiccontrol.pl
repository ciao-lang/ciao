:- module(basiccontrol, [
        ','/2, ';'/2, '->'/2, !/0,
	% '^'/2, %% Moved to aggregates M.H.
	(\+)/1, if/3,
        true/0, % This cannot change
        fail/0, repeat/0,
	false/0, otherwise/0,
	'$metachoice'/1, '$metacut'/1, 
	interpret_goal/2,
	interpret_compiled_goal/2,
	undefined_goal/1,
	debug_goal/1
	],
        [assertions, nortchecks, isomodes, nativeprops]).

:- doc(title,"Control constructs/predicates").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(module,"This module contains the set of basic control
   predicates, except the predicates dealing with exceptions, which are
   in @ref{Exception and Signal handling}.").

:- doc(usage, "These predicates/constructs are builtin in Ciao, so
   nothing special has to be done to use them.  In fact, as they are
   hardwired in some parts of the system, most of them cannot be redefined.").

:- use_module(engine(internals), ['$unknown'/2, '$current_instance'/5, '$unlock_predicate'/1]).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(library(debugger), [debug_trace/1]).

% Compiled inline -- these are hooks for the interpreter.

:- doc(','(P,Q), "Conjunction (@var{P} @em{and} @var{Q}).").

:- trust pred ','(+callable,+callable) + iso.

:- primitive_meta_predicate(','(goal, goal)).
(X, Y) :- undefined_goal((X, Y)).
%(X, Y) :- '$meta_call'(X), '$meta_call'(Y).

:- doc(';'(P,Q), "Disjunction (@var{P} @em{or} @var{Q}).  Note that
   in Ciao @pred{|/2} is not equivalent to @tt{;/2}.").


:- trust pred ';'(+callable,+callable) + iso.

:- primitive_meta_predicate(';'(goal, goal)).
(X;Y) :- undefined_goal((X;Y)).
%(X;_) :- '$meta_call'(X).
%(_;Y) :- '$meta_call'(Y).

:- doc('->'(P,Q), "If @var{P} then @var{Q} else fail, using first
   solution of @var{P} only.  Also, @tt{(}@var{P} @tt{->} @var{Q}
   @tt{;} @var{R}@tt{)}, if @var{P} then @var{Q} else @var{R}, using
   first solution of @var{P} only.  No cuts are allowed in @var{P}.").


:- trust pred '->'(+callable,+callable) + iso.

:- primitive_meta_predicate('->'(goal, goal)).
(X->Y) :- undefined_goal((X->Y)).

:- trust pred '!'/0 + ( iso, is_det, not_fails, relations(1) )
   # "Commit to any choices taken in the current predicate.". 

!.						% simple enough

%% Moved to aggregates. MH
%% (X^Y) :- undefined_goal((X^Y)).

:- doc(\+(P), "Goal @var{P} is not provable (negation by failure).
   Fails if @var{P} has a solution, and succeeds otherwise.  No cuts are
   allowed in @var{P}.").


:- trust pred \+(+callable) + ( iso, native(not(X)), is_det ).

:- primitive_meta_predicate(\+(goal)).
\+X :- undefined_goal(\+X).

:- doc(if(P,Q,R), "If @var{P} then @var{Q} else @var{R}, exploring
   all solutions of @var{P}.  No cuts are allowed in @var{P}."). 


:- trust pred if(+A,+B,+C) 
	: (callable(A), callable(B), callable(C))
        => (callable(A), callable(B), callable(C)).

:- primitive_meta_predicate(if(goal, goal, goal)).
if(P, Q, R) :- undefined_goal(if(P,Q,R)).

:- trust pred true/0 + ( iso, native, sideff(free),
	is_det, not_fails, relations(1) ) # "Succeed (noop).".
:- true comp true/0 + eval.
:- impl_defined(true/0).

:- trust pred otherwise/0.
:- true comp otherwise/0 + ( sideff(free), eval ).
:- impl_defined(otherwise/0).

:- trust pred fail/0 + ( iso, native, sideff(free),
	is_det, fails, relations(0) ) # "Fail, backtrack immediately.".
:- true comp fail/0 + eval.
:- true comp fail/0 + equiv(fail).
:- impl_defined(fail/0).

:- trust pred false/0.
:- true comp false/0 + ( sideff(free), eval ).
:- impl_defined(false/0).

:- trust pred repeat/0 + ( iso, native, sideff(free) ) # "Generates an infinite sequence of
   backtracking choices.".
:- impl_defined(repeat/0).


:- trust pred '$metachoice'(X) => int(X)  +  native.
:- impl_defined('$metachoice'/1).
:- doc(hide,'$metachoice'/1).


:- trust pred '$metacut'(X): int(X) => int(X) + native.
:- impl_defined('$metacut'/1).
:- doc(hide,'$metacut'/1).

%------ interpreter ------%
:- doc(hide,interpret_goal/2).
:- doc(hide,interpret_compiled_goal/2).
:- doc(hide,undefined_goal/1).
:- doc(hide,debug_goal/1).

% called from within the emulator

interpret_goal(Head, Root) :-
	'CHOICE IDIOM'(Cut),
	'$current_instance'(Head, Body, Root, _, no_block),
        '$unlock_predicate'(Root),
	metacall(Body, Cut, interpret).

interpret_compiled_goal(Head, _) :- debug_trace(Head).

undefined_goal(X) :- 'CHOICE IDIOM'(Cut), metacall(X, Cut, undefined).

debug_goal(X) :- 'CHOICE IDIOM'(Cut), metacall(X, Cut, debug).

metacall(X, _, _) :-
	var(X), !,
        throw(error(instantiation_error, call/1-1)).
metacall('true', _, _) :- !.
metacall('basiccontrol:true', _, _) :- !.
metacall('basiccontrol:otherwise', _, _) :- !.
metacall('basiccontrol:false', _, _) :- !, fail.
metacall('fail', _, _) :- !, fail.
metacall('basiccontrol:!', ?, _) :- !,
        message(warning, '! illegal in \\+ or if-parts of ->, if; ignored').
metacall('basiccontrol:!', Cut, _) :- !,
	'CUT IDIOM'(Cut).
metacall('!', ?, _) :- !,
        message(warning, '! illegal in \\+ or if-parts of ->, if; ignored').
metacall('!', Cut, _) :- !,
	'CUT IDIOM'(Cut).
% Can be removed??? - the problem is the cut...
metacall('basiccontrol:,'(X, Y), Cut, _) :- !,
	metacall(X, Cut, interpret),
	metacall(Y, Cut, interpret).
metacall(','(X, Y), Cut, _) :- !,
	metacall(X, Cut, interpret),
	metacall(Y, Cut, interpret).
metacall('basiccontrol:;'('basiccontrol:->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall(';'('basiccontrol:->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall('basiccontrol:;'('->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall(';'('->'(X,Y),Z), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	;   metacall(Z, Cut, interpret)
	).
metacall('basiccontrol:->'(X,Y), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	).
metacall('->'(X,Y), Cut, _) :- !,
	(   metacall(X, ?, interpret) ->
	    metacall(Y, Cut, interpret)
	).
metacall('basiccontrol:;'(X,Y), Cut, _) :- !,
	(   metacall(X, Cut, interpret)
	;   metacall(Y, Cut, interpret)
	).
metacall(';'(X,Y), Cut, _) :- !,
	(   metacall(X, Cut, interpret)
	;   metacall(Y, Cut, interpret)
	).
metacall('basiccontrol:\\+'(X), _, _) :- !,
	\+ metacall(X, ?, interpret).
metacall('\\+'(X), _, _) :- !,
	\+ metacall(X, ?, interpret).
metacall('basiccontrol:if'(P,Q,R), Cut, _) :- !,
	if(metacall(P, ?, interpret),
	   metacall(Q, Cut, interpret),
	   metacall(R, Cut, interpret)).
metacall('if'(P,Q,R), Cut, _) :- !,
	if(metacall(P, ?, interpret),
	   metacall(Q, Cut, interpret),
	   metacall(R, Cut, interpret)).
% Commented out to solve Jesus's bug (Jesus reported it ;) ) bug... remove these lines if you feel that everything works fine (Dec 10 2003) 
%metacall('aggregates:^'(_,G), Cut, _) :- !,
%	metacall(G, Cut, interpret).
%metacall('^'(_,G), Cut, _) :- !,
%	metacall(G, Cut, interpret).
metacall(X, _, _) :-
	number(X), !,
	throw(error(type_error(callable,X), 'in metacall')).
metacall(X, _, Mode) :-
	metacall2(Mode, X).

metacall2(interpret, X) :- '$meta_call'(X).
metacall2(undefined, X) :- '$unknown'(F, F), do_undefined(F, X).
metacall2(debug, X) :- debug_trace(X).

do_undefined(error, X) :-
        functor(X, F, A),
        throw(error(existence_error(procedure, F/A), F/A)).
do_undefined(warning, X) :-
        message(warning, ['The predicate ', X, ' is undefined']),
        fail.
% do_undefined(fail, X) :- fail.


:- trust pred 'CHOICE IDIOM'(X) : term(X) => term(X).
:- impl_defined('CHOICE IDIOM'/1).


:- trust pred 'CUT IDIOM'(X) : term(X) => term(X).
:- impl_defined('CUT IDIOM'/1).
