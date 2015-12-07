:- module(condcomp_tr, [condcomp_sentence/3], []).

:- use_module(library(messages), 
	[error_message/2,error_message/3]).

% ---------------------------------------------------------------------------
% State of the translation (private to the module)

% Stack of states
:- data ststack/2.

% The states for conditional compilation indicate whether we are
% inserting sentences or not:
%
%   'enabled':  inserting
%   'else':     not inserting in this block
%               (but will be in the 'else' block if its condition
%                is true)
%   'disabled': not inserting

% Clean all transformation state for this module
clean(Mod) :-
        retractall_fact(condcomp_def(_,_,Mod)),
        retractall_fact(condcomp_fact(_,Mod)),
        retractall_fact(ststack(_,Mod)).

% Get top stack value
ststack__top(Mod, St0) :-
	% Check only first solution
	current_fact(ststack(St, Mod)), !,
	St = St0. 

% Push a new value in the stack
ststack__push(Mod, St) :-
	asserta_fact(ststack(St, Mod)).

% Pop a value from the stack
ststack__pop(Mod) :-
	retract_fact(ststack(_, Mod)), !.

% The ststack stack is empty
ststack__empty(Mod) :-
	\+ current_fact(ststack(_, Mod)).

% ---------------------------------------------------------------------------
% Translation process

condcomp_sentence(0, [], _Mod) :- !.
condcomp_sentence(end_of_file, end_of_file, Mod) :- !,
	% TODO: missing error messages if conditions are left open!
	( ststack__empty(Mod) ->
	    true
	; error_message("End-of-file found before closing conditional compilation block", [])
	),
	clean(Mod).
condcomp_sentence((:- Decl), [], Mod) :- condcomp_directive(Decl), !,
	condcomp_treat(Decl, Mod).
condcomp_sentence(_, [], Mod) :-
	ststack__top(Mod, St),
	\+ St = enabled,
	!.
% If we are here it means that we are processing sentences
condcomp_sentence((:- compilation_fact(Fact)), [], Mod) :- !,
	add_condcomp(Fact, Mod).
% Not processed sentences are left untouched
%condcomp_sentence(_,_,_) :- fail.

condcomp_directive(if(_)).
condcomp_directive(elif(_)).
condcomp_directive(else).
condcomp_directive(endif).

condcomp_treat(Decl, Mod) :-
	( Decl = if(Cond) ->
	    open_block(Mod, Cond)
	; alternative(Decl, Cond) ->
	    alt_block(Mod, Cond, Decl)
	; % Decl = endif
	  close_block(Mod, Decl)
	).

% Directives that alternate a block
alternative(elif(Cond), Cond).
alternative(else, true).

% ---------------------------------------------------------------------------
% Open and close blocks

% Open a block and evaluate its condition
open_block(Mod, Cond) :-
	current_state(Mod, St0),
	open_block_0(Mod, St0, Cond).

open_block_0(Mod, St0, Cond) :-
	( St0 = enabled ->
	    cond_value(Cond, Mod, Value),
	    ( Value = true -> St = enabled
	    ; Value = false -> St = else
	    )
	; St = disabled
	),
	ststack__push(Mod, St).

% Open an alternative block and evaluate its condition
alt_block(Mod, Cond, Decl) :-
	current_state(Mod, St0),
	close_block(Mod, Decl),
	alt_state(St0, St1),
	open_block_0(Mod, St1, Cond).

% Close a block
close_block(Mod, Decl) :-
	( ststack__empty(Mod) ->
	    functor(Decl, Decl0, _),
	    error_message("`:- ~w' directive without a previous `:- if'", [Decl0])
	; ststack__pop(Mod)
	).

% Get the current state
current_state(Mod, St) :- ststack__top(Mod, St0), !, St = St0.
current_state(_Mod, enabled).

% Get the state for the alternative else branch
alt_state(else, St) :- !, St = enabled.
alt_state(_, disabled).

% ---------------------------------------------------------------------------
% Evaluation of conditions

% Evaluate Cond
cond_value(Cond, Mod, Value) :-
	catch(cond_value_(Cond, Mod, Value),
	      peval_unknown(G),
	      cannot_eval(G, Mod, Value)).

cond_value_(Cond, Mod, Value) :-
	( peval_cond(Cond, Mod) ->
	    Value = true
	; Value = false
	).

cannot_eval(G, Mod, Value) :-
	Value = false, % consider bad case as false
	error_message("Cannot evaluate conditional compilation goal `~w' at compile time in module `~w'", [G, Mod]).

% Interpreter of conditions
peval_cond(X, _Mod) :- var(X), !, throw(peval_unknown(X)).
peval_cond(X, Mod) :- cond__def(X, Mod), !, cond__eval(X, Mod).
peval_cond((X,Y), Mod) :- !,
	peval_cond(X, Mod), peval_cond(Y, Mod).
peval_cond((X;Y), Mod) :- !,
	( peval_cond(X, Mod) ; peval_cond(Y, Mod) ).
peval_cond((\+ X), Mod) :- !,
	\+ peval_cond(X, Mod).
peval_cond(true, _Mod) :- !.
peval_cond(fail, _Mod) :- !, fail.
peval_cond(false, _Mod) :- !, fail.
peval_cond(X, _Mod) :-
	throw(peval_unknown(X)).

% Hooks for definition of handlers for conditional goals
:- discontiguous cond__def/2. % goal defined for condition evaluation
:- discontiguous cond__eval/2. % evaluate goal

% ---------------------------------------------------------------------------

cond__def(current_prolog_flag(_, _), _Mod) :- !.
cond__eval(current_prolog_flag(Flag, Value), _Mod) :- !,
	current_prolog_flag(Flag, Value).

% ---------------------------------------------------------------------------

:- use_module(library(compiler/c_itf_internal),
	[compiler_version/1, compiler_name/1]).

cond__def('$with_compiler_version'(_), _Mod) :- !.
cond__eval('$with_compiler_version'(V), _Mod) :- !,
	c_itf_internal:compiler_version(V).

cond__def('$with_compiler_name'(_), _Mod) :- !.
cond__eval('$with_compiler_name'(Name), _Mod) :- !,
	c_itf_internal:compiler_name(Name).

% ---------------------------------------------------------------------------

:- data condcomp_def/3.
:- data condcomp_fact/2.

cond__def(defined(_), _Mod) :- !.
cond__eval(defined(NA), Mod) :- nonvar(NA), NA = N/A, atom(N), number(A), !,
	condcomp_def(N, A, Mod).
cond__eval(defined(N), Mod) :- atom(N), !,
	condcomp_def(N, 0, Mod).

cond__def(X, Mod) :- functor(X, F, A), condcomp_def(F, A, Mod), !.
cond__eval(X, Mod) :-
	condcomp_fact(X, Mod), !. % TODO: always cut?

% ---------------------------------------------------------------------------

add_condcomp(Fact, _Mod) :- var(Fact), !,
	error_message("Uninstantiated term as clause of compilation_fact/1 directive", []).
add_condcomp(Fact, _Mod) :- not_definable(Fact), !,
	functor(Fact, F, A),
	error_message("Redefining `~w' in compilation_fact/1 directive is not allowed", [F/A]).
add_condcomp(Fact, Mod) :-
	functor(Fact, F, A),
	( current_fact(condcomp_def(F, A, Mod)) ->
	    true
	; assertz_fact(condcomp_def(F, A, Mod))
	),
	assertz_fact(condcomp_fact(Fact, Mod)).

not_definable(true).
not_definable(fail).
not_definable(false).
not_definable((_ :- _)).
not_definable((_, _)).
not_definable((_ ; _)).
not_definable((_ -> _)).
not_definable((\+ _)).
