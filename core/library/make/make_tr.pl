:- module(make_tr, [defdep/3], [assertions]).

:- include(library(make/make_ops)).

:- use_module(library(terms_vars)).
:- use_module(library(messages), [error_message/2]).

%% defdep( ( T <= S :: F <- Deps :- Body ), Clauses, _Mod) :- 
%% 	!,
%% 	Clauses = [ (  do_dependency(T,S,F)         :- Body ), 
%% Note that this is different from what lpdoc 2.0 is using now!
%% 	            (  dependency_precond(T,S,Deps) :- true ), 
%% 	            (  dependency_exists(T,S)       :- true ) ], 
%% 	debug_transformation(Clauses).

%% %% Example (should call make(precond):
%% double <= simple <- precond :: Name :-
%% 	file_to_string(~atom_concat([Name,'.simple']),Content),
%% 	append(Content,[0'\n|Content],DoubleContent),
%% 	string_to_file(DoubleContent,~atom_concat([Name,'.double'])).

:- data dependency_exists/3.
:- data target_exists/2.
%:- data dependency_separator/2.

:- data defined_target_comment/1.
:- data defined_dependency_precond/1.
:- data defined_target_deps/1.

% Prefix with '.' if not prefixed (to obtain extensions),
% or kept intact if '' is passed.
% (e.g., 'pl' -> '.pl')
dot_concat(A0, A) :- A0 == '', !, A = A0.
dot_concat(A0, A) :- atom_codes(A0, [0'.|_]), !, A = A0.
dot_concat(A0, A) :- atom_concat('.', A0, A).

gen_clause_dependency(T, S, Deps, F, Body, Clauses, Mod) :-
%	dependency_separator(Separator,Mod),
	dot_concat(T, TSuffix),
	dot_concat(S, SSuffix),
	Clauses0 = [(do_dependency(TSuffix, SSuffix, F) :- Body)],
	( dependency_exists(TSuffix, SSuffix, Mod) ->
	    Clauses1 = Clauses0
	;
	    Clauses1 = [( dependency_exists(TSuffix, SSuffix) :-
		    true )|Clauses0],
	    assertz_fact(dependency_exists(TSuffix, SSuffix, Mod))
	),
	(
	    Deps == [] ->
	    Clauses = Clauses1
	;
	    Clauses = [( dependency_precond(TSuffix, SSuffix, Deps) :-
		    true )|Clauses1],
	    ( defined_dependency_precond(Mod) -> true
	    ; assertz_fact(defined_dependency_precond(Mod)) )
	).

gen_clause_target(Target, Deps, TargetName, Body, Clauses, Mod) :-
% Note that TargetName must be solved at run time
	varset(Deps, VarSet),
	( atom(Target) ->
	    Clauses0 = [( do_target_atm(Target, VarSet) :- TargetName=Target,
		    Body )]
	;
	    Clauses0 = [( do_target_var(TargetName, VarSet) :-
		    TargetName = Target, !, Body )]
	),
	( target_exists(Target, Mod) ->
	    Clauses1 = Clauses0
	;
	    Clauses1 = [(target_exists(Target) :- true)|Clauses0],
	    assertz_fact(target_exists(Target, Mod))
	),
	(
%	    Deps == [] ->
%	    Clauses = Clauses1
%	;
	    ( atom(Target) ->
		Clauses = [( target_deps(Target, Deps0, VarSet) :- !, Deps0 =
			Deps )|Clauses1]
	    ;
		Clauses = [( target_deps(TargetName, Deps0, VarSet) :-
			TargetName=Target, !, Deps0 = Deps )|Clauses1]
	    ),
	    ( defined_target_deps(Mod) -> true ; assertz_fact(
		    defined_target_deps(Mod)) )
	).

gen_clause_target_comment(Target, Deps, TargetName, Comment, Body, Clauses,
	    Mod) :-
	Clauses = [(target_comment(Target, Comment, []) :- true)|Clauses0],
	( defined_target_comment(Mod) -> true
	; assertz_fact(defined_target_comment(Mod)) ),
	gen_clause_target(Target, Deps, TargetName, Body, Clauses0, Mod).

get_multifile_preds(Clauses, M) :-
	( dependency_exists(_, _, M) ->
	    retractall_fact(dependency_exists(_, _, M)),
	    Clauses = [( m_dependency_exists(M, A, B) :-
		    M:dependency_exists(A, B) ),
		( m_do_dependency(M, A, B, C) :-
		    M:do_dependency(A, B, C) )|Clauses1]
	;
	    Clauses = Clauses1
	),
	( target_exists(T0, M), atom(T0) ->
	    Clauses1 = [( m_do_target_atm(M, A, B) :-
		    M:do_target_atm(A, B) )|Clauses2]
	;
	    Clauses1 = Clauses2
	),
	( target_exists(T1, M), \+atom(T1) ->
	    Clauses2 = [( m_do_target_var(M, A, B) :-
		    M:do_target_var(A, B) )|Clauses3]
	;
	    Clauses2 = Clauses3
	),
	( target_exists(_, M) ->
	    retractall_fact(target_exists(_, M)),
	    Clauses3 = [( m_target_exists(M, A) :-
		    M:target_exists(A) )|Clauses4]
	;
	    Clauses3 = Clauses4
	),
	(
	    defined_target_deps(M) ->
	    retractall_fact(defined_target_deps(M)),
	    Clauses4 = [( m_target_deps(M, A, B, C) :-
		    M:target_deps(A, B, C) )|Clauses5]
	;
	    Clauses4 = Clauses5
	),
	(
	    defined_dependency_precond(M) ->
	    retractall_fact(defined_dependency_precond(M)),
	    Clauses5 = [( m_dependency_precond(M, A, B, C) :-
		    M:dependency_precond(A, B, C) )|Clauses6]
	;
	    Clauses5 = Clauses6
	),
	(
	    defined_target_comment(M) ->
	    retractall_fact(defined_target_comment(M)),
	    Clauses6 = [( m_target_comment(M, A, B, C) :-
		    M:target_comment(A, B, C) )|Clauses7]
	;
	    Clauses6 = Clauses7
	),
	Clauses7 = [end_of_file].

% by default the separator is '.' but could be ''
% defdep(0,_,M) :-
% 	asserta_fact(dependency_separator('.',M)).
defdep(end_of_file, Clauses, M) :-
	get_multifile_preds(Clauses, M).

%	retractall_fact(dependency_separator(_,M)).
% The next is the syntax to put a dependency_precond/3:
defdep((T <= S <- F :- _Body), [], _Mod) :-
	!,
%	dependency_separator(Separator,Mod),
	error_message("Option ~w <= ~w <- ~w :- Body " ||
	    "is no longer supported. Use T <= S <- D :: F :- B instead",
	    [T, S, F]).
%	dot_concat(T,TSuffix),
%	dot_concat(S,SSuffix),
%	Clauses = [ ( dependency_precond(TSuffix,SSuffix,F) :- Body ) ],

defdep((T <= S <- Deps :: F :- Body), Clauses, Mod) :-
	!,
	gen_clause_dependency(T, S, Deps, F, Body, Clauses, Mod).
defdep((T <= S :: F :- Body), Clauses, Mod) :-
	!,
	gen_clause_dependency(T, S, [], F, Body, Clauses, Mod).
defdep((Target <- DepsComment :- Body), Clauses, Mod) :-
	nonvar(DepsComment),
	DepsComment = (Deps # Comment),
	!,
	gen_clause_target_comment(Target, Deps, _TargetName, Comment, Body,
	    Clauses, Mod).
defdep((Target <- Deps :- Body), Clauses, Mod) :-
	!,
	gen_clause_target(Target, Deps, _TargetName, Body, Clauses, Mod).
defdep((Target <- :- Body), Clauses, Mod) :-
	!,
	gen_clause_target(Target, [], _TargetName, Body, Clauses, Mod).
% This clause is for backward compatibility:
defdep((Target <- Deps # Comment :: TargetName :- Body), Clauses, Mod) :-
	nonvar(Comment),
	!,
	gen_clause_target(Target, Deps, TargetName, Body, Clauses, Mod).
defdep((Target <- Deps :: TargetName # Comment :- Body), Clauses, Mod) :-
	nonvar(Comment),
	!,
	gen_clause_target_comment(Target, Deps, TargetName, Comment, Body,
	    Clauses, Mod).
defdep((Target <- Deps :: TargetName :- Body), Clauses, Mod) :-
	!,
	gen_clause_target(Target, Deps, TargetName, Body, Clauses, Mod).
%	defdep( ( Target <- Deps :- Body ), Clauses, Mod).
defdep((:- target_comment(Target, Comment)),
	    [(target_comment(Target, Comment, []))], Mod) :-
	( defined_target_comment(Mod) -> true ; assertz_fact(
		defined_target_comment(Mod)) ).
defdep((:- target_comment(Target, Comment, Args)),
	    [(target_comment(Target, Comment, Args))], Mod) :-
	( defined_target_comment(Mod) -> true ; assertz_fact(
		defined_target_comment(Mod)) ).
% defdep( (:- dependency_separator(Separator) ), [], Mod) :-
% 	retractall_fact(dependency_separator(_, Mod)),
% 	asserta_fact(dependency_separator(Separator, Mod)).

concat_bodies((G, Gs), B, (G, NB)) :- !,
	concat_bodies(Gs, B, NB).
concat_bodies(G, B, (G, B)).
