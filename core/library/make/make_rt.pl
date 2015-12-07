:- module(make_rt, [make/1, target/1, make_option/1,
		trace_message/1,
		trace_message/2,
		call_unknown/1,
		all_values/2,
		get_value/2,
		get_value_def/3,
		get_all_values/2,
		name_value/2,
		set_name_value/2,
		cp_name_value/2,
		parse_name_value/3,
		parse_name_value_string/3,
		add_name_value/2,
		del_name_value/1,
		check_var_exists/1,
		find_file/2,
		vpath/1,
		add_vpath/1,
		newer/2,
		register_module/1, % TODO: unused?
		unregister_module/1, % TODO: unused?
		push_name_value/3,
		pop_name_value/1,
		push_active_config/1,
		pop_active_config/0,
		get_active_config/1,
		dyn_load_cfg_module_into_make/1,
		get_settings_nvalue/1],
	    [assertions, regtypes, hiord]).

:- include(library(make/make_com)).

%% ---------------------------------------------------------------------------

:- doc(title, "Predicates Available When Using The Make Package").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Edison Mera").

:- doc(usage, "This module is loaded automatically when the
   @lib{make} library package is used.").

:- doc(module, "This is the run-time module which implements the
   predicates which are provided when using the @lib{make} library
   package in a given application. For example, they are used
   internally by @apl{lpmake}.").

%% Needs fixing the English... MH
:- doc(bug, "The current handle of help messages is defficient.
   It must be in a standar form, and the user of this library only
   must be allowed to add messages, not procedures to print it.").

:- doc(bug, "target_comment/1 does not work, why? :-(.").

%% ---------------------------------------------------------------------------

:- use_module(library(compiler), [use_module/1, unload/1]).
:- use_module(library(pathnames), [path_splitext/3, path_concat/3, path_basename/2]).
:- use_module(library(terms),     [atom_concat/2]).
:- use_module(library(system),    [file_exists/1]).
:- use_module(library(lists),     [append/3]).

:- use_module(library(messages)).

:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(library(format), [format_control/1]).
:- pop_prolog_flag(unused_pred_warnings).

:- use_module(library(lists), [list_concat/2]).
:- reexport(library(make/up_to_date)).

:- data name_value/2.
:- data vpath/1.

:- data active_config/1.

active_config(_). % TODO: why a free var?

register_module(A) :- dyn_load_cfg_module_into_make(A). % TODO: not used?
unregister_module(A) :- unload(A). % TODO: not used?

add_name_value(Name, Value) :-
	data_facts:assertz_fact(name_value(Name, Value)).

del_name_value(Name) :-
	data_facts:retractall_fact(name_value(Name, _)).

set_name_value(Name, Value) :-
	data_facts:retractall_fact(name_value(Name, _)),
	data_facts:asserta_fact(name_value(Name, Value)).

% Push ActiveConfig on the active_config/1 stack
push_active_config(ActiveConfig) :-
	data_facts:asserta_fact(active_config(ActiveConfig)).

% Remove the top of the active_config/1 stack
pop_active_config :-
	data_facts:retract_fact(active_config(_)),
	!.

% Get the top of the active_config/1 stack
get_active_config(ActiveConfig) :-
	active_config(ActiveConfig),
	!.


:- pred push_name_value(Name, Var, R)

# "Push variable name @var{Name} with all values of variable @var{Var}
  and returns @var{R}, an abstract type to pass to
  @pred{pop_name_value/1} to undo the push changes. Push cannot be
  nested.".

push_name_value(Name, Var, R) :-
	findall(L, get_value(Var, L), Values),
	data_facts:retractall_fact(name_value(Name, _)),
	push_name_value__(Values, Name, R).


push_name_value__([],     _,    []).
push_name_value__([A|As], Name, [RA|RAs]) :-
	data_facts:assertz_fact(name_value(Name, A), RA),
	push_name_value__(As, Name, RAs).


:- pred pop_name_value(R)

# "Restores the value of the variable indicated by the abstract type
  @var{R}. Notice that @var{R} _must be_ the argument returned by
  @pred{push_name_value/2}.".

pop_name_value([]).
pop_name_value([R|Rs]) :-
	erase(R),
	pop_name_value(Rs).


:- pred cp_name_value(Source, Target)
	: (atm(Source), atm(Target))
# "Copy the variable values from @var{Source} to @var{Target}".

cp_name_value(Source, Target) :-
	data_facts:retractall_fact(name_value(Target, _)),
	(
	    get_value(Source, Value),
	    data_facts:assertz_fact(name_value(Target, Value)),
	    fail
	;
	    true
	).

add_vpath(Path) :-
	path_concat(Path, '', DirPath),
	( data_facts:current_fact(vpath(DirPath)) ->
	    true
	; data_facts:assertz_fact(vpath(DirPath))
	).

:- regtype target(T) # "@var{T} is a Makefile target.".

target(X) :- atm(X).

verb_target_comment(Target) :-
	( make_option('--trace-deps') ->
	    show_target_comment(Target)
	; true
	).

show_target_comment(Target) :-
	( call_unknown(_:target_comment(Target)) ->
	    true
	; ( get_active_config(AC),
	    m_target_comment(AC, Target, Comment, Args) ->
	      simple_message("~w: "||Comment, [Target|Args])
	  ; true
	  )
	).

:- pred make(TargetList) : list(target)
# "This is the main entry point to the make library. It makes the list
	of targets one by one as well as any intermediate targets
	needed as dictated by the dependency rules.".

make(A) :-
	dependency_list(A, R, [], Faileds, R, []),
	show_dependency_list(R, A),
	catch(list(R, make_dep),
	    make_error(Message, Args),
	    rethrow_make_error(Faileds, Message, Args)).

dependency_list(Targets, R0, ProcessedTargets, Faileds, R1, R) :-
	( dependency_list_(Targets, R0, ProcessedTargets, R1, R) -> true
	; R1 = R
	),
	findall(Failed, retract_fact(failed_target(Failed)), Faileds).

rethrow_make_error([], Message, Args) :-
	!,
	throw(make_error(Message, Args)).
rethrow_make_error(Faileds, Message, Args) :-
	throw(make_error(
		"Could not complete ~w. " ||
		"Verify that the dependent elements "||
		"exist in a known path or that "||
		"it is a valid target.  Use the --trace-deps option " ||
		"to see more detailed information.\n" || Message,
		[Faileds|Args])).

% dependency_list(A, R) :-
% 	dependency_list(A, R, [], R, []).

do_target(Target, VarSet) :-
	get_active_config(AC),
	m_do_target_atm(AC, Target, VarSet),
	!.
do_target(Target, VarSet) :-
	get_active_config(AC),
	m_do_target_var(AC, Target, VarSet).

make_dep(do_target(Target, VarSet)) :-
	show_trace_processing(Target),
	verb_target_comment(Target),
	(
	    do_target(Target, VarSet) -> true
	;
	    throw(make_error("Failure when making target ~w", [Target]))
	).
make_dep(do_dependency(Target, TSuffix, SSuffix, FileBase)) :-
	do_show_dependency_comment(TSuffix, SSuffix, FileBase),
	(
	    get_active_config(AC),
	    m_do_dependency(AC, TSuffix, SSuffix, FileBase) -> true
	;
	    throw(make_error("Failure when making dependency ~w <- ~w~w",
		    [Target, FileBase, SSuffix]))
	).

do_show_dependency_comment(TSuffix, SSuffix, FileBase) :-
	(
	    call_unknown(_:dependency_comment(SSuffix, TSuffix, FileBase))
	-> true
	;
	    show_dependency_comment(SSuffix, TSuffix, FileBase)
	).

% ----------------------------------------------------------------------------

is_member(_, L) :-
	var(L),
	!,
	fail.
is_member(X, [Y|_]) :-
	X = Y,
	!.
is_member(X, [_|L]) :-
	is_member(X, L).

:- data failed_target/1.

dependency_list_([],               _,  _,                R,  R) :- !.
dependency_list_([Target|Targets], R0, ProcessedTargets, R1, R) :-
	!,
	dependency_list_(Target,  R0, ProcessedTargets, R1, R2),
	dependency_list_(Targets, R0, ProcessedTargets, R2, R).
dependency_list_(Target, R0, ProcessedTargets, R1, R) :-
	( is_member(Target, ProcessedTargets) ->
	    show_warning_circular_reference(Target, ProcessedTargets),
	    R1 = R
	;
	    get_active_config(AC),
	    (
		m_target_exists(AC, Target),
		dependency_target(Target, R0, ProcessedTargets, R1, R) ->
		true
	    ;
		path_splitext(Target, FileBase, TSuffix),
		m_dependency_exists(AC, TSuffix, SSuffix),
		dependency_suffix(Target, FileBase, TSuffix,
		    SSuffix, R0, ProcessedTargets, R1, R) ->
		true
	    ;
		find_file(Target, PathTarget) ->
		show_trace_unconditional_target_exists(PathTarget),
		R1 = R
	    ;
		(
		    m_target_exists(AC, Target) ->
		    show_trace_dependent_target_not_exist(Target)
		;
		    path_splitext(Target, FileBase, TSuffix),
		    m_dependency_exists(AC, TSuffix, SSuffix) ->
		    show_trace_dependent_target_not_exist(Target)
		;
		    show_warning_target_not_exist(Target)
		),
		assertz_fact(failed_target(Target)),
		fail
	    )
	).

dependency_target(Target, R0, ProcessedTargets, R1, R) :-
	E = do_target(Target, VarSet),
	( is_member(E, R0) ->
	    show_trace_ignoring_already_added_target(Target),
	    R1 = R
	;
	    get_active_config(AC),
	    m_target_deps(AC, Target, Deps, VarSet),
	    dependency_list_(Deps, R0, [Target|ProcessedTargets], R1, R2),
	    insert_dependency_element(Target, Deps, E, R0, R2, R),
	    ( Deps == [] -> show_trace_unconditional_target(Target)
	    ; show_trace_conditional_target(Target, Deps)
	    )
	).

dependency_suffix(Target, FileBase, TSuffix, SSuffix, R0, ProcessedTargets,
	    R1, R) :-
	show_trace_checking(FileBase, TSuffix, SSuffix),
	atom_concat(FileBase, SSuffix, Dep),
	(
	    get_active_config(AC),
	    m_dependency_precond(AC, TSuffix, SSuffix, Deps0) ->
	    Deps = [Dep, Deps0]
	;
	    Deps = Dep
	),
	E = do_dependency(Target, TSuffix, SSuffix, FileBase),
	(
	    is_member(E, R0) ->
	    show_trace_ignoring_already_added_dependency(FileBase, TSuffix,
		SSuffix),
	    R1 = R
	;
	    dependency_list_(Deps, R0, [Target|ProcessedTargets], R1, R2),
	    show_trace_dependency(FileBase, TSuffix, SSuffix),
	    insert_dependency_element(Target, Deps, E, R0, R2, R)
	).

already_added(Dep, _) :-
	var(Dep),
	!,
	fail.
already_added([], _) :-
	!,
	fail.
already_added([Deps|_], R) :-
	already_added(Deps, R),
	!.
already_added([_|Deps], R) :-
	already_added(Deps, R),
	!.
already_added(Dep, R) :-
	is_member(do_target(Dep, _), R),
	!.
already_added(Dep, R) :-
	is_member(do_dependency(Dep, _, _, _), R),
	!.

insert_dependency_element(Target, Deps, E, R0, R1, R) :-
	(
	    nonvar(Deps),
	    newer(Deps, Target),
	    \+ already_added(Deps, R0) ->
	    show_trace_up_to_date(Target),
	    R1 = R
	;
	    R1 = [E|R]
	).

show_dependency_list(R, A) :-
	( R \= [] -> trace_message("Dependency list for ~w is ~w", [A, R])
	; % TODO: This cannot be an error since sometimes there are no dependencies
% 	    --jfran
% 	error_message("No rules found to build ~w.", [A])
	    trace_message("Dependency list for ~w is empty.", [A])
	).
show_trace_ignoring_already_added_target(Target) :-
	trace_message("Ignoring already added target ~w", [Target]).
show_trace_unconditional_target(Target) :-
	trace_message("Making unconditional target ~w", [Target]).
show_trace_conditional_target(Target, Deps) :-
	trace_message("Adding conditional target ~w < ~w", [Target, Deps]).
show_trace_unconditional_target_exists(PathTarget) :-
	trace_message("Unconditional target ~w exists", [PathTarget]).
show_warning_target_not_exist(PathTarget) :-
	warning_message("Target ~w does not exist", [PathTarget]).
show_trace_dependent_target_not_exist(PathTarget) :-
	trace_message("Target ~w does not exist", [PathTarget]).
show_warning_circular_reference(Target, ProcessedTargets) :-
	warning_message("Ignoring circular reference ~w -> ~w",
	    [Target, ProcessedTargets]).
show_trace_up_to_date(Target) :-
	trace_message("~w is up to date", [Target]).
show_trace_processing(Target) :-
	trace_message("Processing ~w", [Target]).
show_trace_dependency(FileBase, TSuffix, SSuffix) :-
	trace_message("Found that ~w~w can be generated from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
show_trace_checking(FileBase, TSuffix, SSuffix) :-
	trace_message("Checking if ~w~w should be generated from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
%% This is what makes calls for dependency targets
show_dependency_comment(SSuffix, TSuffix, FileBase) :-
	trace_message("Generating ~w~w from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
show_trace_ignoring_already_added_dependency(FileBase, TSuffix, SSuffix) :-
	trace_message("Ignoring already added dependency ~w~w from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).

%% ---------------------------------------------------------------------------
%% Procesing target dependencies
%% ---------------------------------------------------------------------------

newer(Files, Target) :-
	trace_message("Checking if ~w is newer than ~w", [Files, Target]),
	find_file(Target, PathTarget),
	newer_(Files, PathTarget).

newer_([],           _) :- !.
newer_([File|Files], Target) :-
	!,
	newer_(File,  Target),
	newer_(Files, Target).
newer_(File, Target) :-
	find_file(File, PathFile),
	trace_message("Checking if ~w is newer", [PathFile]),
	up_to_date(Target, PathFile).

%% ---------------------------------------------------------------------------
%% Resolve the path name for file w.r.t. the paths in vpath/1
%% ---------------------------------------------------------------------------

% Find file in current directory or any of vpath/1
find_file(File, PathFile) :-
	( Path = ''
	; vpath(Path)
	),
	path_concat(Path, File, PathFile),
	file_exists(PathFile),
	!.

%% ---------------------------------------------------------------------------
%% Support code
%% ---------------------------------------------------------------------------

:- pred dyn_load_cfg_module_into_make(ConfigFile) : sourcename

# "Used to load dynamically a module (typically, a @file{Makefile})
      into the make library from the application using the library.".

dyn_load_cfg_module_into_make(ConfigFile) :-
	use_module(ConfigFile).

:- pred make_option(Option) : atm

# "Asserting/retracting facts of this predicate sets/clears library 
      options. Default is no options (i.e., the predicate is undefined). The 
      following values are supported:
@begin{verbatim}
make_option('--trace-deps'). % Verbose: prints progress messages (useful 
                             % for debugging rules).
@end{verbatim}
  ".

:- data make_option/1.

%% Default is silent. Typically asserted by 
%% make_option('--trace-deps').

:- pred trace_message(Text, ArgList) : format_control * list

# "The text provided in @var{Text} is printed as a message, using the
   arguments in @var{ArgList}, if @tt{make_option('--trace-deps')} is
   defined. Otherwise nothing is printed.".

:- push_prolog_flag(multi_arity_warnings, off).

trace_message(Text) :-
	trace_message(Text, []).

trace_message(Mess, Args) :-
	( make_option('--trace-deps') ->
	    simple_message(Mess, Args)
	;
	    true
	).

:- pop_prolog_flag(multi_arity_warnings).

:- use_module(library(aggregates)).

% :- meta_predicate call_unknown(goal).

call_unknown(G) :-
	prolog_flag(unknown, Old,  fail),
	prolog_flag(quiet,   QOld, error),
	(
	    call(G),
	    prolog_flag(unknown, _, Old),
	    prolog_flag(quiet, _, QOld)
	; prolog_flag(unknown, _, Old),
	    prolog_flag(quiet, _, QOld),
	    fail
	).

all_pred_values(Name, Values) :-
	findall(Value,
	    (
		Pred =.. [Name, Value],
		call_unknown(_:Pred)
	    ),
	    Values).

all_name_values(Name, Values) :-
	findall(Value, name_value(Name, Value), Values).

% read all values
all_values(Name, Values) :-
	all_name_values(Name, Values0),
	(
	    Values0 == [] ->
	    all_pred_values(Name, Values)
	;
	    Values = Values0
	).

parse_name_value(NameValue, Name, Value) :-
	parse_name_value_string(NameValue, Name, ValueS),
	atom_codes(Value, ValueS).

parse_name_value_string(NameValue, Name, ValueS) :-
	atom_codes(NameValue, NameValueS),
	list_concat([NameS, "=", ValueS], NameValueS),
	!,
	atom_codes(Name, NameS).

get_value(Name, Value) :-
	name_value(Name, _) ->
	name_value(Name, Value)
    ;
	get_pred_value(Name, Value).

% (Get value, given by a predicate definition Name/1)
get_pred_value(Name, Value) :-
	atom(Name),
	!,
	Pred =.. [Name, Value],
	call_unknown(_:Pred).
get_pred_value(Name, Value) :-
	% TODO: used? documented? similar to functional notation
	Name =.. Flat,
	append(Flat, [Value], PredList),
	Pred =.. PredList,
	call_unknown(_:Pred).

get_value_def(Name, DefValue, Value) :-
	(get_value(Name, Value) -> true ; DefValue = Value).
% 	name_value(Name, Value) -> true
%  ;
% 	get_pred_value_def(Name, DefValue, Value).
%
% get_pred_value_def(Name, _DefValue, Value) :-
% 	Pred =.. [Name, Value],
% 	catch(call_unknown(_:Pred), _Error, false),
% 	!.
% get_pred_value_def(_PredName, DefValue, DefValue).


:- pred get_settings_nvalue(Pred)
	: term(Pred)

# "Executes @var{Pred} as unkown call, in other words, it is useful to
  execute predicates that have been loaded by @var{register_module/1}. 

  Example: @tt{get_settings_nvalue(my_options(ciao, A, B)).}".

get_settings_nvalue(Pred) :-
	call_unknown(_:Pred).

:- meta_predicate check_var_exists(addmodule).

:- pred check_var_exists(Var)

# "Fails printing a message if variable @var{Var} does not exist.".

check_var_exists(Var, _) :-
	get_value(Var, _),
	!.
check_var_exists(Var, M) :-
	error_message(_, "In module ~w: Variable ~w not found", [M, Var]),
% 	display( 'Current Defined Variables:\n' ),
% 	get_value( V , VV ),
% 	displayq( V ),
% 	display( ' = ' ),
% 	displayq( VV ),nl,
	fail.

:- pred get_all_values(Name, Values)

# "@var{Values} are all the possible values of @var{Name}.".

get_all_values(Name, Value) :-
	findall(V, get_value(Name, V), Value).

