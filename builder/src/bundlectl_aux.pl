:- module(bundlectl_aux, [], [assertions, fsyntax, dcg]).

:- doc(bug, "Complete documentation").
:- doc(bug, "Merge with @apl{ciao_builder} version of bundles").
% TODO: separate dynamic bundle load (for use_module) from the rest

:- doc(title, "Auxiliary for bundlectl").
:- doc(author, "Jose F. Morales").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(strings), [write_string/1]).
:- use_module(library(streams)).
:- use_module(library(write), [portray_clause/1]).
:- use_module(library(system), [file_exists/1, working_directory/2]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(process), [process_call/3]).

:- use_module(engine(internals), ['$bundle_alias_path'/3]).

:- use_module(library(bundle), 
	[ensure_bundle_manifest/2,
	 current_bundle_root/1,
	 lookup_bundle_root/2]).

% ---------------------------------------------------------------------------
:- doc(section, "Manifest and bundle setup").

% TODO: merge with 'bundlectl', 'bundlectl_aux' script (used in external code)

:- export(bundle_configure/1).
% Configure the bundle
bundle_configure(BundleDir) :-
	ensure_bundle_manifest(BundleDir, BundleName),
	ensure_bundle_paths(BundleDir, BundleName),
	message(['Bundle \'', BundleName, '\' successfully configured']).

:- export(bundle_info/1).
% Show bundle info
bundle_info(BundleDir) :-
	BundleDir = ~current_bundle_root,
	ensure_bundle_manifest(BundleDir, BundleName),
	check_bundle_paths(BundleDir, BundleName),
	show_info(BundleDir, BundleName).

:- export(bundle_ciaoc_opts/1).
% Print options for bundle usage in 'ciaoc'
bundle_ciaoc_opts(BundleDir) :-
	ensure_bundle_manifest(BundleDir, BundleName),
	ciaoc_bundle_opts(BundleDir, BundleName).

:- export(bundle_ciaoc_mods/1).
% Print extra modules for bundle usage in 'ciaoc' (for executable creation)
bundle_ciaoc_mods(BundleDir) :-
	ensure_bundle_manifest(BundleDir, BundleName),
	ciaoc_bundle_mods(BundleDir, BundleName).

:- export(bundle_run/3).
% Run a particular executable
bundle_run(BundleDir, Exec, Args) :-
	exec_aux(BundleDir, ['run', Exec|Args]).

:- export(bundle_build/1).
% Build this bundle
bundle_build(BundleDir) :- !,
	exec_aux(BundleDir, ['build']).

:- export(bundle_clean/1).
% Clean the whole bundle
bundle_clean(BundleDir) :-
	exec_aux(BundleDir, ['clean']).

:- export(bundle_test/2).
% Perform the bundle tests
bundle_test(BundleDir, Action) :- !,
	( test_action(Action) ->
	    true
	; throw(error(['Unknown test action ', Action]))
	),
	exec_aux(BundleDir, [Action]).

test_action(check).
test_action(compare).
test_action(briefcompare).
test_action(save).
test_action(bench).

exec_aux(BundleDir, Args) :-
	ensure_bundle_manifest(BundleDir, BundleName),
	exec_aux_(BundleDir, BundleName, Args).
	
exec_aux_(BundleDir, BundleName, Args) :-
	Env = ['CURR_BUNDLEDIR' = BundleDir,
	       'CURR_BUNDLENAME' = BundleName],
	fsR(bundle_src(ciao)/builder/src/'bundlectl_aux.bash', Exec),
	process_call(Exec, Args, 
	             [env(Env), cwd(BundleDir)]).

% ---------------------------------------------------------------------------

show_info(BundleDir, BundleName) :-
	info_string(BundleDir, BundleName, Text, []),
	write_string(Text).

info_string(BundleDir, BundleName) -->
	{ F = ~bundle_paths_file(BundleDir, BundleName) },
	"Recipes for '", atom(BundleName), "' bundle usage:\n",
	"\n",
	"  Executable creation (paths file must also be included in executable):\n",
	"    ciaoc -u ", atom(F), " PARAMS ", atom(F), "\n",
	% TODO: Make executables relocatable without recompilation
	"  NOTE: executables cannot currently be relocated without recompilation\n",
	"\n",
	"  Compilation (no executable):\n",
	"    ciaoc -u ", atom(F), " PARAMS\n",
	"\n",
	"  Top-level usage:\n",
	"    ?- ['", atom(F), "'].\n".

% Options for using this bundle in ciaoc (just compiling)
ciaoc_bundle_opts(BundleDir, BundleName) :-
	ciaoc_bundle_opts_string(BundleDir, BundleName, Text, []),
	write_string(Text).
	
ciaoc_bundle_opts_string(BundleDir, BundleName) -->
	{ F = ~bundle_paths_file(BundleDir, BundleName) },
	"-u ", atom(F).

% Modules for using this bundle in ciaoc (for linking)
ciaoc_bundle_mods(BundleDir, BundleName) :-
	ciaoc_bundle_mods_string(BundleDir, BundleName, Text, []),
	write_string(Text).

ciaoc_bundle_mods_string(BundleDir, BundleName) -->
	{ F = ~bundle_paths_file(BundleDir, BundleName) },
	atom(F).

atom(Atm) --> { atom_codes(Atm, Str) }, string(Str).

string(Str, Xs, Xs0) :- append(Str, Xs0, Xs).

% ---------------------------------------------------------------------------

% Default directory for the bundle state directory at the bundle root
bundle_state_dir('.ciao').

% TODO: memoize
bundle_state_base(BundleDir) := D :-
	BundleStateDir = ~bundle_state_dir,
	atom_concat([BundleDir, '/', BundleStateDir], D).

% TODO: memoize
bundle_paths_file(BundleDir, BundleName) := F :-
	D = ~bundle_state_base(BundleDir),
	M = ~bundle_paths_module(BundleName),
	atom_concat([D, '/', M], F).

bundle_paths_module(BundleName) := M :-
	atom_concat(BundleName, '__paths', M).

% bundle_paths file (with .pl suffix)
bundle_paths_file_pl(BundleDir, BundleName) := F_pl :-
	F_pl = ~atom_concat(~bundle_paths_file(BundleDir, BundleName), '.pl').

bundlereg_file(BundleDir, BundleName) := F :-
	D = ~bundle_state_base(BundleDir),
	atom_concat(BundleName, '.bundlereg', M),
	atom_concat([D, '/', M], F).

% ---------------------------------------------------------------------------

% Create the bundle state base (if it does not exists)
ensure_bundle_state_dir(BundleDir) :-
	D = ~bundle_state_base(BundleDir),
	mkpath(D).

% ---------------------------------------------------------------------------

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(aggregates), [findall/3]).

% Ensure that BundleDir contains the bundle_paths file
ensure_bundle_paths(BundleDir, BundleName) :-
	has_bundle_paths(BundleDir, BundleName),
	!.
ensure_bundle_paths(BundleDir, BundleName) :-
	generate_bundle_paths(BundleDir, BundleName).

% Generate the bundle_paths file for BundleDir
generate_bundle_paths(BundleDir, BundleName) :-
	ensure_bundle_state_dir(BundleDir),
	F_pl = ~bundle_paths_file_pl(BundleDir, BundleName),
	open_output(F_pl, Out),
	bundle_paths(BundleName),
	close_output(Out).

% BundleDir contains the bundle_paths file
has_bundle_paths(BundleDir, BundleName) :-
	ensure_bundle_state_dir(BundleDir),
	F_pl = ~bundle_paths_file_pl(BundleDir, BundleName),
	file_exists(F_pl),
	!.

% Check that BundleDir contains the bundle_paths file
check_bundle_paths(BundleDir, BundleName) :-
	has_bundle_paths(BundleDir, BundleName),
	!.
check_bundle_paths(_BundleDir, BundleName) :-
	throw(error(['Bundle \'', BundleName, '\' is not configured'])).

% Contents of the bundle_paths file (based on the Manifest information for BundleDir)
bundle_paths(BundleName) :-
	M = ~bundle_paths_module(BundleName),
	portray_clause((:- module(M, [], []))),
	portray_clause((:- multifile file_search_path/2)),
	portray_clause((:- dynamic file_search_path/2)),
	portray_clause((:- multifile library_directory/1)),
	portray_clause((:- dynamic library_directory/1)),
	As = ~get_aliases(BundleName),
	alias_clauses(As).

get_aliases(Bundle) := As :-
	findall(alias(Alias, Path),
                '$bundle_alias_path'(Alias, Bundle, Path),
		As).

alias_clauses([]).
alias_clauses([A|As]) :-
	alias_clause(A),
	alias_clauses(As).

alias_clause(alias(Alias, Path)) :-
	( Alias = library ->
	    C = library_directory(Path)
	; C = file_search_path(Alias, Path)
	),
	portray_clause(C).


