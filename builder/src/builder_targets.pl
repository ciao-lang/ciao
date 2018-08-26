:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title, "Targets for builder").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module defines predicates to map between input
   targets (which can be paths to bundles, bundle names, or paths to
   workspaces) and resolved target names (absolute paths to workspace
   or symbolic bundle names)").

:- use_module(library(hiordlib), [maplist/2, maplist/3]).

% ---------------------------------------------------------------------------
:- doc(section, "Target names for the builder").

:- regtype target/1 # "A target for the builder".
target(X) :- bundle(X). % e.g., core
target(X) :- bundle_part(X). % e.g., core.engine
target(X) :- workspace(X). % e.g., CIAOROOT or any path in CIAOPATH

:- regtype bundle/1 # "A bundle name".
bundle(X) :- atm(X).

:- regtype bundle_part/1 # "Part (item) of a bundle".
bundle_part(X) :- atm(X).

:- regtype workspace/1 # "A workspace (absolute path)".
workspace(X) :- atm(X).

% ---------------------------------------------------------------------------
:- doc(section, "Updating and resolving targets").

:- use_module(ciaobld(bundle_scan), [scan_bundles_at_path/1]).
:- use_module(engine(internals),
	['$bundle_id'/1,
	 '$bundle_srcdir'/2]).
:- use_module(ciaobld(builder_aux),
	[lookup_workspace/2,
	 dir_to_bundle/2]).
:- use_module(ciaobld(manifest_compiler),
	[lookup_bundle_root/2,
	 split_target/3]).
:- use_module(ciaobld(bundle_fetch),
	[check_bundle_alias/3,
	 add_bundle_origin/2]).

:- data target_workspace/1. 

% TODO: rescursive all_workspaces does not rescan all workspaces, fix

:- export(rescan_targets/1).
:- pred rescan_targets(+Targets) :: list(atm)
   # "(Re)scan bundles at the workspaces where targets are located".

rescan_targets(Targets) :-
	retractall_fact(target_workspace(_)),
	maplist(collect_workspace, Targets),
	( target_workspace(Path),
	    scan_bundles_at_path(Path),
	    fail
	; true
	),
	retractall_fact(target_workspace(_)).

collect_workspace(Target) :-
	( target_to_workspace(Target, Path) ->
	    ( target_workspace(Path) -> true
	    ; assertz_fact(target_workspace(Path))
	    )
	; true
	).

% (Fail if target does not correspond to a directory known workspace
% or a directory under a known bundle)
target_to_workspace(Target, Path) :-
	( is_dir(Target) ->
	    Path0 = Target
	; split_target(Target, Bundle, _Part),
	  '$bundle_id'(Bundle),
	  '$bundle_srcdir'(Bundle, Path0)
	),
	lookup_workspace(Path0, Path).

:- export(resolve_targets/3).
:- pred resolve_targets(+Targets0, OnUnknown, -Targets) :: list(atm) * term * list(target)
   # "Resolve targets (which may be paths). This assumes that
     @pred{rescan_targets/1} has been called".

resolve_targets(Targets0, OnUnknown, Targets) :-
	maplist(resolve_target(OnUnknown), Targets0, Targets).

:- pred resolve_target(+Target0, +OnUnknown, -Target) :: atm * term *target.
resolve_target(Target0, OnUnknown, Target) :-
	( is_dir(Target0),
	  lookup_bundle_root(Target0, BundleDir),
	  dir_to_bundle(BundleDir, Bundle) ->
	    Target = Bundle
	; is_dir(Target0),
	  lookup_workspace(Target0, Target1) -> % Target1 is absolute file name at this point
	    Target = Target1 % (absolute paths are workspace targets)
	; check_bundle_alias(Target0, Origin, Bundle) ->
            add_bundle_origin(Bundle, Origin),
	    Target = Bundle
	; split_target(Target0, Bundle, _Part),
	  '$bundle_id'(Bundle) ->
	    Target = Target0 % TODO: missing check that _Part is a valid item
	; ( OnUnknown = error ->
	      throw(unknown_target(Target0))
	  ; % OnUnknown = silent
	    Target = Target0
	  )
	).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_properties/6]).
:- use_module(engine(prolog_flags), [prolog_flag/3, set_prolog_flag/2]).

% TODO: duplicated
% TODO: better solution (e.g., catch exceptions)
is_dir(Path) :-
        prolog_flag(fileerrors, OldFE, off),
        file_properties(Path, directory, [], [], [], []),
        set_prolog_flag(fileerrors, OldFE).
