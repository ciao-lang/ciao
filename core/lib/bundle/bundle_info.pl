:- module(_, [], [assertions, basicmodes, nativeprops, fsyntax, hiord, regtypes]).

:- doc(title,  "Bundle Information").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module defines the operations to extract
   additional bundle information (like versions from VCS).").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [reverse/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(internals),
	['$bundle_id'/1, '$bundle_prop'/2, '$bundle_srcdir'/2]).

% ---------------------------------------------------------------------------
% Bundle dependencies

% The root bundle (that collects or registered bundles)
:- export(root_bundle/1).
root_bundle(ciao). % TODO: This may not be right if users install their own bundles

:- export(bundle_deps/2).
% Obtain recursively all bundle dependencies of the given list of
% bundles, based on 'bundle_requires' property. Enumerate dependencies
% first.
bundle_deps(Bundles, Deps) :-
	bundle_deps_(Bundles, [], _Seen, Deps, []).

bundle_deps_([], Seen0, Seen, Deps, Deps0) :- !, Deps = Deps0, Seen = Seen0.
bundle_deps_([B|Bs], Seen0, Seen, Deps, Deps0) :-
	( member(B, Seen0) -> % seen, ignore
	    Deps = Deps2, Seen2 = Seen0
	; ( '$bundle_prop'(B, requires(Requires)) -> true
	  ; Requires = []
	  ),
	  Seen1 = [B|Seen0],
	  bundle_deps_(Requires, Seen1, Seen2, Deps, Deps1),
	  Deps1 = [B|Deps2]
	),
	bundle_deps_(Bs, Seen2, Seen, Deps2, Deps0).

% TODO: sub_bundle only make sense for root_bundle, remove or generalize?

:- export(enum_sub_bundles/2).
% Bundle is a sub-bundle of ParentBundle or a dependency of it
% (nondet, order matters)
enum_sub_bundles(ParentBundle, Bundle) :-
	( root_bundle(ParentBundle) ->
	    member(Bundle, ~bundle_deps(~all_bundles))
	; fail
	).

:- export(enumrev_sub_bundles/2).
% Like enum_sub_bundles/2, in reverse order
enumrev_sub_bundles(ParentBundle, Bundle) :-
	( root_bundle(ParentBundle) ->
	    member(Bundle, ~reverse(~bundle_deps(~all_bundles)))
	; fail
	).

% All bundles except ~root_bundle
all_bundles := ~findall(B, ('$bundle_id'(B), \+ root_bundle(B))).

% ---------------------------------------------------------------------------
% Bundle name and version numbers

:- export(bundle_version/2).
% Version number of a bundle (as a atom) (fails if missing)
bundle_version(Bundle) := Version :-
	'$bundle_prop'(Bundle, version(Version)).

:- export(bundle_patch/2).
% Patch number of a bundle (as a atom) (fails if missing)
bundle_patch(Bundle) := Patch :-
	'$bundle_prop'(Bundle, patch(Patch)).

:- export(bundle_version_patch/2).
% Version and patch number
bundle_version_patch(Bundle) := ~atom_concat([~bundle_version(Bundle), '.', ~bundle_patch(Bundle)]).

:- export(bundle_name/2).
% TODO: use (and fix, not always the identity if we want to
% distinguish between the loaded bundle and the built bundle)
bundle_name(Bundle) := Bundle.

% ---------------------------------------------------------------------------
% TODO: Merge with code in autodoc:get_last_version/3
% TODO: customize format (e.g., like in 'git log')

:- use_module(library(format)).

:- export(list_bundles/0).
:- pred list_bundles # "List all registered bundles".
list_bundles :-
	( % (failure-driven loop)
	  '$bundle_id'(Bundle),
	    format("~w\n", [Bundle]),
	    fail
	; true
	).

:- export(bundle_info/1).
:- pred bundle_info(Bundle) # "Show info of @var{Bundle}".
bundle_info(Bundle) :-
	( nonvar(Bundle), '$bundle_id'(Bundle) ->
	    '$bundle_prop'(Bundle, packname(Pack)),
	    ( '$bundle_prop'(Bundle, requires(Requires)) -> true ; Requires = [] ),
	    ( '$bundle_srcdir'(Bundle, SrcDir) -> true ; SrcDir = '(none)' ),
	    Version = ~bundle_version(Bundle),
	    Patch = ~bundle_patch(Bundle),
	    % format("~w (~w ~w.~w) src:~q, requires:~w\n",
	    %        [Bundle, Pack, Version, Patch, SrcDir, Requires]),
	    % (looks like .yaml format)
	    format("~w:\n", [Bundle]),
	    format("  version: ~w.~w\n", [Version, Patch]),
	    format("  name: ~w\n", [Pack]),
	    format("  src: ~w\n", [SrcDir]),
	    format("  requires:\n", []),
	    ( % (failure-driven loop)
	      member(X, Requires),
	        format("  - ~w\n", [X]),
		fail
	    ; true
	    )
	; format("ERROR: unknown bundle ~w\n", [Bundle])
	).

