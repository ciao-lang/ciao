:- module(bundle, [], [assertions, fsyntax, dcg]).

:- doc(title, "Dynamic bundle loading").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(bug, "Complete documentation, change name of module?").
:- doc(bug, "Merge, make it optional with a Prolog flag?").
:- doc(bug, "Bundle unload? Bundle reload?").
:- doc(bug, "Control visibility of bundles?").

:- doc(module, "A bundle is a collection of modules together with
   meta-information about namespaces (such as alias paths). This
   module defines operations to load bundle Manifests dynamically, as
   well as bundle-aware wrappers of module loading code.

   For example, assume that a Manifest (@tt{Manifest.pl} or
   @tt{Manifest/Manifest.pl) exists in any parent directroy of file
   @tt{baz.pl}. Then, the following will automatically load the
   manifest definitions:

@begin{verbatim}
?- bundle:use_module('foo/bar/baz').
@end{verbatim}
").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).

:- use_module(library(compiler), [use_module/3]).

:- use_module(engine(internals), [load_bundlereg/1, '$bundle_id'/1]).
:- use_module(library(bundle/bundlereg_gen),
	[is_bundle_dir/1,
	 lookup_bundle_root/2,
	 gen_bundlereg/4]).

% ---------------------------------------------------------------------------
% Loading of bundles (just enable namespaces, does not load any module)

:- export(use_bundle/1).
:- export(use_bundle/2).
:- meta_predicate use_bundle(addmodule).
% Use a bundle (from the toplevel)
use_bundle(BundleDir, _This) :-
	ensure_bundle_manifest(BundleDir, _).

% ---------------------------------------------------------------------------
% Loading of modules with automatic bundle load

:- export(use_module/1).
:- export(use_module/2).
:- meta_predicate use_module(addmodule).

use_module(Mod,This) :- bundle:use_module(Mod,all,This).

:- export(use_module/3).
:- meta_predicate use_module(+,addmodule).

% TODO: optimize and remove overheads
use_module(Mod, Imports, This) :-
	% If Mod is a file, load its bundle automatically
	atom(Mod),
	lookup_bundle_root(Mod, BundleDir),
	!,
	use_bundle(BundleDir),
	compiler:use_module(Mod, Imports, This).
use_module(Mod, Imports, This) :-
	compiler:use_module(Mod, Imports, This).

% ---------------------------------------------------------------------------

% Default directory for the bundle state directory at the bundle root
bundle_state_dir('.ciao').

% TODO: memoize
bundle_state_base(BundleDir) := D :-
	BundleStateDir = ~bundle_state_dir,
	path_concat(BundleDir, BundleStateDir, D).

bundlereg_file(BundleDir, BundleName) := F :-
	D = ~bundle_state_base(BundleDir),
	atom_concat(BundleName, '.bundlereg', M),
	path_concat(D, M, F).

% ---------------------------------------------------------------------------

% Create the bundle state base (if it does not exists)
ensure_bundle_state_dir(BundleDir) :-
	D = ~bundle_state_base(BundleDir),
	mkpath(D).

% ---------------------------------------------------------------------------

% bundle_name(BundleDir, BundleName)
:- data bundle_name/2.

:- export(ensure_bundle_manifest/2).
% Ensure that the manifest for BundleDir has been loaded. BundleName
% is unified with the unique bundle id.
ensure_bundle_manifest(BundleDir, BundleName) :-
	bundle_name(BundleDir, BundleName0),
	!,
	BundleName = BundleName0.
ensure_bundle_manifest(BundleDir, BundleName) :-
	path_split(BundleDir, _, BundleName0),
	( '$bundle_id'(BundleName0) ->
	    true % bundle already loaded % TODO: does not check that BundleDir was the same
	; load_bundle_manifest(BundleDir, BundleName0),
	  % associate BundleDir with this BundleName
	  assertz_fact(bundle_name(BundleDir, BundleName0))
	),
	BundleName = BundleName0.

load_bundle_manifest(BundleDir, BundleName) :-
	bundlereg_file(BundleDir, BundleName, F),
%	message(['Reading and loading manifest for bundle \'', BundleName, '\'']),
	AliasBase = BundleDir,
	gen_bundlereg(BundleDir, BundleName, AliasBase, F),
	load_bundlereg(F),
	% TODO: BundleName extracted from path and Manifest.pl should be the same
	( '$bundle_id'(BundleName) ->
	    true
	; throw(error(['Bundle \'', BundleName, '\' has not been registered properly']))
	).

