:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes, datafacts]).

:- doc(title, "Manifest compiler").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements predicates to load, consult,
   and compile @tt{Manifest.pl} files. A Manifest file allows the
   specification of a bundle:

   @begin{itemize}
   @item dependencies to other bundles
   @item configuration flags (see @lib{bundle_configure})
   @item primitive targets (see @lib{grade_bin}, @lib{grade_docs}, etc.)
   @item nested definitions for bundle parts
   @item custom definitions for build commands (see @lib{builder_cmds})
   @end{itemize}

@begin{note}
NOTE: Configuration flags and nested definitions must be defined in a
@tt{<bundle>.hooks.pl} module. 
@end{note}

   Use @pred{make_bundlereg/4} for processing of @tt{Manifest.pl}
   files into bundle registry entries. Bundle registries are written
   using @lib{fastrw} so that the support code to load them is
   minimal. They include cheaper runtime meta-information (like
   dependencies, versions, @concept{path alias}es, etc.).

   For registered bundles, you can use @pred{ensure_load_manifest/1}
   and @pred{manifest_call/2} for loading and consulting a bundle
   specification.

   @begin{note}
   NOTE: Users should never call a custom build command definitions
   directly with @pred{manifest_call/2}. Use @lib{builder_cmds}
   instead.
   @end{note}
").

:- doc(bug, "Simplify reusing the module compiler and some translation
   modules").

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
% TODO: error_messages are ignored; mark erroneous Manifests
:- use_module(library(messages), [error_message/2]).

% ===========================================================================
:- doc(section, "Database").

% loaded_with_hooks(Bundle): Manifest loaded via ensure_load_manifest
:- data loaded_with_hooks/1.

% manifest_fact(Bundle, Fact): Facts of Manifest.pl (not hooks)
:- data manifest_fact/2.

% (module that does dynamic use_module of manifest_hooks)
:- use_module(ciaobld(bundlehooks_holder)).

% ===========================================================================
:- doc(section, "Location of bundles and manifests").

:- use_module(library(pathnames), [path_concat/3, path_split/3, path_is_root/1]).
:- use_module(library(system), [file_exists/1]).

:- export(is_bundledir/1).
:- pred is_bundledir(BundleDir) # "@var{BundleDir} is the directory
   for a bundle (i.e., contains a Manifest.pl file".

is_bundledir(BundleDir) :-
	locate_manifest_file(BundleDir, _).

:- export(locate_manifest_file/2).
:- pred locate_manifest_file(BundleDir, ManifestFile) # "Given
   @var{BundleDir} a bundle directory, @var{ManifestFile} is unified
   with the name of the manifest file".

locate_manifest_file(BundleDir, ManifestFile) :-
	( BundleDir2 = BundleDir
	; path_concat(BundleDir, 'Manifest', BundleDir2)
	),
	path_concat(BundleDir2, 'Manifest.pl', ManifestFile),
	file_exists(ManifestFile),
	!.

:- export(lookup_bundle_root/2).
:- pred lookup_bundle_root(File, BundleDir) # "Detect the bundle root
   dir for the given File (a directory or normal file)".

lookup_bundle_root(File, BundleDir) :-
	fixed_absolute_file_name(File, '.', Path),
	lookup_bundle_root_(Path, BundleDir).

lookup_bundle_root_(File, BundleDir) :-
	is_bundledir(File),
	!,
	% Found a bundle dir
	BundleDir = File.
lookup_bundle_root_(File, BundleDir) :-
	% Not a bundle dir, visit the parent
	\+ path_is_root(File),
	path_split(File, Dir, _),
	lookup_bundle_root_(Dir, BundleDir).

% ===========================================================================
:- doc(section, "Load manifest and hooks").
% NOTE: The bundle Manifest must be already registered.

:- use_module(library(system), [cd/1, working_directory/2, file_exists/1]).
:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(engine(internals), ['$bundle_id'/1]).

:- doc(bug, "ensure_load_manifest/1 requires previous make_bundlereg").
:- doc(bug, "ensure_load_manifest/1 not unloaded; no refcount or module GC").

%:- export(check_known_bundle/1).
:- pred check_known_bundle(Bundle) # "Check that Bundle is registered".
check_known_bundle(Bundle) :-
	( nonvar(Bundle), '$bundle_id'(Bundle) ->
	    true
	; throw(unknown_bundle(Bundle))
	).

:- export(ensure_load_manifest/1).
:- pred ensure_load_manifest(Target) # "Ensure that the manifest for
   @var{Target} is loaded (including Manifest sentences and .hooks.pl
   modules). The bundle must have been scanned before, so that its
   source directory is known.".

ensure_load_manifest(Target) :-
	split_target(Target, Bundle, _Part),
	( loaded_with_hooks(Bundle) ->
	    true
	; assertz_fact(loaded_with_hooks(Bundle)),
	  check_known_bundle(Bundle), % TODO: bundle must have been scanned before
	  BundleDir = ~bundle_path(Bundle, '.'),
	  load_manifest(Bundle, BundleDir),
	  load_manifest_hooks(Bundle, BundleDir)
	).
%	% If target is a bundle part, check that it is a valid part
%       % TODO: we can use item_nested but it is not declared for all items (e.g., static_engine)
%	( _Part = '' -> true
%	; ...
%       ).

% Load manifest_hooks
load_manifest_hooks(Bundle, BundleDir) :-
	HooksFile = ~hooks_file(Bundle, BundleDir),
	( file_exists(HooksFile) ->
	    working_directory(PWD, BundleDir), % TODO: Needed here?
	    once_port_reify(bundlehooks_holder:do_use_module(HooksFile), Port),
	    cd(PWD),
	    port_call(Port)
	; true % (no error if it does not exist)
	).

% TODO: add unload_manifest/1
% TODO: add reference counting or GC
unload_manifest_hooks(Bundle, BundleDir) :-
	HooksFile = ~hooks_file(Bundle, BundleDir),
	bundlehooks_holder:do_unload(HooksFile).

% Module that implements manifest_hooks (.hooks.pl file) for Bundle
hooks_mod(Bundle) := ~atom_concat(Bundle, '.hooks').

% Note: hooks must be defined in a Manifest/<bundle>.hooks.pl file
hooks_file(Bundle, BundleDir) := File :-
	Pl = ~atom_concat(~hooks_mod(Bundle), '.pl'),
	File = ~path_concat(~path_concat(BundleDir, 'Manifest'), Pl).

% ===========================================================================
:- doc(section, "Manifest reader").

:- use_module(library(read), [read/2]).

% TODO: Use standard compiler? (with a package to delimit the language)

% load_manifest(+Bundle, +BundleDir):
%   Load Manifest of Bundle at BundleDir. Assert them in manifest_fact/2.
%   (shows an error message if bundle name does not coincide).
load_manifest(Bundle, BundleDir) :-
	retractall_fact(manifest_fact(Bundle, _)),
	locate_manifest_file(BundleDir, ManifestFile),
	open(ManifestFile, read, S),
	once_port_reify(read_manifest_(S, Bundle), Port),
	close(S),
	port_call(Port),
	% Fill version number
	read_version(BundleDir, Bundle).

% TODO: post process sentences here instead than in make_bundlereg/4?
read_manifest_(S, Bundle) :-
	check_manifest(S, Bundle),
	read_loop(S, Bundle).

% Check that the Manifest is well formed
check_manifest(S, Bundle) :-
	read(S, Sent),
	% Check that this is a manifest file
	( Sent = (:- bundle(Bundle0)) ->
	    % Check that bundle name matches declared
	    ( Bundle0 = Bundle -> % OK
	        true
	    ; error_message("Mismatch in bundle name (expected ~w)", [Bundle])
	    )
	; error_message("Missing ':- bundle/1' declaration on Manifest.pl for ~w", [Bundle])
	).

read_loop(S, Bundle) :-
	read(S, X),
	X \== end_of_file,
	!,
	treat_sentence(X, Bundle),
	read_loop(S, Bundle).
read_loop(_S, _Bundle).

% ---------------------------------------------------------------------------
% Treat manifest sentences

% Sentences allowed in Manifest.pl
manifest_def(version(_)).
manifest_def(alias_paths(_)).
%manifest_def(depends(_,_)). % (normalized as dep/2)
manifest_def(lib(_)). % TODO: bintgt/1?
manifest_def(cmd(_)). % TODO: bintgt/1?
manifest_def(cmd(_,_)). % TODO: bintgt/1?
manifest_def(readme(_,_)). % TODO: docstgt/1?
manifest_def(manual(_,_)). % TODO: docstgt/1?
manifest_def(service(_,_)). % TODO: for service_registry.pl

treat_sentence(Sent, Bundle) :- var(Sent), !,
	error_message("Unbound variable is not a valid sentence at Manifest.pl for ~w", [Bundle]).
treat_sentence(depends(Depends), Bundle) :- !,
	( % (failure-driven loop)
	  member(DepProps, Depends),
	    normalize_depprop(DepProps, Dep, Props),
	    assertz_fact(manifest_fact(Bundle, dep(Dep, Props))),
	    fail
	; true
	).
treat_sentence(Sent, Bundle) :- manifest_def(Sent), !,
	assertz_fact(manifest_fact(Bundle, Sent)).
treat_sentence(Sent, Bundle) :-
	error_message("Unknown sentence ~w at Manifest.pl for ~w", [Sent, Bundle]).

% ---------------------------------------------------------------------------
% Normalize a dependency:
%  - add properties (possibly empty)
%  - add origin=Origin to properties if Dep is a bundle_alias

:- use_module(library(aggregates), [findall/3]).
:- use_module(ciaobld(bundle_fetch), [check_bundle_alias/3]).

normalize_depprop(DepProps, Dep2, Props2) :-
	( DepProps = Dep-Props -> true
	; Dep = DepProps, Props = []
	),
	( check_bundle_alias(Dep, DepOrigin, Dep2) ->
	    Props2 = [origin=DepOrigin|Props]
	; Dep2 = Dep,
	  Props2 = Props
	).

% ---------------------------------------------------------------------------
% Load bundle version from GlobalVersion,GlobalPath files if not
% provided in Manifest

:- use_module(library(system_extra), [file_to_line/2]).
:- use_module(library(version_strings), [version_split_patch/3]).

% Make sure that version/1 is defined
read_version(_BundleDir, Bundle) :-
	manifest_fact(Bundle, version(_)),
	% TODO: check that it is well formed?
	!.
read_version(BundleDir, Bundle) :-
	% TODO: Deprecate
        path_concat(BundleDir, 'Manifest/GlobalVersion', FileV),
        path_concat(BundleDir, 'Manifest/GlobalPatch', FileP),
	( file_to_atom(FileV, VerNopatch),
	  file_to_atom(FileP, Patch) ->
	    Ver = ~atom_concat([VerNopatch, '.', Patch]),
	    assertz_fact(manifest_fact(Bundle, version(Ver)))
	; true
	).

file_to_atom(File, X) :-
	file_exists(File),
	file_to_line(File, Str),
	atom_codes(X, Str).

% ===========================================================================
:- doc(section, "Call predicates from manifests").

:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(terms), [atom_concat/2]).

:- include(ciaobld(bundlehooks/bundlehooks_defs)).

:- export(manifest_call/2).
:- pred manifest_call(Target, Head) # "Call @tt{Target:Head}, a
   predicate defined in a Manifest (fact, predicate in hooks, or
   nested predicate)".

manifest_call(_Target, Head) :- var(Head), !,
	throw(error(uninstantiated, manifest_call/2)).
manifest_call(Target, Head) :-
	split_target(Target, Bundle, Part),
	Mod = ~hooks_mod(Bundle),
	( % (nondet)
	  ( m_bundlehook_decl(Mod, Part, Head) ->
	      m_bundlehook_do(Mod, Part, Head)
	  ; fail
	  )
	; % (enumerate also defs from Manifest if Part = '')
	  ( Part = '' -> manifest_fact(Bundle, Head) ; fail )
	).

:- export(manifest_current_predicate/2).
:- pred manifest_current_predicate(Target, Head) # "@var{Head} is
   declared in @var{Target}".

manifest_current_predicate(_Target, Head) :- var(Head), !,
	throw(error(uninstantiated, manifest_current_predicate/2)).
manifest_current_predicate(Target, Head) :-
	split_target(Target, Bundle, Part),
	Mod = ~hooks_mod(Bundle),
	m_bundlehook_decl(Mod, Part, Head).

:- export(split_target/3).
% decompose target names
% E.g., 'core.engine' -> ('core','engine')
split_target(Target, Bundle, Part) :-
	( atom_codes(Target, Codes),
	  append(Bundle0, "."||Part0, Codes) ->
	    atom_codes(Bundle, Bundle0),
	    atom_codes(Part, Part0)
	; Bundle = Target, Part = ''
	).

:- export(compose_target/3).
% compose target names
% E.g., ('core','engine') -> 'core.engine'
compose_target(Bundle, '', Target) :- !, Target = Bundle.
compose_target(Bundle, Part, Target) :-
	atom_concat([Bundle, '.', Part], Target).

:- export(target_is_bundle/1).
% (the bundle, not a part)
target_is_bundle(Target) :-
	split_target(Target, _Bundle, Part),
	Part = ''.

% ===========================================================================
:- doc(section, "Manifest to bundlereg compiler").

:- use_module(library(fastrw), [fast_write/1]).
:- use_module(engine(internals), [bundlereg_version/1]).
:- use_module(library(stream_utils), [open_output/2, close_output/1]).

:- export(make_bundlereg/4).
:- pred make_bundlereg(Bundle, BundleDir, FinalBundleDir, RegFile)
   # "Generate a bundlereg @var{RegFile} for bundle @var{Bundle}
      at @var{BundleDir}, where the final bundle directory is 
      @var{FinalBundleDir}.".

% TODO: code in a similar way to itf_data
% TODO: use relative alias_path? process them in treat_sentence
make_bundlereg(Bundle, BundleDir, FinalBundleDir, RegFile) :-
	load_manifest(Bundle, BundleDir),
	%
	( manifest_fact(Bundle, alias_paths(RelAliasPaths)) ->
	    true
	; RelAliasPaths = []
	),
	%
	abs_alias_paths(RelAliasPaths, FinalBundleDir, AliasPaths),
	%
	open_output(RegFile, UO),
	%
	% Version of bundlereg
	bundlereg_version(V),
	fast_write(bundlereg_version(V)),
	fast_write(bundle_id(Bundle)),
        % Alias paths
	( % (failure-driven loop)
	  member(AliasPath, AliasPaths),
	    write_alias_path(AliasPath, Bundle),
	    fail
	; true
	),
	% Dependencies
	( % (failure-driven loop)
	  manifest_fact(Bundle, dep(Dep, _)),
	    % NOTE: bundle dependency properties are ignored in bundlereg
	    % TODO: Use depends in bundlereg loading to ensure that
	    %   all requirements are loaded
	    fast_write(bundle_prop(Bundle, dep(Dep))),
	    fail
	; true
	),
	% Version
	( manifest_fact(Bundle, version(Ver)) ->
	    fast_write(bundle_prop(Bundle, version(Ver)))
	; true
	),
	% Source dir
	fast_write(bundle_srcdir(Bundle, FinalBundleDir)),
	%
	close_output(UO).

write_alias_path(AliasName=AliasPath, Bundle) :- !,
	fast_write(bundle_alias_path(AliasName, Bundle, AliasPath)).
write_alias_path(AliasPath, Bundle) :-
	fast_write(bundle_alias_path(library, Bundle, AliasPath)).

% Compute absolute paths names for path aliases (add @var{Base} to the
% beginning of each relative path alias).
abs_alias_paths([], _, []).
abs_alias_paths([RelAliasPath|RelAliasPaths], Base, [AliasPath|AliasPaths]) :-
	abs_alias_path(RelAliasPath, Base, AliasPath),
	abs_alias_paths(RelAliasPaths, Base, AliasPaths).

abs_alias_path(X, Base, Y) :-
	( X = (AliasName=RelAliasPath) -> Y = (AliasName=AliasPath)
	; X = RelAliasPath, Y = AliasPath
	),
	( RelAliasPath = '.' ->
	    AliasPath = Base
	; path_concat(Base, RelAliasPath, AliasPath)
	).

% ===========================================================================
:- doc(section, "Display bundle info").

:- use_module(engine(internals), ['$bundle_prop'/2, '$bundle_srcdir'/2]).
:- use_module(library(bundle/bundle_info), [bundle_status/2]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).
:- use_module(library(format)).

% TODO: Merge with code in autodoc:get_last_version/3
% TODO: customize format (e.g., like in 'git log')

:- export(bundle_info/1).
:- pred bundle_info(Bundle) # "Show info of @var{Bundle}".
bundle_info(Bundle) :-
	check_known_bundle(Bundle),
	( '$bundle_srcdir'(Bundle, SrcDir) -> true ; SrcDir = '(none)' ),
	% (looks like .yaml format)
	format("~w:\n", [Bundle]),
	( Version = ~bundle_version(Bundle) ->
	    format("  version: ~w\n", [Version])
	; true
	),
	Status = ~bundle_status(Bundle),
	format("  src: ~w\n", [SrcDir]),
	format("  status: ~w\n", [Status]), % TODO: status is not a good name
	( manifest_call(Bundle, dep(_, _)) -> % some dependencies
	    format("  depends:\n", []),
	    ( % (failure-driven loop)
	      manifest_call(Bundle, dep(Dep, Props)),
	        format("  - ~w", [Dep]),
		( Props = [] -> true
		; format(" ~w", [Props])
		),
		nl,
		fail
	    ; true
	    )
	; true
	),
	( manifest_call(Bundle, item_nested(_)) -> % some nested
	    format("  parts:\n", []),
	    ( % (failure-driven loop)
		manifest_call(Bundle, item_nested(X)),
		format("  - ~w\n", [X]),
		fail
	    ; true
	    )
	; true
	).

% ===========================================================================
:- doc(section, "Enumerate bundle documentation").

% TODO: some duplicated/reexported in autodoc_lookup.pl

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

:- export(get_bundle_readme/2).
% Output for bundle README files
get_bundle_readme(Bundle, R) :-
	manifest_call(Bundle, readme(OutName, _Props)), % (nondet)
	R = ~bundle_path(Bundle, OutName).

:- export(bundle_manual_base/2).
% Base name for manuals of Bundle
bundle_manual_base(Bundle) := R :-
	manifest_call(Bundle, manual(Base, _Props)), % (nondet)
	R = Base.

:- export(main_file_relpath/2).
% Relative path of the main file specified in some Props
% (for manuals, readme, cmds, etc. entries in manifests)
main_file_relpath(Props) := R :-
	( member(main=SrcPath, Props) -> R = SrcPath
	; fail
	).

:- export(main_file_path/3).
% Absolute path of the main file specified in some Props
% (for manuals, readme, cmds, etc. entries in manifests)
main_file_path(Bundle, Props) := R :-
	R = ~bundle_path(Bundle, ~main_file_relpath(Props)).




