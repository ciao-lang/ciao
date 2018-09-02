:- module(bundle_scan, [], [assertions, fsyntax, datafacts]).

:- doc(title, "Scanning and registering of bundles").
:- doc(author, "Jose F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(module, "Scanning source for bundles and processing of
   @tt{Manifest.pl} files.

   A registered bundle can be viewed as the result of the compilation
   or processing of a @tt{Manifest.pl} (it should contain plain facts
   and no syntactic sugar).
").

:- use_module(library(lists), [member/2]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system_extra), [mkpath/1, del_file_nofail/1]).
:- use_module(library(system), [directory_files/2, file_exists/1, delete_file/1]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(port_reify)).

% NOTE: be careful with bundle_path/3 (bundles may not be loaded yet)
:- use_module(engine(internals), [get_bundlereg_dir/2]).

% ---------------------------------------------------------------------------
:- doc(section, "Scan bundles at given workspace").

:- use_module(engine(internals), [reload_bundleregs/0]).

% Note: scanning bundles must be done before configuration
% Note: bundle_path/? cannot be used until bundles are scanned (may
%   fail or report outdated data)

:- export(scan_bundles_at_path/1).
:- pred scan_bundles_at_path(Path) # "Update the bundle registry for
   the given workspace at @var{Path} directory (and reload
   bundleregs).".

% TODO: Allow a single bundle (use BundleSet?)
scan_bundles_at_path(Path) :-
	% Find bundles under Path and scan
	find_bundles(Path),
	once_port_reify(scan_bundles_at_path_(Path), Port),
	cleanup_find_bundles,
	port_call(Port).

% TODO: Assumes that Path corresponds to some valid workspace
scan_bundles_at_path_(Path) :- % (requires find_bundles/2 data)
	% Create bundleregs
	ensure_bundlereg_dir(Path),
	( % (failure-driven loop)
	  found_bundle(_Name, BundleDir),
	    create_bundlereg(BundleDir, Path),
	    fail
	; true
	),
	% Remove orphan bundleregs (including configuration)
	swipe_bundlereg_dir(Path),
	% Finally reload bundleregs
	reload_bundleregs.

swipe_bundlereg_dir(Wksp) :-
	get_bundlereg_dir(Wksp, BundleRegDir),
	directory_files(BundleRegDir, Files),
	( member(File, Files),
	    ( orphan_reg_file(File) ->
	        path_concat(BundleRegDir, File, AbsFile),
	        delete_file(AbsFile)
	    ; true
	    ),
	    fail
	; true
	).

% A .bundlereg, .bundlecfg, or .bundlecfg_sh file for a bundle that is
% no longer available.
orphan_reg_file(File) :-
	( atom_concat(Name, '.bundlereg', File) -> true
	; atom_concat(Name, '.bundlecfg', File) -> true
	; atom_concat(Name, '.bundlecfg_sh', File) -> true
	),
	\+ found_bundle(Name, _).

% Ensure that build/bundlereg exists in for Wksp workspace
ensure_bundlereg_dir(Wksp) :-
	get_bundlereg_dir(Wksp, BundleRegDir),
	mkpath(BundleRegDir).

% ---------------------------------------------------------------------------

:- use_module(ciaobld(manifest_compiler), [is_bundledir/1]).

% found_bundle(Name,Dir): found bundle Name at Dir
:- data found_bundle/2.

% Find bundles at @var{Path} workspace using @pred{bundledirs_at_dir/3}.
% Store them at @pred{found_bundle/2} data. Use @pred{cleanup_find_bundles/0}
% when done.

find_bundles(Path) :-
	cleanup_find_bundles,
	( % (failure-driven loop)
	  bundledirs_at_dir(Path, no, Dir),
	    bundledir_to_name(Dir, Bundle),
	    assertz_fact(found_bundle(Bundle, Dir)),
	    fail
	; true
	).

cleanup_find_bundles :- retractall_fact(found_bundle(_, _)).

% Enumerate of all bundle directories (absolute path) under @var{Src}.
% If @var{Optional} is @tt{yes}, bundles require a @tt{ACTIVATE}
% directory mark to be enabled.
%
% This search is non-recursive by default. If the directory contains a
% file called BUNDLE_CATALOG, search goes into that directory. Bundles
% in a BUNDLE_CATALOG are only recognized if they contain a file
% called ACTIVATE.
%
% (nondet)
bundledirs_at_dir(Src, Optional, BundleDir) :-
	\+ is_bundledir(Src), % (it cannot be a bundle)
	directory_files(Src, Files),
	member(File, Files),
	\+ not_bundle(File),
	path_concat(Src, File, Dir),
	\+ directory_has_mark(nocompile, Dir), % TODO: not needed now?
	%
	( directory_has_mark(bundle_catalog, Dir) ->
	    % search recursively on the catalog (only if ACTIVATE is set on the bundle)
	    bundledirs_at_dir(Dir, yes, BundleDir)
	; is_bundledir(Dir) -> % a bundle
	    ( Optional = yes -> directory_has_mark(activate, Dir) ; true ),
	    BundleDir = Dir
	; fail % (none, backtrack)
	).

not_bundle('.').
not_bundle('..').

directory_has_mark(nocompile, Dir) :-
	path_concat(Dir, 'NOCOMPILE', F),
	file_exists(F).
directory_has_mark(bundle_catalog, Dir) :-
	path_concat(Dir, 'BUNDLE_CATALOG', F),
	file_exists(F).
directory_has_mark(activate, Dir) :-
	path_concat(Dir, 'ACTIVATE', F),
	file_exists(F).

% ---------------------------------------------------------------------------

:- doc(section, "Create/destroy bundleregs").
% A bundlereg contains the processed manifest information and resolved
% paths.

:- use_module(library(pathnames), [path_split/3]).
:- use_module(ciaobld(manifest_compiler), [make_bundlereg/4]).
:- use_module(engine(internals), [bundlereg_filename/3]).

% TODO: extract name from Manifest?
bundledir_to_name(BundleDir, Bundle) :-
	path_split(BundleDir, _, Bundle).

:- export(create_bundlereg/2).
% Create a bundlereg for bundle at @var{BundleDir} (for workspace Wksp)
create_bundlereg(BundleDir, Wksp) :-
	bundledir_to_name(BundleDir, Bundle), % bundle name from path
	get_bundlereg_dir(Wksp, BundleRegDir),
	bundlereg_filename(Bundle, BundleRegDir, RegFile),
	make_bundlereg(Bundle, BundleDir, BundleDir, RegFile).

:- export(remove_bundlereg/2).
% Remove the bundlereg for bundle @var{Bundle} (for workspace Wksp)
remove_bundlereg(Bundle, Wksp) :-
	get_bundlereg_dir(Wksp, BundleRegDir),
	bundlereg_filename(Bundle, BundleRegDir, RegFile),
	del_file_nofail(RegFile).

