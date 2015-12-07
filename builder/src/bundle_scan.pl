:- module(bundle_scan, [], [assertions, fsyntax]).

:- doc(title, "Scanning and Registering of Bundles").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Scanning source for bundles and processing of
   @tt{Manifest.pl} files.

   A registered bundle can be viewed as the result of the compilation
   or processing of a @tt{Manifest.pl} (it should contain plain facts
   and no syntactic sugar).

@begin{alert}
  Make sure that this code does not end in executables unless
  necessary (it should not be in the dependencies of usual user
  programs).
@end{alert}").

:- use_module(library(system)).

% TODO: be careful with fsR/2 (bundles may not be loaded yet)
:- use_module(ciaobld(config_common), [instciao_bundledir/2]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(bundle/bundlereg_gen), [is_bundle_dir/1, gen_bundlereg/4]).
:- use_module(engine(internals), [current_bundle_reg_dir/1]).

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system_extra)).

% ----------------------------------------------------------------------------

:- export(bundle_scan/1).
:- pred bundle_scan(Src) # "Scan all the bundles under the @var{Src}
   directory and annotate the results in the bundle registry (creating
   it if missing)".

bundle_scan(Src) :-
	bundledirs_at_dir(Src, BundleDirs),
	%
	ensure_current_bundle_reg_dir,
	local_bundleregs(BundleDirs).

local_bundleregs([]).
local_bundleregs([BundleDir|BundleDirs]) :-
	create_bundlereg(BundleDir, local),
	local_bundleregs(BundleDirs).

% Make sure that the directory for the bundle database exists
ensure_current_bundle_reg_dir :-
	current_bundle_reg_dir(BundleRegDir),
	mkpath(BundleRegDir),
 	mark_directory(noinstall, BundleRegDir).

% ---------------------------------------------------------------------------

:- use_module(engine(system_info), [ciao_lib_dir/1]).
:- use_module(engine(internals), [bundle_reg_dir/2, bundlereg_filename/3]).
:- use_module(engine(internals), [bundlereg_version/1]).
:- use_module(ciaobld(builder_aux), [rootprefixed/2]).

% Like current_bundle_reg_dir/1, but for the given installation type
% InsType and prefixed with rootprefix if needed.

rootprefix_bundle_reg_dir(InsType, BundleRegDir) :-
	rootprefix_ciao_lib_dir(InsType, BundleRegDirBase),
	bundle_reg_dir(BundleRegDirBase, BundleRegDir).

% TODO: see get_defaultlibdir in ciao.config.pl
rootprefix_ciao_lib_dir(InsType, Dir) :-
	( InsType = local ->
	    ciao_lib_dir(Dir)
	; InsType = global ->
	    instciao_bundledir(core, Dir0),
	    Dir = ~rootprefixed(Dir0)
	; fail
	).

% File is the registry file for the BundleName bundle
rootprefix_bundle_reg_file(InsType, BundleName, RegFile) :-
	rootprefix_bundle_reg_dir(InsType, BundleRegDir),
	bundlereg_filename(BundleName, BundleRegDir, RegFile).

% ---------------------------------------------------------------------------

% List of all bundle directories (absolute path) under @var{Src} (non recursively)
%:- export(bundledirs_at_dir/2).
bundledirs_at_dir(Src, BundleDirs) :-
	directory_files(Src, Files),
	find_bundles(Files, Src, BundleDirs).

find_bundles([],           _,   []).
find_bundles([File|Files], Src, [BundleDir|BundleDirs]) :-
	nonvar(File),
	\+ File = '..',
	\+ File = 'Manifest', % not the manifest directory
	( File = '.' ->
	    C1 = Src
	; path_concat(Src, File, C1)
	),
	\+ directory_has_mark(nocompile, C1),
	is_bundle_dir(C1),
	!,
	BundleDir = C1,
	find_bundles(Files, Src, BundleDirs).
find_bundles([_File|Files], Src, BundleDirs) :-
	find_bundles(Files, Src, BundleDirs).

% ---------------------------------------------------------------------------

% BundleDir for bundle registry (depends on InsType)
reg_bundledir(InsType, BundleName, BundleDir, Dir) :-
	( InsType = local -> Dir = BundleDir
	; InsType = global -> instciao_bundledir(BundleName, Dir)
	; fail
	).

% ---------------------------------------------------------------------------
% Directory marks

% TODO: Complete missing cases, implement unmark?

% TODO: Directory marks (move to a module)
mark_directory(noinstall, Dir) :-
 	% creates empty NOINSTALL file (which marks that this
 	% directory will not be installed)
 	path_concat(Dir, 'NOINSTALL', F),
 	touch(F).

directory_has_mark(nocompile, Dir) :-
	path_concat(Dir, 'NOCOMPILE', F),
	file_exists(F).

% ---------------------------------------------------------------------------

:- doc(section, "Create/destroy bundleregs").
% A bundlereg contains the processed manifest information and resolved
% paths.

% TODO: allow relative paths? (with some relocation rules)

:- use_module(library(pathnames), [path_split/3]).
:- use_module(library(bundle/bundle_info), [root_bundle/1]).
:- use_module(ciaobld(builder_aux), [root_bundle_source_dir/1]).

% TODO: hack, try to extract BundleName from Manifest, not dir (also in bundle.pl)
bundle_dir_to_name(BundleDir, BundleName) :-
	root_bundle_source_dir(RootDir),
	( BundleDir = RootDir ->
	    root_bundle(BundleName)
	; path_split(BundleDir, _, BundleName)
	).

:- export(create_bundlereg/2).
% Create a bundlereg for bundle at @var{BundleDir} and installation
% type @var{InsType} (which determines the location of the bundle
% registry and the absolute directories for alias paths).
create_bundlereg(BundleDir, InsType) :-
	% Obtain bundle name from path
	bundle_dir_to_name(BundleDir, BundleName),
	reg_bundledir(InsType, BundleName, BundleDir, AliasBase),
	rootprefix_bundle_reg_file(InsType, BundleName, RegFile),
	gen_bundlereg(BundleDir, BundleName, AliasBase, RegFile).

:- export(remove_bundlereg/2).
% Remove the bundlereg for bundle @var{BundleName} and installation
% type @var{InsType} (which determines the location of the bundle
% registry).
remove_bundlereg(BundleName, InsType) :-
	rootprefix_bundle_reg_file(InsType, BundleName, RegFile),
	my_del_file_nofail(RegFile).

% TODO: merge with system_extra:del_file_nofail?
my_del_file_nofail(FileName) :-
	( file_exists(FileName) -> delete_file(FileName) ; true ).

:- export(ensure_global_bundle_reg_dir/0).
% Make sure that the directory for the (global installation) bundle
% database exists
ensure_global_bundle_reg_dir :-
	rootprefix_bundle_reg_dir(global, BundleRegDir),
	mkpath(BundleRegDir).



