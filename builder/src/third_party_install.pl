:- module(third_party_install, [], [assertions]).

:- doc(title, "Installation of third-party components").
:- doc(author, "Remy Haemmerle").
:- doc(author, "Jose F. Morales (minor changes)").

:- doc(module, "This module implements automatic fetch and build and
   installation of third-party components, without interfering with
   the system installed software.

   See @href{http://brew.sh/}{Homebrew}, or
   @href{https://www.gnu.org/software/guix/manual/guix.html}{GNU Guix}
   for related systems.

   The supported build systems are:

   @begin{description}
   @item{@tt{gnu_build_system}} See @href{http://www.gnu.org/prep/standards/standards.html#Configuration}
   @end{description}

   @section{Specification of third-party components}

   @begin{alert}TO BE DONE@end{alert}").

:- doc(bug, "Write documentation").
:- doc(bug, "Relation between third_party, bundleitem, and bundle?").
:- doc(bug, "Implement more build systems (see GNU Guix manual)").
:- doc(bug, "Easy import packages from other systems?").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [append/3, difference/3]).

:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(library(http_get), [http_get/2]).
:- use_module(library(md5sum), [md5sum/2]).
:- use_module(library(pathnames),
	[path_concat/3, path_split/3, path_splitext/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(system),
	[file_exists/1, directory_files/2, file_property/2, delete_file/1, getenvstr/2]).

:- use_module(engine(system_info), [get_arch/1]).

% ============================================================================

:- doc(section, "Default options").

:- include(ciaobld(bundlehooks/bundlehooks_defs)).

%env('CC', gcc).
%env('CXX', 'g++').
env('CFLAGS', '-m32') :- get_arch(Arch), member(Arch, ['i686']).
env('CXXFLAGS', Flag) :- env('CFLAGS', Flag).
env('CPPFLAGS', Flag) :-
	third_party_path(includedir, Path),
	atom_concat(['-I', Path], Flag).
env('LDFLAGS', Flag) :-
	third_party_path(libdir, Path),
	atom_concat(['-L', Path], Flag).
env('LD_LIBRARY_PATH', Path) :-
	third_party_path(libdir, Path).
env('PATH', Paths):-
	getenvstr('PATH', Paths1),
	third_party_path(bindir, Path),
	atom_codes(Path, PathStr),
	append(PathStr, ":"|| Paths1, Paths2),
	atom_codes(Paths, Paths2).

option1(Lib, Opt) :- m_third_party_option1(Lib, Opt).

option2(Lib, Opt, Val) :- m_third_party_option2(Lib, Opt, Val).
% TODO: check host triple for 64-bit mode
option2(_Lib, host, 'i386-apple-darwin') :-   get_os('DARWIN'), get_arch('i686').
option2(_Lib, host, 'x86_64-apple-darwin') :- get_os('DARWIN'), get_arch('x86_64').
option2(_Lib, host, 'i386-pc-linux') :-   get_os('LINUX'), get_arch('i686').
option2(_Lib, host, 'x86_64-pc-linux') :- get_os('LINUX'), get_arch('x86_64').
option2(Lib, prefix, Prefix) :- third_party_path(private(Lib, storedir), Prefix).

:- doc(section, "Paths").

third_party_name_instance(Lib, Id) :-
	m_third_party_version(Lib, Version),
	atom_concat([Lib, -, Version], Id).

source_tar(Lib, File) :-
	( m_third_party_source_url(Lib, tar(URL)) ->
	    path_split(URL, _Dir, File)
	; m_third_party_source_url(Lib, git(_URL, Branch)) ->
	    atom_concat([Lib, '-', Branch, '.tgz'], File)
	; throw(error(bad_third_party_source_url(Lib), third_pary_install:source_tar/2))
	).

% level1(DirId, Base): Paths in level-1 (in third-party prefix)
level1(bindir,     'bin').
level1(includedir, 'include').
level1(libdir,     'lib').
level1(srcdir,     'src').
%
level1(storedir,   'store').
level1(cachedir,   'cache').

% level2(DirId): Paths in level-2 (+ instance name)
level2(srcdir).
level2(storedir).

% level3(DirId, DirId2): Level-3 paths (Level-2 + subdir)
level3(bindir, storedir).
level3(includedir, storedir).
level3(libdir, storedir).

:- export(third_party_path/2).
% Level-0 absolute paths (prefix)
third_party_path(prefix, Prefix) :- !,
	fsR(bundle_src(ciao),  CiaoSrc),
	% TODO: duplicated in source_tree.pl
	path_concat(CiaoSrc, 'third-party', Prefix).
%
% Level-1 absolute paths
third_party_path(DirId, Path) :- level1(DirId, Base), !,
	third_party_path(prefix, Prefix),
	path_concat(Prefix, Base, Path).
%
% Level-2 absolute paths (contains instance name)
third_party_path(private(Lib, DirId), Path) :- level2(DirId), !,
	third_party_path(DirId, Dir),
	third_party_name_instance(Lib, Instance),
	path_concat(Dir, Instance, Path).
third_party_path(cachetar(Lib), Path) :- !, % tarball path
	third_party_path(cachedir, Dir),
	source_tar(Lib, Source),
	path_concat(Dir, Source, Path).
%
% Level-3 absolute paths (subdirs of level-2)
third_party_path(private(Lib, DirId), Path) :- level3(DirId, DirId2), !,
	third_party_path(private(Lib, DirId2), Prefix),
	level1(DirId, Base),
	path_concat(Prefix, Base, Path).
third_party_path(real_sourcedir(Lib), Path) :- !,
	% (get unique dir inside private(Lib, srcdir)),
	m_third_party_source_url(Lib, TypedURL),
	( member(TypedURL, [tar(_)]) ->
	    third_party_path(private(Lib, srcdir), PrivateSrcDir),
	    directory_files(PrivateSrcDir, List),
	    difference(List, ['..', '.'], [SourceDir]),
	    path_concat(PrivateSrcDir, SourceDir, Path)
	; member(TypedURL, [git(_, _)]) ->
	    third_party_path(private(Lib, srcdir), Path)
	).

% Relative path to level-3 from level-1 path
%   ../store/INSTANCE/...
third_party_relpath31(private(Lib, DirId), Path) :- level3(DirId, DirId2), !,
	third_party_name_instance(Lib, Instance),
	level1(DirId2, Base2),
	path_concat(Base2, Instance, RelPrefix),
	path_concat('..', RelPrefix, Path_),
	level1(DirId, Base),
	path_concat(Path_, Base, Path).

:- doc(section, "Aggregates").

get_env(Env) :-
	findall(Var=Value, env(Var, Value), Env).
get_config_options(Lib, Options) :-
	findall(Option, (
		  option2(Lib, Flag, Value),
		  atom_concat(['--', Flag, '=', Value], Option);
		  option1(Lib, Flag), atom_concat(['--', Flag], Option)
			), Options).

:- doc(section, "Commands").

make(Lib, List) :-
	third_party_path(real_sourcedir(Lib), SrcDir),
	get_env(Env),
	process_call(path(make), List, [ cwd(SrcDir), env(Env), status(0) ]).

mkdir(Dir) :-
	process_call(path(mkdir), ['-p', Dir], [status(0)]).

link_all_files(DirDest, DirSrc, RelativePath) :-
	file_exists(DirSrc),
	mkdir(DirDest),
	directory_files(DirSrc, Files),
	member(File, Files), \+ member(File, ['.', '..']),
	path_concat(RelativePath, File, FileSrc),
	( ln(FileSrc, DirDest) -> true ; ! ),
	fail.
link_all_files(_, _, _).

ln(FileSrc, DirDest) :-
	process_call(path(ln), ['-s', FileSrc], [cwd(DirDest), status(0)]).

unlink_all_files(DirDest, DirSrc, RelativePath) :-
	file_exists(DirSrc),
	directory_files(DirSrc, Files),
	member(File, Files), \+ member(File, ['.', '..']),
	path_concat(DirDest, File, FileDest),
	path_concat(RelativePath, File, FileSrc),
	(
	    file_exists(FileDest),
	    file_property(FileDest, linkto(FileSrc_)),
	    FileSrc_ = FileSrc,
	    delete_file(FileDest) -> true
	;
	    !
	),
	fail.
unlink_all_files(_, _, _).

:- export(clean/1).
clean(Lib) :-
	delete_cachetar(Lib),
	source_clean(Lib).

delete_cachetar(Lib) :-
	Operation = "delete tarball",
	message_start(Lib, Operation),
	third_party_path(cachetar(Lib), Tar),
	(file_exists(Tar); delete_file(Tar)),
	message_end(Lib, Operation), !.

source_clean(Lib) :-
	Operation = "clean sources",
	message_start(Lib, Operation),
	third_party_path(private(Lib, srcdir), Dir),
	rec_delete_file_nofail(Dir),
	message_end(Lib, Operation),!.

:- export(download/1).
download(Lib) :-
	third_party_path(cachetar(Lib), Tar),
	file_exists(Tar), !.
download(Lib) :-
	Operation = "download tarball",
	message_start(Lib, Operation),
	third_party_path(cachedir, CacheDir), mkdir(CacheDir),
	third_party_path(cachetar(Lib), TarPath),
	( m_third_party_source_url(Lib, tar(URL)) ->
	    http_get(URL, file(TarPath))
	; m_third_party_source_url(Lib, git(URL, Ref)) ->
	    process_call(path(git),
	                 ['archive', '--format', 'tgz', '--remote', URL, '--output', TarPath, Ref],
			 [status(0)])
	; throw(error(bad_third_party_source_url(Lib), third_pary_install:download/1))
	),
	message_end(Lib, Operation),!.

:- export(decompress_and_patch/1).
decompress_and_patch(Lib) :-
	Operation = "decompress tarball",
	message_start(Lib, Operation),
	third_party_path(private(Lib, srcdir), SrcDir), mkdir(SrcDir),
	third_party_path(cachetar(Lib), TarPath),
	process_call(path(tar), ['-x', '-f', TarPath], [cwd(SrcDir), status(0)]),
	patch(Lib),
	message_end(Lib, Operation),!.

patch(Lib) :-
	m_third_party_patch(Lib, PatchFile), !,
	Operation = "patch source",
	message_start(Lib, Operation),
	fsR(PatchFile, PatchFilePath),
	third_party_path(real_sourcedir(Lib), SrcDir),
	process_call(path(patch), ['-p0'], [cwd(SrcDir), stdin(file(PatchFilePath)), status(0)]),
	message_end(Lib, Operation),!.
patch(_).

:- export(checksum/1).
checksum(Lib) :-
 	m_third_party_source_md5(Lib, Sum), !,
	Operation = "verify tarball checksum",
	message_start(Lib, Operation),
	third_party_path(cachetar(Lib), TarPath),
	md5sum(TarPath, Sum),
	message_end(Lib, Operation),!.
checksum(_Lib).

:- export(configure/1).
configure(Lib) :-
	Operation = "configure",
	message_start(Lib, Operation),
	get_build_system(Lib, BuildSystem),
	get_env(Env),
	( BuildSystem = gnu_build_system ->
	    third_party_path(real_sourcedir(Lib), SrcDir),
	    path_concat(SrcDir, configure, ConfigurePath),
	    get_config_options(Lib, Options),
	    process_call(ConfigurePath, Options,
	      [cwd(SrcDir), env(Env), status(0)])
	; BuildSystem = custom ->
	    m_third_party_custom_configure(Lib, Env)
	; message(['ERROR: ', 'Unknown build system \'', BuildSystem,
	           '\' for third-party \'', Lib, '\'']),
	    fail
	),
	message_end(Lib, Operation),!.

:- export(build/1).
build(Lib) :-
	Operation = "build",
	message_start(Lib, Operation),
	get_build_system(Lib, BuildSystem),
	( BuildSystem = gnu_build_system ->
	  make(Lib, [all])
	; BuildSystem = custom ->
	    get_env(Env),
	    m_third_party_custom_build(Lib, Env)
	; message(['ERROR: ', 'Unknown build system \'', BuildSystem,
	           '\' for third-party \'', Lib, '\'']),
	    fail
	),
	message_end(Lib, Operation),!.

:- export(installed/1).
installed(Lib) :-
	third_party_path(private(Lib, storedir), Dir),
	file_exists(Dir).

:- export(install/1).
install(Lib) :- Operation = "install",
	installed(Lib), !,
	message_status(Lib, Operation, already_done).
install(Lib) :- Operation = "install",
	message_start(Lib, Operation),
	get_build_system(Lib, BuildSystem),
	( BuildSystem = gnu_build_system ->
	  make(Lib, [install])
	; BuildSystem = custom ->
	    get_env(Env),
	    m_third_party_custom_install(Lib, Env)
	; message(['ERROR: ', 'Unknown build system \'', BuildSystem,
	           '\' for third-party \'', Lib, '\'']),
	    fail
	),
	message_end(Lib, Operation),!.

:- export(activate/1).
activated(Lib) :-
	( DirId = bindir ; DirId = libdir ; DirId = includedir ),
	third_party_path(DirId, Dir), third_party_path(private(Lib, DirId), PrivDir),
	file_exists(Dir), file_exists(PrivDir),
	directory_files(PrivDir, PrivFiles),
	member(PrivFile, PrivFiles), \+ member(PrivFile, ['.', '..']),
	path_concat(Dir, PrivFile, File),
	file_exists(File), !.

:- export(activate/1).
activate(Lib) :- Operation = "activate",
	installed(Lib), activated(Lib), !,
	message_status(Lib, Operation, already_done).
activate(Lib) :-
	Operation = "activate",
	message_start(Lib, Operation),
	%
	( installed(Lib) -> true; install(Lib) ),
	%
	third_party_link(Lib, bindir),
	third_party_link(Lib, libdir),
	third_party_link(Lib, includedir),
	%
	message_end(Lib, Operation), !.
	
:- export(deactivate/1).
deactivate(Lib) :- Operation = "deactivate library",
	(\+ installed(Lib); \+ activated(Lib)), !,
 	message_status(Lib, Operation, already_done).
deactivate(Lib) :- Operation = "deactivate",
	message_start(Lib, Operation),
	%
	third_party_unlink(Lib, bindir),
	third_party_unlink(Lib, libdir),
	third_party_unlink(Lib, includedir),
	%
	message_end(Lib, Operation), !.

third_party_link(Lib, DirId) :-
	PrivDirId = private(Lib, DirId),
	third_party_path(DirId, Dir),
	third_party_path(PrivDirId, PrivDir),
	third_party_relpath31(PrivDirId, RelativePrivDir),
	link_all_files(Dir, PrivDir, RelativePrivDir).

third_party_unlink(Lib, DirId) :-
	PrivDirId = private(Lib, DirId),
	third_party_path(DirId, Dir),
	third_party_path(PrivDirId, PrivDir),
	third_party_relpath31(PrivDirId, RelativePrivDir),
	unlink_all_files(Dir, PrivDir, RelativePrivDir).

:- export(uninstall/1).
uninstall(Lib) :- Operation = "uninstall",
	(\+ installed(Lib)), !,
	message_status(Lib, Operation, already_done).
uninstall(Lib) :- Operation = "uninstall",
	message_start(Lib, Operation),
	%
	( activated(Lib) -> deactivate(Lib) ; true ),
	third_party_path(private(Lib, storedir), Path),
	rec_delete_file_nofail(Path),
	%
	message_end(Lib, Operation), !.
	
:- export(auto_install/1).
auto_install(Lib) :-
	\+ installed(Lib), !,
	% Fetch source
	source_clean(Lib),
	download(Lib),
	checksum(Lib),
	decompress_and_patch(Lib),
	% Configure, build, and install
	configure(Lib),
	build(Lib),
	install(Lib),
	% Activate
	activate(Lib),
	delete_cachetar(Lib).
auto_install(Lib) :-
	activate(Lib).

get_build_system(Lib, BuildSystem) :-
	( m_third_party_build_system(Lib, BuildSystem0) ->
	    BuildSystem = BuildSystem0
	; BuildSystem = unknown
	).

:- doc(section, "Utils").

:- doc(bug, "We use our own recursive rm, because te one available in
the system poluate the atom tables").

rec_delete_file_nofail(FILE) :-
	process_call(path(rm), ['-Rf', FILE], [status(0)]).

message_status(Lib, Msg, Status) :-
	message(['**',  ' ', Lib, ' ', 'auto', ' ', 'installation', ':', ' ', $$(Msg), ' ', [Status], ' ', '**']).
	
message_start(Lib, Msg) :- message_status(Lib, Msg, start).
message_start(Lib, Msg) :- message_status(Lib, Msg, fail), fail.

message_end(Lib, Msg) :- message_status(Lib, Msg, completed).
	

