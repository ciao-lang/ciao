:- module(pbundle_gen_win32, [], [fsyntax, assertions]).

:- doc(title,  "Windows distributions").
:- doc(author, "Edison Mera").

:- doc(module, "This module provides the definitions used to generate
   installers for the Windows platform. Currently the installer is
   based on the @em{Inno Setup Compiler} (ISCC). Thus, ISCC is required
   in order to build a graphical installer. To get the latest version
   visit @href{http://www.jrsoftware.org/isdl.php} and download the
   @em{QuickStart Pack}.").

% ===========================================================================

:- use_module(library(aggregates)).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(messages), [simple_message/1, simple_message/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(streams_utils)).
:- use_module(library(process), [process_call/3, process_pipe/2]).
:- use_module(library(system), [winpath/3, winpath/2, working_directory/2]).
:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(library(pathnames), [path_basename/2, path_concat/3]).
:- use_module(library(pathnames), [path_get_relative/3]).
:- use_module(library(version_strings), [version_split_patch/3]).

:- use_module(ciaobld(eng_defs), [eng_path/3]).
:- use_module(ciaobld(config_common),
	[default_eng_def/1,
	 concat_ext/3]).
:- use_module(engine(internals), [ciao_root/1]).
:- use_module(engine(internals), ['$bundle_prop'/2]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(ciaobld(bundle_hash), [bundle_commit_info/3]).
:- use_module(ciaobld(pbundle_generator)).
:- use_module(ciaobld(builder_aux), [wr_template/4]).

:- use_module(ciaobld(manifest_compiler), [
    ensure_load_manifest/1,
    bundle_manual_base/2
]).

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

% ===========================================================================

:- doc(iscc/1, "Specifies the path where the @em{Inno Setup Compiler} is
   installed.").

iscc := '/cygdrive/c/Program\\ Files/Inno\\ Setup\\ 5/iscc.exe'.

% TODO: Use Target and MainBundle to customize more definitions

% (hook)
gen_pbundle_hook(win32, Target, _Options) :- !,
	simple_message("Creating Windows installer for ~w, please be patient... ",
	    [~dist_versioned_pkgname(Target)]),
	simple_message("Creating ISS scripts... "),
	CiaoRoot = ~ciao_root,
	FileIss = ~path_concat(CiaoRoot, 'Ciao.iss'),
	FileListName = ~path_concat(CiaoRoot, 'file_list.iss'),
	create_iss_file(Target, CiaoRoot, FileIss, FileListName),
	compute_file_list_iss(FileListName),
	create_pbundle_output_dir(Target),
	simple_message("Compiling ISS scripts... "),
	process_call(~iscc, [FileIss], []),
	simple_message("Creation of Windows installer finished").

% TODO: too many definitions here are hardwired
create_iss_file(Target, CiaoRoot, FileIss, FileListName) :-
	Version = ~dist_version(Target),
	version_split_patch(Version, VersionNopatch, _),
	OutputBaseFileName = ~atom_codes(~dist_versioned_pkgname(Target)),
	% TODO: see PrettyCommitDesc in pbundle_download.pl
	dist_main_bundle(Target, MainBundle),
	BuildDir = ~bundle_path(MainBundle, builddir, '.'),
	CommitId = ~bundle_commit_info(MainBundle, id),
	AppVerName = ~atom_codes(~atom_concat(['Ciao-', Version, ' (', CommitId, ')'])),
	Eng = ~default_eng_def,
	%
	working_directory(Cwd, Cwd), % TODO: sure?
	wr_template(at(Cwd), ~bundle_path(builder, 'src/win32'), FileIss, [
	    'MyAppName' = "Ciao", % TODO: extract from bundle
	    'MyAppVerName' = AppVerName,
	    'OutputBaseFileName' = OutputBaseFileName,
	    'MyAppPublisher' = "The CLIP Laboratory", % TODO: extract from bundle
	    'LicenseFile' = ~license_file,
 	    'MyAppExeName' = ~concat_ext(plexe, 'ciaosh'), % TODO: extract from bundle
	    'CiaoVersion' = VersionNopatch, % TODO: extract from bundle
	    'SourceDir' = ~atom_codes(~winpath(relative, CiaoRoot)),
	    'MyRelBuildDir' = ~relciaodir(BuildDir),
	    'OutputDir' = ~atom_codes(~winpath(relative, ~pbundle_output_dir(Target))),
	    'ManualIcons' = ~get_manual_icons(Target),
	    'DefaultDirName' = ~atom_codes(~dist_pkgname(Target)),
	    'CiaoEngineExec' = ~winpath(relative, ~relciaodir(~eng_path(exec, Eng))),
	    'FileListName' = ~winpath(full, FileListName)
	]).

get_manual_icons(Target, S) :-
	findall(Str, get_manual_icons_(Target, Str), L),
	flatten(L, S).

% TODO: Do not use PkgName as name of the manual!
get_manual_icons_(Target, Str) :-
	dist_bundles(Target, Bundle),
	ensure_load_manifest(Bundle),
	ManualBase = ~bundle_manual_base(Bundle), % (nondet)
	%
	DocFormat = pdf,
	RelBuildDir = ~relciaodir(~bundle_path(Bundle, builddir, '.')),
	PkgName = ~dist_pkgname(Target),
	FileMain = ~atom_concat([ManualBase, '.', DocFormat]),
	Str = ["Name: {group}\\" || (~atom_codes(PkgName)), " Manual in PDF; ",
	       "Filename: {app}\\" || (~atom_codes(RelBuildDir)), "\\doc\\" || (~atom_codes(FileMain)), "; ",
	       "WorkingDir: {app}\\" || (~atom_codes(RelBuildDir)), "\\doc\n"].

% Compute the list of files that will be included in the ISS file
compute_file_list_iss(FileName) :-
	open_output(FileName, Output),
	output_file_list,
	close_output(Output).

output_file_list :-
	( % (failure-driven loop)
	  current_file(Source, DestDir),
	    display_file_entry(Source, DestDir),
	    fail
	; true
	).

% TODO: include into library(source_tree) logic
current_file(Source, DestDir) :-
	( current_file_find(distributable_precomp(bin), './', FileName),
	  % TODO: Disabled (check again if it is needed)
	  % \+ exclude_win_subdir(FileName),
	  atom_concat('./', BaseFile, FileName),
	  fullwinname(BaseFile, Source),
	  fullwinpath(BaseFile, DestDir)
	; rel_extra_system_file(Source, DestDir)
	).

% TODO: why? (it was not really, './' prefix does not match)
% exclude_win_subdir('./core/doc').
% exclude_win_subdir('./core/engine').
% exclude_win_subdir('./lpdoc/doc').
% exclude_win_subdir('./ciaopp/doc').
% exclude_win_subdir('./alldocs').
% exclude_win_subdir('./core_OC').
% exclude_win_subdir('./core_OCjs').

rel_extra_system_file(Source, DestDir) :-
	Eng = ~default_eng_def,
	Source = ~winpath(~extra_system_file), % (nondet)
	DestDir0 = ~winpath(relative, ~relciaodir(~eng_path(objdir, Eng))),
	DestDir = ~atom_concat(DestDir0, '\\').

each_line(Lines0, Line) :-
	( append(Line0, [0'\n|Lines], Lines0) ->
	    ( Line = Line0
	    ; each_line(Lines, Line)
	    )
        ; Line = Lines0
        ).

bin_sh('/usr/bin/sh.exe').

extra_system_file(A) :- bin_sh(A).
extra_system_file(A) :-
	% Get shared library dependencies for /usr/bin/sh.exe
	% TODO: generalize, move to library, "otool -L" in macosx
	bin_sh(BinSh),
	process_pipe([
          process_call(path(ldd), [BinSh], [status(_)]),
          process_call(path(grep), ['/usr/bin'], [status(_)]),
          process_call(path(sed), ['-e', 's:.* => \\(.*\\) (0.*:\\1:g'])
        ], [stdout(string(Lines))]),
	each_line(Lines, Line),
	Line \= [],
	atom_codes(A, Line).
% TODO: hardwired, why?
extra_system_file('/usr/bin/cyggsl-0.dll').
extra_system_file('/usr/bin/cyggslcblas-0.dll').
extra_system_file('/usr/lib/lapack/cygblas-0.dll').
% TODO: hardwired, why?
% TODO: Use inst_* instead of bld_*?
extra_system_file := ~relciaodir(~eng_path(exec, Eng)) :-
	Eng = ~default_eng_def.
extra_system_file := ~relciaodir(~eng_path(lib_so, Eng)) :-
	Eng = ~default_eng_def.

relciaodir(S) := Dir :-
	path_get_relative(~ciao_root, S, Dir).

display_file_entry(Source, DestDir) :-
	display_list(['Source: ', Source, '; DestDir:{app}\\', DestDir, '\n']).

license_file := ~atom_codes(~winpath(relative, ~path_concat(~ciao_root, 'LGPL'))).

fullwinname(File, WinName) :-
	winpath(relative, File, WinName).

fullwinpath(File, WinPath) :-
	extract_unix_filepath(File, Path),
	winpath(relative, Path, WinPath).

decompose_win_filename_string(FullName, Dir, Name, Extension) :-
	decompose_filename_string(FullName, 0'\\, Dir, Name, Extension).

decompose_unix_filename_string(FullName, Dir, Name, Extension) :-
	decompose_filename_string(FullName, 0'/, Dir, Name, Extension).

decompose_filename_string(FullName, PathSeparator, Dir, Name, Extension) :-
	append(Main, [PathSeparator|SubPath], FullName),
	!,
	decompose_filename2_string(Main, PathSeparator, Dir, SubPath, Name,
	    Extension).
decompose_filename_string(FullName, PathSeparator, "", Name, Extension) :-
	append(Name, [PathSeparator|Extension], FullName),
	!.
decompose_filename_string(Name, _, "", Name, "").

decompose_filename2_string(Main, PathSeparator, Dir, SubPath, Name, Extension) :-
	decompose_filename_string(SubPath, PathSeparator, Dir2, Name, Extension),
	append(Main, [PathSeparator|Dir2], Dir).

extract_win_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'\\, Path).

extract_unix_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'/, Path).

extract_filepath(FullName, PathSeparator, Path) :-
	decompose_filename_string(~atom_codes(FullName), PathSeparator, PathC, _, _),
	atom_codes(Path, PathC).

% ===========================================================================

% 'core/Win32/wsetup.cpx' <- ['core/Win32/wsetup.pl'] :-
% 	make_exec(['core/Win32/wsetup.pl'], 'core/Win32/wsetup.cpx').

