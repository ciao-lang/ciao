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
:- use_module(library(messages)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(process), [process_call/3, process_pipe/2]).
:- use_module(library(system), [winpath/3, winpath/2, working_directory/2]).
:- use_module(library(source_tree), [current_file_find/3]).
:- use_module(library(pathnames), [path_basename/2]).

:- use_module(ciaobld(config_common), [default_eng/1, bld_eng_path/4, cmdname_ver/5]).
:- use_module(engine(internals), ['$bundle_prop'/2]).
:- use_module(library(bundle/bundle_info), [
	enum_sub_bundles/2,
	bundle_version/2,
	bundle_version_patch/2]).
:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(bundle_hash), [
	bundle_versioned_packname/2, bundle_commit_info/3]).
:- use_module(ciaobld(pbundle_generator)).
:- use_module(ciaobld(builder_aux), [wr_template/4]).

% (hooks for gen_pbundle)
:- include(ciaobld(pbundle_gen_hookdefs)).

% ===========================================================================

:- doc(iscc/1, "Specifies the path where the @em{Inno Setup Compiler} is
   installed.").

iscc := '/cygdrive/c/Program\\ Files/Inno\\ Setup\\ 5/iscc.exe'.

% TODO: Use Bundle to customize more definitions

% (hook)
gen_pbundle_hook(win32, Bundle, _Options) :- !,
	simple_message("Creating Windows installer for ~w, please be patient... ",
	    [~bundle_versioned_packname(Bundle)]),
	simple_message("Creating ISS scripts... "),
	FileIss = 'Ciao.iss',
	FileListName = 'file_list.iss',
	create_iss_file(Bundle, FileIss, FileListName),
	compute_file_list_iss(FileListName),
	simple_message("Compiling ISS scripts... "),
	invoke_iscc(FileIss),
	simple_message("Creation of Windows installer finished").

invoke_iscc(FileIss) :-
	process_call(~iscc, [FileIss], []),
	% TODO: should it appear before?
	create_pbundle_output_dir.

% TODO: too many definitions here are hardwired
create_iss_file(Bundle, FileIss, FileListName) :-
	OutputBaseFileName = ~atom_codes(~bundle_versioned_packname(Bundle)),
	% TODO: see PrettyCommitDesc in pbundle_download.pl
	CommitId = ~bundle_commit_info(Bundle, id),
	AppVerName = ~atom_codes(~atom_concat(['Ciao-', ~bundle_version_patch(Bundle), ' (', CommitId, ')'])),
	EngMainMod = ~default_eng,
	%
	working_directory(Cwd, Cwd), % TODO: sure?
	wr_template(at(Cwd), ~builder_src_dir/'win32', FileIss, [
	    'MyAppName' = "Ciao", % TODO: extract from bundle
	    'MyAppVerName' = AppVerName,
	    'OutputBaseFileName' = OutputBaseFileName,
	    'MyAppPublisher' = "The CLIP Laboratory", % TODO: extract from bundle
	    'LicenseFile' = ~license_file,
 	    'MyAppExeName' = ~cmdname_ver(yes, core, plexe, 'ciaosh'), % TODO: extract from bundle
	    'CiaoVersion' = ~bundle_version(core), % TODO: extract from bundle
	    'SourceDir' = ~source_dir,
	    'MyRelBuildDir' = ~relciaodir(~fsR(builddir(build))),
	    'OutputDir' = ~output_dir,
	    'ManualIcons' = ~get_manual_icons(Bundle),
	    'DefaultDirName' = ~default_dir_name(Bundle),
	    'CiaoEngineExec' = ~winpath(relative, ~relciaodir(~bld_eng_path(exec, build, EngMainMod))),
	    'FileListName' = ~winpath(full, ~fsR(bundle_src(ciao)/FileListName))
	]).

default_dir_name(Bundle) := D :-
	'$bundle_prop'(Bundle, packname(Packname)),
	D = ~atom_codes(Packname).

get_manual_icons(Bundle, S) :-
	findall(Str, get_manual_icons_(Bundle, Str), L),
	flatten(L, S).

:- use_module(ciaobld(builder_cmds),
	[ensure_load_bundlehooks/1, bundle_manual_base/2]).

% TODO: Check that this is correct when multiple manuals per bundle are generated ("Name" is shared)
get_manual_icons_(ParentBundle, Str) :-
	enum_sub_bundles(ParentBundle, Bundle),
	ensure_load_bundlehooks(Bundle),
	%
	DocFormat = pdf,
	RelBuildDir = ~relciaodir(~fsR(builddir(build))),
	'$bundle_prop'(Bundle, packname(PackName)),
	ManualBase = ~bundle_manual_base(Bundle), % (nondet)
	FileMain = ~atom_concat([ManualBase, '.', DocFormat]),
	Str = ["Name: {group}\\" || (~atom_codes(PackName)), " Manual in PDF; ",
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
% exclude_win_subdir('./doc').
% exclude_win_subdir('./core_OC').
% exclude_win_subdir('./core_OCjs').

rel_extra_system_file(Source, DestDir) :-
	EngMainMod = ~default_eng,
	Source = ~winpath(~extra_system_file), % (nondet)
	DestDir0 = ~winpath(relative, ~relciaodir(~bld_eng_path(objdir, build, EngMainMod))),
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
extra_system_file := ~relciaodir(~bld_eng_path(exec, build, EngMainMod)) :-
	EngMainMod = ~default_eng.
extra_system_file := ~relciaodir(~bld_eng_path(lib_so, build, EngMainMod)) :-
	EngMainMod = ~default_eng.

display_file_entry(Source, DestDir) :-
	display_list(['Source: ', Source, '; DestDir:{app}\\', DestDir, '\n']).

license_file := ~atom_codes(~winpath(relative, ~fsR(bundle_src(ciao)/'LGPL'))).

source_dir := ~atom_codes(~winpath(relative, ~fsR(bundle_src(ciao)))).
output_dir := ~atom_codes(~winpath(relative, ~pbundle_output_dir)).

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

% % TODO: this target is probably not necessary
% gen_pbundle__win32_test <- :-
% 	ciao_iss('Ciao_test.iss', 'file_list_test.iss',
% 	    ~atom_codes(~bundle_versioned_packname(~root_bundle))),
% 	file_list_test_iss('file_list_test.iss'),
% 	gen_pbundle__win32_test.

% TODO: targets that are not necessary
% 'Ciao.iss' <- ['Manifest/Ciao.iss.skel'] :: FileName :-
% 	OutputBaseFileName = ~atom_codes(~bundle_versioned_packname(~root_bundle)),
% 	ciao_iss(FileName, 'file_list.iss', OutputBaseFileName).
% 
% 'Ciao_test.iss' <- ['Manifest/Ciao.iss.skel'] :: FileName :-
% 	OutputBaseFileName = ~append(~atom_codes(~bundle_versioned_packname(~root_bundle)),
% 	    "-test"),
% 	ciao_iss(FileName, 'file_list_test.iss', OutputBaseFileName).
% 
% 'file_list.iss' <- [] :: FileName :-
% 	compute_file_list_iss(FileName).
% 
% 'file_list_test.iss' <- [] :: FileName :-
% 	file_list_test_iss(FileName).

% 'core/Win32/wsetup.cpx' <- ['core/Win32/wsetup.pl'] :-
% 	make_exec(['core/Win32/wsetup.pl'], 'core/Win32/wsetup.cpx').

% % TODO: this target is probably not necessary
% gen_pbundle__win32_test :-
% 	gen_pbundle__win32_('Ciao_test.iss').

% file_list_test_iss(FileName) :-
% 	file_list_type_iss(test, FileName).
