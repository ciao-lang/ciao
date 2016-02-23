:- module(archive_files, [], [assertions, basicmodes, nativeprops, regtypes, fsyntax]).

:- doc(title, "File Archiver").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module offers predicates to generate file
   @concept{archive}s in some common formats. An archive file is a
   file that is composed of one or more computer files along with
   metadata, with can be compressed. See @pred{archive_files/4} for
   the supported formats.
").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(process), [process_call/3, process_pipe/2]).
:- use_module(library(pathnames),
	[path_split/3, path_concat/3, path_splitext/3]).
:- use_module(library(system),
	[mktemp_in_tmp/2, copy_file/3, rename_file/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(port_reify)).

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
gtar := ~get_bundle_flag(ciao:gtar_cmd).

:- export(archive_files/4).
:- pred archive_files(SourceDir, Files, TopDir, Archive) ::
	atm * list(atm) * atm * atm
 # "Create an archive @var{Archive} of the given files at
    @var{Files}, relative to @var{SourceDir}. If @var{TopDir} is
    not @tt{''}, files are placed inside the archive under
    @var{TopDir}.

    The format is automatically detected from the extension of
    @var{Archive} (where valid formats are: @tt{.tar.gz}, @tt{.tgz},
    @tt{.tar.bz2}, @tt{.tbz}, and @tt{.zip}).".

archive_files(SourceDir, Files, TopDir, Archive) :-
	archive_ext(Archive, Ext),
	atom_concat(Archive, '.tmp', TmpArchive),
	archive_files_(Ext, SourceDir, Files, TopDir, TmpArchive),
	del_file_nofail(Archive),
	rename_file(TmpArchive, Archive).

archive_files_(Ext, SourceDir, Files, TopDir, Archive) :-
	tar_compress(Ext, Compress),
	!,
	tar_archive(Compress, SourceDir, Files, TopDir, Archive).
archive_files_('.zip', SourceDir, Files, TopDir, Archive) :- !,
	zip_archive(SourceDir, Files, TopDir, Archive).
archive_files_(Ext, _SourceDir, _Files, _TopDir, _Archive) :-
	throw(error(unrecognized_extension(Ext), archive_files/4)).

% An archive extension (path_splitext/3 does not work with .tar.gz)
archive_ext(Archive, Ext) :-
	path_splitext(Archive, Archive0, Ext0),
	( tar_compress_ext(Ext0),
	  path_splitext(Archive0, _, DotTar),
	  DotTar = '.tar' ->
	    atom_concat(DotTar, Ext0, Ext)
	; Ext = Ext0
	).

% Additional extension of a .tar file
tar_compress_ext('.gz').
tar_compress_ext('.bz2').
	
% Command to compress a tar extension
tar_compress('.tar.gz',  'gzip').
tar_compress('.tgz',     'gzip').
tar_compress('.tar.bz2', 'bzip2').
tar_compress('.tbz',     'bzip2').

% ---------------------------------------------------------------------------

% Create a tar file `Archive` compressed using `Compress`, storing
% files specified in `Files` paths, prefixed with `TopDir` inside the
% archive.
tar_archive(Compress, SourceDir, Files, TopDir, Archive) :-
	write_file_list_tmp(Files, '', FileList), % topdir added with '--transform'
	once_port_reify(tar_archive_(Compress, SourceDir, FileList, TopDir, Archive), R),
	del_file_nofail(FileList),
	port_call(R).

tar_archive_(Compress, SourceDir, FileList, TopDir, Archive) :-
	( TopDir = '' -> Args = []
	; % transform path to add TopDir
	  Args = ['--transform', ~atom_concat(['s,^,', TopDir, '/,S'])]
	),
	process_pipe([
          process_call(~gtar,
	               ['--directory', SourceDir,
			'-cf', '-',
			'--owner=0', '--group=0',
			~atom_concat('--files-from=', FileList)|Args]),
	  process_call(path(Compress), ['--best', '-c'])
        ], [stdout(file(Archive))]).

% ---------------------------------------------------------------------------

% Create a zip file `Archive` compressed using, storing files
% specified in `Files` paths, prefixed with `TopDir` inside the
% archive.
zip_archive(SourceDir, Files, '', Archive) :- !,
	zip_archive_(SourceDir, Files, '', Archive).
zip_archive(SourceDir, Files, TopDir, Archive) :-
	path_split(SourceDir, SourceDir0, SourceTop),
	( SourceTop = TopDir ->
	    % Top dir coincides
	    zip_archive_(SourceDir0, Files, TopDir, Archive)
	; % Fake top with a symbolic link and archive
	  % (it seems to be the most portable way to do it)
	  path_concat(SourceDir0, TopDir, FakeSourceDir),
	  del_file_nofail(FakeSourceDir),
	  copy_file(SourceDir, FakeSourceDir, [overwrite, symlink]),
	  once_port_reify(zip_archive_(SourceDir0, Files, TopDir, Archive), R),
	  del_file_nofail(FakeSourceDir),
	  port_call(R)
	).

zip_archive_(SourceDir, Files, TopDir, Archive) :-
	write_file_list_tmp(Files, TopDir, FileList),
	once_port_reify(zip_archive__(SourceDir, FileList, Archive), R),
	del_file_nofail(FileList),
	port_call(R).

zip_archive__(SourceDir, FileList, Archive) :-
	process_call(path(zip),
	       ['-@', '-', '-q'],
	       [cwd(SourceDir),
		stdin(file(FileList)),
		stdout(file(Archive))]).

% ---------------------------------------------------------------------------

:- use_module(library(streams), [open_output/2, close_output/1]).

% (obtain FileList file)
write_file_list_tmp(Files, TopDir, FileList) :-
	mktemp_in_tmp('filelistXXXXXX', FileList),
	write_file_list(Files, TopDir, FileList).

% Write the archive file list (adding TopDir)
write_file_list(Files, TopDir, FileList) :-
	open_output(FileList, Output),
	write_file_list_(Files, TopDir),
	close_output(Output).

write_file_list_([], _).
write_file_list_([RelFile|Fs], TopDir) :-
	path_concat(TopDir, RelFile, File),
	display(File), nl, % (do not use displayq/1)
	write_file_list_(Fs, TopDir).

