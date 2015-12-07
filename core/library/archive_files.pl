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

:- use_module(library(process), [process_call/3, process_pipe/2]).
:- use_module(library(pathnames),
	[path_split/3, path_concat/3, path_splitext/3]).
:- use_module(library(system),
	[mktemp_in_tmp/2, copy_file/3, rename_file/2]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(streams), [open_output/2, close_output/1]).

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
	mktemp_in_tmp('filelistXXXXXX', FileList),
	write_file_list(Files, TopDir, FileList),
	archive_from(SourceDir, FileList, TopDir, Archive),
	del_file_nofail(FileList).

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

archive_from(SourceDir, FileList, '', Archive) :- !,
	archive_from_(SourceDir, FileList, Archive).
archive_from(SourceDir, FileList, TopDir, Archive) :-
	% If actual top of SourceDir is different than TopDir, fake a
	% symbolic link
	path_split(SourceDir, SourceDir0, SourceTop),
	( SourceTop = TopDir -> % Top coincides
	    archive_from_(SourceDir0, FileList, Archive)
	; % Fake top with a symbolic link and archive
	  % (it seems to be the most portable way to do it)
	  path_concat(SourceDir0, TopDir, FakeSourceDir),
	  del_file_nofail(FakeSourceDir),
	  copy_file(SourceDir, FakeSourceDir, [overwrite, symlink]),
	  archive_from_(SourceDir0, FileList, Archive),
	  del_file_nofail(FakeSourceDir)
	).

archive_from_(SourceDir, FileList, Archive) :-
	atom_concat(Archive, '.tmp', TmpArchive),
	path_splitext(Archive, _, Ext),
	%
	( tar_compress(Ext, Cmd) ->
	    archive_as_tar(Cmd, SourceDir, FileList, TmpArchive)
	; archive_as_zip(SourceDir, FileList, TmpArchive)
	),
	%
	del_file_nofail(Archive),
	rename_file(TmpArchive, Archive).

% Command to compress a tar extension
tar_compress('.tar.gz',  'gzip').
tar_compress('.tgz',     'gzip').
tar_compress('.tar.bz2', 'bzip2').
tar_compress('.tbz',     'bzip2').

archive_as_tar(Compress, SourceDir, FileList, Archive) :-
	process_pipe([
          process_call(~gtar,
	               ['--directory', SourceDir,
			'-cf', '-',
			'--owner=0', '--group=0',
			~atom_concat('--files-from=', FileList)]),
	  process_call(path(Compress), ['--best', '-c'])
        ], [stdout(file(Archive))]).

archive_as_zip(SourceDir, FileList, Archive) :-
	process_call(path(zip),
	       ['-@', '-', '-q'],
	       [cwd(SourceDir),
		stdin(file(FileList)),
		stdout(file(Archive))]).

