:- module(_, [], [assertions, datafacts]).

:- doc(title, "File buffers with atomic writes").
:- doc(author, "Jose F. Morales").

% TODO: implement atomic replacement based on "rename" (atomic on POSIX)
% TODO: make sure that we use MoveFileEx in Win32
% TODO: call C flush() and fsync()? (measure overhead)

:- use_module(engine(internals), ['$open'/3]).
:- use_module(engine(stream_basic)).
:- use_module(library(system), [delete_file/1, rename_file/2, mktemp/2]).
:- use_module(library(port_reify), [once_port_reify/2]).
:- use_module(library(ctrlcclean), [delete_on_ctrlc/2]).
:- use_module(engine(runtime_control), [set_prolog_flag/2, prolog_flag/3]).

:- use_module(library(compiler/compressed_bytecode)). % OPA % TODO: generalize

:- export(file_buffer_begin/4).
% file_buffer_begin(+Path, +Compress, -Buffer, -Stream):
%   Begin an atomic file write for file `Path`. It will open
%   an output stream `Stream` to a temporary file, which is
%   atomically copied to `Path` on `file_buffer_commit/1`:
%
%     - Compress the output if Compress=yes.
%     - Buffer is a handler for file_buffer_commit/1
%   
%   This is not expected to fail.
file_buffer_begin(Path, Compress, Buffer, Stream) :-
	% Create a temporary file
	% NOTE: do not create in /tmp, rename() requires the files in
	% the same filesystem.
	atom_concat(Path, '-tmpciaoXXXXXX', PathTmp0),
	mktemp(PathTmp0, TmpFile), % TODO: fix mktemp, do not close the file
	delete_on_ctrlc(TmpFile, Ref),
	'$open'(TmpFile, w, TmpS), % TODO: recover on error?
	Stream = TmpS,
	Buffer = file_buffer(Ref, Path, TmpFile, TmpS, Compress).

:- export(file_buffer_commit/1).
% file_buffer_commit(+Buffer):
%   Commit an atomic file write for Path file.
%     - Fail if output cannot be created.
%
file_buffer_commit(file_buffer(Ref, Path, TmpFile, TmpS, Compress)) :-
	close(TmpS),
	( Compress = yes ->
	    atom_concat(Path, '-tmpciaoXXXXXX', PathTmp0),
	    mktemp(PathTmp0, TmpFile2),
	    compress_file(TmpFile, TmpFile2),
	    delete_file(TmpFile)
	; TmpFile2 = TmpFile
	),
	% replace atomically Path by the temporary file buffer
	prolog_flag(fileerrors, OldFE, off), % (fail on file errors)
	( rename_file(TmpFile2, Path) -> OK = yes
	; del_file_nofail(TmpFile2),
	  OK = no
	),
	set_prolog_flag(fileerrors, OldFE),
	erase(Ref),
	OK = yes.

:- export(file_buffer_erase/1).
% file_buffer_erase(+Buffer):
%   Cancel an atomic file write for Path file and remove the file.
%
file_buffer_erase(file_buffer(Ref, Path, TmpFile, TmpS, _Compress)) :-
	close(TmpS),
	delete_file(TmpFile),
	erase(Ref),
	del_file_nofail(Path).

compress_file(InFile, OutFile) :-
	'$open'(OutFile, w, OutS),
	current_output(So),
	set_output(OutS),
	%
	open(InFile,read,InS),
	compressLZ(InS),
	close(InS),
	%
	set_output(So),
	close(OutS).

:- export(del_file_nofail/1).
% TODO: duplicated, move to system? (system_extra is not used in ciaoc)
del_file_nofail(File) :-
	once_port_reify(delete_file(File), _).

