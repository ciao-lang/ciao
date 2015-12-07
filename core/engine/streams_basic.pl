:- module(streams_basic, [
        open/3, open/4, open_option_list/1, close/1,
        set_input/1, current_input/1,
        set_output/1, current_output/1,
        character_count/2, line_count/2, line_position/2,
        flush_output/1, flush_output/0, clearerr/1,
        current_stream/3, stream_code/2,
        absolute_file_name/2, absolute_file_name/7,
	pipe/2,
        sourcename/1, stream/1, stream_alias/1, io_mode/1, 
        atm_or_int/1
        ],
        [assertions, nortchecks, isomodes]).

:- doc(title, "Basic file/stream handling").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Mats Carlsson").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"This module provides basic predicates for handling
   files and streams, in order to make input/output on them.").

:- use_module(engine(internals), ['$open'/3, '$absolute_file_name_checked'/7]).

:- doc(open(File, Mode, Stream), "Open @var{File} with mode
   @var{Mode} and return in @var{Stream} the stream associated with the
   file.  No extension is implicit in @var{File}.").


:- pred open(+sourcename, +io_mode, ?stream) + (iso, native)
        # "Normal use.".

:- pred open(+int, +io_mode, ?stream) # "In the special case that
        @var{File} is an integer, it is assumed to be a file
        descriptor passed to Prolog from a foreign function call. The
        file descriptor is connected to a Prolog stream (invoking the
        C POSIX function @tt{fdopen()}) which is unified with
        @var{Stream}.".

:- doc(open(File, Mode, Stream, Options), "Same as
   @tt{open(@var{File}, @var{Mode}, @var{Stream})} with options @var{Options}.
   See the definition of @pred{open_option_list/1} for details.").


:- pred open(+sourcename, +io_mode, ?stream, +open_option_list) + native.

open(FileName, Mode, S) :-
        open_internal(FileName, Mode, S, [], 3).

open(FileName, Mode, S, Opts) :-
        open_internal(FileName, Mode, S, Opts, 4).

open_internal(FileName, Mode, S, Opts, N) :-
        var(S),
        atom(FileName),
        nonvar(Mode),
        io_mode(Mode), !,
        ( var(Opts) ->
	    throw(error(instantiation_error, open/N-4))
	; codif_opts(Opts, '', Codif) ->
            sub_atom(Mode, 0, 1, M),
	    atom_concat(M, Codif, MCodif),
	    ( integer(FileName) -> AbsoluteFileName=FileName
	    ; absolute_file_name(FileName, '', '', '.', AbsoluteFileName, _, _)
	    ),
	    '$open'(AbsoluteFileName, MCodif, S)
	;
	  throw(error(domain_error(open_option_list, Opts), open/N-4))
	).
open_internal(_, _, S, _, N) :- nonvar(S), !,
        throw(error(uninstantiation_error(S), open/N-3)).
open_internal(FileName, _, _, _, N) :- var(FileName), !,
        throw(error(instantiation_error, open/N-1)).
open_internal(FileName, _, _, _, N) :- nonvar(FileName), !,
        throw(error(domain_error(source_sink, FileName), open/N-1)).
open_internal(_, Mode, _, _, N) :- var(Mode), !,
        throw(error(instantiation_error, open/N-2)).
open_internal(_, Mode, _, _, N) :- atom(Mode), !,
        throw(error(domain_error(io_mode, Mode), open/N-2)).
open_internal(_, Mode, _, _, N) :- nonvar(Mode), \+ atom(Mode), !,
        throw(error(type_error(atom, Mode), open/N-2)).

codif_opts([], Codif, Codif).
codif_opts([Opt|Opts], InCodif, OutCodif):-
	codif_opt(Opt, InCodif, Codif),
	codif_opts(Opts, Codif, OutCodif).

codif_opt(V, _, _) :- var(V), !, fail.
codif_opt(lock, _, b) :- !.
codif_opt(lock_nb, _, l) :- !.
/* SWI options */
codif_opt(lock(Mode), _, Codif) :- !,
        lock_mode(Mode, M),
        atom_concat(b,M,Codif).
codif_opt(lock_nb(Mode), _, Codif) :- !,
        lock_mode(Mode, M),
        atom_concat(l,M,Codif).
codif_opt(Opt, Codif, Codif) :-
        message(warning, ['Open option ',Opt,' not yet implemented']).

lock_mode(read, r).
lock_mode(shared, r).
lock_mode(write, w).
lock_mode(exclusive, w).

:- doc(io_mode/1, "Can have the following values:
   @begin{description}

   @item{@tt{read}} Open the file for input.

   @item{@tt{write}} Open the file for output.  The file is created if
   it does not already exist, the file will otherwise be truncated.

   @item{@tt{append}} Open the file for output.  The file is created if
   it does not already exist, the file will otherwise be appended to.

   @end{description}").


:- prop io_mode(M) + regtype # "@var{M} is an opening mode ('read',
        'write' or 'append').".

io_mode(read).
io_mode(write).
io_mode(append).

:- doc(close(Stream), "Close the stream @var{Stream}.").

:- trust pred close(+stream) => stream + (iso, native).
:- impl_defined(close/1).

:- doc(set_input(Stream), "Set the current input stream to
   @var{Stream}.  A notion of @index{current input stream} is maintained
   by the system, so that input predicates with no explicit stream
   operate on the current input stream.  Initially it is set to
   @tt{user_input}.").

:- trust pred set_input(+stream) => stream + (iso, native).
:- impl_defined(set_input/1).

:- doc(current_input(Stream), "Unify @var{Stream} with the
@concept{current input stream}.  In addition to the ISO behavior,
stream aliases are allowed.  This is useful for most applications
checking whether a stream is the standard input or output.").

:- trust pred current_input(?stream) + (iso, native).
:- impl_defined(current_input/1).

:- doc(set_output(Stream), "Set the current output stream to
   @var{Stream}.  A notion of @index{current output stream} is maintained
   by the system, so that output predicates with no explicit stream
   operate on the current output stream.  Initially it is set to
   @tt{user_output}.").

:- trust pred set_output(+stream) + (iso, native).
:- impl_defined(set_output/1).

:- doc(current_output(Stream), "Unify @var{Stream} with the
@concept{current output stream}.  The same comment as for
@pred{current_input/1} applies.").

:- trust pred current_output(?stream) + (iso, native).
:- impl_defined(current_output/1).

:- doc(character_count(Stream,Count), "@var{Count} characters have
   been read from or written to @var{Stream}.").

:- trust pred character_count(+stream,?int) + native.
:- impl_defined(character_count/2).

:- doc(line_count(Stream,Count), "@var{Count} lines have been read
   from or written to @var{Stream}.").

:- trust pred line_count(+stream,?int) + native.
:- impl_defined(line_count/2).

:- doc(line_position(Stream,Count), "@var{Count} characters have
   been read from or written to the current line of @var{Stream}.").

:- trust pred line_position(+stream,?int) + native.
:- impl_defined(line_position/2).

:- doc(flush_output(Stream), "Flush any buffered data to output
   stream @var{Stream}.").

:- trust pred flush_output(+stream) => stream + (iso, native).
:- impl_defined(flush_output/1).

:- doc(flush_output, "Behaves like @tt{current_output(S),
   flush_output(S)}").

:- trust pred flush_output + iso.
:- impl_defined(flush_output/0).

:- doc(clearerr(Stream), "Clear the end-of-file and error indicators
           for input stream @var{Stream}.").

:- trust pred clearerr(+stream) => stream.
:- impl_defined(clearerr/1).

:- doc(current_stream(Filename,Mode,Stream), "@var{Stream} is a
   stream which was opened in mode @var{Mode} and which is connected to
   the absolute file name @var{Filename} (an atom) or to the file
   descriptor @var{Filename} (an integer).  This predicate can be used
   for enumerating all currently open streams through backtracking.").

:- trust pred current_stream(?atm_or_int,?io_mode,?stream) + native.
:- impl_defined(current_stream/3).

:- prop atm_or_int/1 + regtype.

atm_or_int(X):- atm(X).
atm_or_int(X):- int(X).


:- doc(stream_code(Stream,StreamCode), "@var{StreamCode} is the file
   descriptor (an integer) corresponding to the Prolog stream
   @var{Stream}.").

:- trust pred stream_code(+stream,?int).
:- trust pred stream_code(-stream,+int).
:- impl_defined(stream_code/2).


:- doc(file_search_path(Alias, Path), "The @concept{path alias}
   @var{Alias} is linked to path @var{Path}.  Both arguments must be
   atoms.  New facts (or clauses) of this predicate can be asserted to
   define new path aliases.  Predefined path aliases in Ciao are:

@begin{description}

@item{@tt{library}} Initially points to all Ciao library paths.  See
   @pred{library_directory/1}.

@item{@tt{engine}} The path of the @concept{Ciao engine builtins}.

@item{@tt{.}} The current path (@tt{'.'}).

@end{description}
").


:- trust success file_search_path(X,Y) => (gnd(X), gnd(Y)).
:- multifile file_search_path/2.
:- dynamic file_search_path/2.

:- doc(library_directory(Path), "@var{Path} is a library path (a
   path represented by the @concept{path alias} @tt{library}). More
   library paths can be defined by asserting new facts (or clauses) of
   this predicate.").

:- trust success library_directory(X) => gnd(X).
:- multifile library_directory/1.
:- dynamic library_directory/1.


:- doc(absolute_file_name(RelFileSpec, AbsFileSpec), "If
   @var{RelFileSpec} is an absolute pathname then do an absolute
   lookup. If @var{RelFileSpec} is a relative pathname then prefix the
   name with the name of the current directory and do an absolute
   lookup.  If @var{RelFileSpec} is a path alias, perform the lookup
   following the path alias rules (see @prop{sourcename/1}). In all
   cases: if a matching file with suffix @tt{.pl} exists, then
   @var{AbsFileSpec} will be unified with this file.  Failure to open
   a file normally causes an exception.  The behaviour can be
   controlled by the @tt{fileerrors} @concept{prolog flag}.").


:- pred absolute_file_name(+RelFileSpec, -AbsFileSpec) 
   :: sourcename * atm + native
   # "@var{AbsFileSpec} is the absolute name (with full path) of 
      @var{RelFileSpec}.".

absolute_file_name(File, Abs) :-
        File==user, !, Abs=user.
absolute_file_name(File, Abs) :-
        absolute_file_name(File, '_opt', '.pl', '.', Abs, _, _).

:- doc(
   absolute_file_name(Spec, Opt, Suffix, CurrDir, AbsFile, AbsBase, AbsDir),
   "@var{AbsFile} is the absolute name (with full path) of @var{Spec},
   which has an optional first suffix @var{Opt} and an optional second
   suffix @var{Suffix}, when the current directory is @var{CurrDir}.
   @var{AbsBase} is the same as @var{AbsFile}, but without the second
   suffix, and @var{AbsDir} is the absolute path of the directory where
   @var{AbsFile} is.  The Ciao compiler invokes this predicate with
   @var{Opt}=@tt{'_opt'} and @var{Suffix}=@tt{'.pl'} when searching
   source files.").


:- pred absolute_file_name(+sourcename,+atm,+atm,+atm,-atm,-atm,-atm)
	+ native.

% Error catching for three arguments predicates of absolute_file_name/7
abs_file_name_check_atom(Arg, Number):-
        (
            atom(Arg) -> 
            true 
        ; 
            throw(error(type_error(atom, Arg), absolute_file_name/7-Number))
        ).

absolute_file_name(Spec, Opt, Suffix, CurrDir, AbsFile, AbsBase, AbsDir) :-
        abs_file_name_check_atom(Opt, 2),
        abs_file_name_check_atom(Suffix, 3),
        abs_file_name_check_atom(CurrDir, 4),
        '$absolute_file_name_checked'(Spec, Opt, Suffix, CurrDir, 
                                      AbsFile, AbsBase, AbsDir).

:- export(fixed_absolute_file_name/2).
% TODO: There is a problem with absolute_file_name/?, reported by
% Paulo Moura (it duplicates the last name -- this is necessary for
% locating modules but really strange for the end user).
fixed_absolute_file_name(X, Y) :-
	Dummy = '/(((...D-U-M-M-Y...)))',
	atom_concat(X, Dummy, X1),
	absolute_file_name(X1, Y1),
	atom_concat(Y, Dummy, Y1).

:- doc(stream/1, "Streams correspond to the file pointers used at
   the operating system level, and usually represent opened files.
   There are four special streams which correspond with the operating
   system standard streams:
@begin{description}

@item{@tt{user_input}} The standard input stream, i.e. the terminal, usually.

@item{@tt{user_output}} The standard output stream, i.e. the terminal, usually.

@item{@tt{user_error}} The standard error stream.

@item{@tt{user}} The standard input or output stream, depending on context.

@end{description}

").

:- true prop stream_alias(S) + regtype # "@var{S} is the alias of an
open stream, i.e., an atom which represents a stream at Prolog
level.".

%  This does not capture the "not closed" fact
stream_alias(user_input).
stream_alias(user_output).
stream_alias(user_error).


:- true prop stream(S) + regtype # "@var{S} is an open stream.".

%  This does not capture the "not closed" fact
stream(S):- stream_alias(S).
stream(user).    %% 'user' is special: its mode depends on context!
stream('$stream'(X,Y)) :- 
        int(X), int(Y).

% TODO: We need to distinguish sourcename from plain pathnames. Most
%   of those expansions are applied to all pathnames in os_utils.c

:- doc(sourcename/1, "A source name is a flexible way of referring
   to a concrete file. A source name is either a relative or absolute
   filename given as:

@begin{itemize}

@item an atom, or

@item a unary functor (which represents a @index{path alias}, see
      below) applied to a @em{relative} path, the latter being given
      as an atom.

@end{itemize}

In all cases certain filename extensions (e.g., @tt{.pl}) can be
implicit.  In the first form above, file names can be relative to the
current directory.  Also, file names beginning with @tt{~} or @tt{$}
are treated specially.  For example,

@begin{description}

@item{@tt{'~/ciao/sample.pl'}}
is equivalent to @tt{'/home/staff/herme/ciao/sample.pl'}, if
@tt{/home/staff/herme} is the user's home directory.  (On POSIX
systems, this is equivalent to @tt{'$HOME/ciao/sample.pl'} as
explained below.)

@item{@tt{'~bardo/prolog/sample.pl'}}
is equivalent to @tt{'/home/bardo/prolog/sample.pl'}, if
@tt{/home/bardo} is @tt{bardo}'s home directory.

@item{@tt{'$UTIL/sample.pl'}}
is equivalent to @tt{'/usr/local/src/utilities/sample.pl'}, if
@tt{/usr/local/src/utilities} is the value of the environment variable
@tt{UTIL}.

@end{description}

   The second form allows using path aliases. Such aliases allow
   refering to files not with absolute file system paths but with
   paths which are relative to predefined (or user-defined) abstract
   names.  For example, given the path alias @tt{myutils} which has
   been defined to refer to path @tt{'/home/bardo/utilities'}, if that
   directory contains the file @tt{stuff.pl} then the term
   @tt{myutils(stuff)} in a @decl{use_module/1} declaration would
   refer to the file @tt{'/home/bardo/utilities/stuff.pl'} (the
   @tt{.pl} extension is implicit in the @decl{use_module/1}
   declaration).  As a special case, if that directory contains a
   subdirectory named @tt{stuff} which in turn contains the file
   @tt{stuff.pl}, the same term would refer to the file
   @tt{'/home/bardo/utilities/stuff/stuff.pl'}.  If a path alias is
   related to several paths, all paths are scanned in sequence until a
   match is found.  For information on predefined path aliases or how
   to define new path aliases, see @pred{file_search_path/2}.  ").

:- true prop sourcename(F) + regtype # "@var{F} is a source name.".

sourcename(S) :- atm(S).
sourcename(S) :- struct(S).

:- doc(open_option_list/1, "A list of options for @pred{open/4},
   currently the meaningful options are:

   @begin{description}

   @item{@tt{lock}} Try to set an advisory lock for the file.  If the
     open mode is @tt{read}, the lock is a read (shared) lock, else it
     is a write (exclusive) lock.  If the lock cannot be acquired, the
     call waits until it is released (but can fail in exceptional cases).

   @item{@tt{lock_nb}} Same as @tt{lock}, but the call immediately fails
     if the lock cannot be acquired.

   @item{@tt{lock(Lock_Mode)}} Same as @tt{lock}, but specifying in
     @tt{Lock_Mode} whether the lock is @tt{read} (also @tt{shared}) or
     @tt{write} (also @tt{exclusive}).  This option has be included for
     compatibility with the SWI-Prolog locking options, because in
     general the type of lock should match the open mode as in the
     @tt{lock} option.

   @item{@tt{lock_nb(Lock_Mode)}} Same as the previous option but with the 
     @tt{lock_nb} behavior.

   @end{description}

   All file locking is implemented via the POSIX function fcntl().
   Please refer to its manual page for details.").

:- true prop open_option_list(L) + regtype
        # "@var{L} is a list of options for @pred{open/4}.".

open_option_list(L) :- list(L, open_option).

:- true prop open_option(O) + regtype
        # "@var{O} is an option for @pred{open/4}.".

open_option(lock).
open_option(lock_nb).
open_option(lock(Lock_Mode)) :- lock_mode(Lock_Mode).
open_option(lock_nb(Lock_Mode)) :- lock_mode(Lock_Mode).

:- push_prolog_flag(multi_arity_warnings,off).

:- true prop lock_mode(M) + regtype.

lock_mode(read).
lock_mode(shared).
lock_mode(write).
lock_mode(exclusive).

:- pop_prolog_flag(multi_arity_warnings).


:- trust pred pipe(-stream, -stream) + (iso, native).
:- impl_defined([pipe/2]).
