:- module(pathnames, [], [assertions, isomodes, nativeprops]).

:- doc(title, "File path names").

:- doc(author, "Jose F. Morales").

:- doc(module, "A @concept{pathname} is an symbolic identifier that
   locates a file in a filesystem (e.g., @tt{foo/bar/baz.txt}). This
   module provides predicates to manipulate pathnames, encoded as
   atoms. No file system access is required for pathname
   manipulation.").

% ---------------------------------------------------------------------------

:- use_module(library(lists), [append/3, reverse/2]).
:- use_module(engine(internals), ['$path_is_absolute'/1]). % TODO: merge?

% ---------------------------------------------------------------------------

:- export(pathname/1).
:- prop pathname(X) + regtype
	# "@var{X} is a pathname (encoded as an atom)".

pathname(X) :- atm(X).

% ---------------------------------------------------------------------------

:- export(path_is_absolute/1).
:- pred path_is_absolute(+Path) :: pathname
   # "@var{Path} is an absolute pathname".

path_is_absolute(Path) :- '$path_is_absolute'(Path).
%	atom_concat('/', _, Path).

:- export(path_is_relative/1).
:- pred path_is_relative(+Path) :: pathname
   # "@var{Path} is a relative pathname".

path_is_relative(Path) :-
	\+ path_is_absolute(Path).

:- export(path_is_basename/1).
:- pred path_is_basename(+Path) :: pathname
   # "@var{Path} is a basename (empty directory part) (equivalent to
     @tt{path_split(Path, '', _)}".

path_is_basename(Path) :-
	path_split(Path, '', _).

:- export(path_is_root/1).
:- pred path_is_root(+Path) :: pathname
   # "@var{Path} is a root directory (normalized into @tt{/} or
     @tt{//}) (equivalent to @tt{\+ Path = '', path_split(Path, Path,
     '')}".

path_is_root(Path) :-
	\+ Path = '',
	path_split(Path, Path, '').

% ---------------------------------------------------------------------------

:- export(path_concat/3).
:- pred path_concat(+PathA, +PathB, ?Path)
	:: pathname * pathname * pathname
   # "Concatenate pathnames @var{PathA} and @var{PathB} in a new path
      @var{Path}, adding a @tt{/} separator if needed. If @var{PathB}
      is @tt{''}, then @var{Path} is a @tt{/} ended path. If
      @var{PathB} is an absolute pathname, @var{Path} is
      @var{PathB}. No pathname normalization is performed in any
      case.".

path_concat('', B, R) :- !, R = B.
path_concat(_A, B, R) :- path_is_absolute(B), !,
	R = B.
path_concat(A, B, R) :-
	( atom_concat(_, '/', A) ->
	    A0 = A
        ; atom_concat(A, '/', A0) % add '/' if needed
	),
	atom_concat(A0, B, R).

% ---------------------------------------------------------------------------

:- export(path_split/3).
:- pred path_split(+Path, +Dir, ?Base) ::
	pathname * pathname * pathname
   # "Split @var{Path} into the directory part @var{Dir} and the
      basename part @var{Base}.".

:- doc(path_split(Path, Dir, Base),
   "Given a pathname @var{Path}, @var{Base} is the last component of
    the path and @var{Dir} is the rest of the path. The following
    rules must hold:

    @begin{itemize}
    @item All trailing slashes from @var{Dir} are removed (except if
      it is a root directory containing one or more slashes, e.g.,
      @tt{'/'}, @tt{'//'}).
    @item If @var{Path} ends in a slash (@tt{'}...@tt{/'}, @var{Base}
      is empty (@tt{''}).
    @item If @var{Path} is empty, both @var{Dir} and @var{Base} are
      empty.
    @item Concatenating @var{Dir} and @var{Base} results in pathname
      that is equivalent (modulo normalization) to @var{Path}. That
      is, for all @var{A}, @var{B}, @var{C}:
      @begin{verbatim}
        path_split(A,B,C),
        path_concat(B,C,D),
        path_norm(A,An), path_norm(D,Dn), An = Dn.
      @end{verbatim}
    @end{itemize}").

path_split(Path, Dir, Base) :-
	atom_codes(Path, PathS),
	path_split_(PathS, DirS, BaseS),
	atom_codes(Dir, DirS),
	atom_codes(Base, BaseS).

path_split_(Path, Dir, Base) :-
	reverse(Path, R),
	( append(BaseR, "/"||DirR, R) ->
	    anysep(DirR, DirR2), % Strip all trailing /
	    ( DirR2 = "" -> % (Dir is root, preserve all /)
	        Dir = "/"||DirR
	    ; reverse(DirR2, Dir)
	    ),
	    reverse(BaseR, Base)
	; Dir = "",
	  Base = Path
	).

% ---------------------------------------------------------------------------

:- export(path_norm/2).
:- pred path_norm(+Path, ?NormPath)
	:: pathname * pathname
   # "@var{NormPath} is the normalized pathname version of
      @var{Path}.".

:- doc(path_norm(Path, NormPath),
   "@var{NormPath} is the normalized pathname version of
    @var{Path}. Normalization removes redundant separators (@tt{//}
    into @tt{/}), and collapses references to the parent (@tt{..})
    and current (@tt{.}) level. The parent of a root path (@tt{/} or
    @tt{//}) is itself. Empty paths (@tt{''}) or relative paths where
    all parent references are collapsed are normalized as @tt{'.'}.

    For compatibility, leading double-slashes are preserved in
    normalization (the POSIX.2 standard states that \"a pathname that
    begins with two successive slashes may be interpreted in an
    implementation-defined manner, although more than two leading
    slashes shall be treated as a single slash.\"). Some systems (like
    Linux) ignore double-slashes while others (like Cygwin for
    @tt{//hostname/path} SMB network drives) do not.

    Note that @pred{path_norm/2} does not access the filesystem, which
    may affect the semantics when symbolic links are used (and no path
    normalization is involved). E.g., the following query obtain
    different values for @var{S1} and @var{S2}:
    
@begin{verbatim}
?- P = '/usr/donotexists/..',
   path_norm(P, N),
   process_call(path(test), ['-e', P], [status(S1)]),
   process_call(path(test), ['-e', N], [status(S2)]).

N = '/usr',
P = '/usr/donotexists/..',
S1 = 1,
S2 = 0 ? 

?- use_module(library(process)).
?- copy_file('/usr', 'u', [symlink]),
   P = 'u/../u',
   path_norm(P, N),
   process_call(path(test), ['-e', P], [status(S1)]),
   process_call(path(test), ['-e', N], [status(S2)]),
   delete_file('u').

N = u,
P = 'u/../u',
S1 = 1,
S2 = 0 ? 

yes
@end{verbatim}
").

:- doc(bug, "Some system predicates do unexpected path normalization
   (which also changes the semantics when symbolic links are
   used). Document at least.").

path_norm(Path, NormPath) :-
	atom_codes(Path, PathS),
	path_norm_(PathS, NormPathS),
	atom_codes(NormPath, NormPathS).

path_norm_(Xs, Ys) :-
	parse_names(Xs, Nss),
	collapse_names(Nss, Nss2),
	compose_names(Nss2, Ys).

compose_names([], Ys) :- !, Ys = ".".
compose_names([Ns|Nss], Ys) :-
	append(Ns, Ys1, Ys),
	( Nss = [],
	    ( Ns = "" ; Ns = "/" ) % (root)
	->
	    Ys1 = "/"
	; Nss = [] -> Ys1 = []
	; Ys1 = "/"||Ys0,
	  compose_names(Nss, Ys0)
	).

% Collapse references to '..' and '.'
collapse_names(Xs, Ys) :-
	collapse_names_(Xs, [], Rs),
	reverse(Rs, Ys).

% (Ss is the stack of names)
collapse_names_([], Ss, Ss).
collapse_names_([X|Xs], S, Rs) :-
	( X = "." -> S1 = S % nothing
	; % ignore '..' if previous is single or double root
	  X = "..", ( S = [""] ; S = ["/"] ) ->
	    S1 = S
	; % try pop (except if previous is '..')
	  X = "..", S = [Y|S0], \+ Y = ".." ->
	    S1 = S0
	; % otherwise, push
	  S1 = [X|S]
	),
	collapse_names_(Xs, S1, Rs).

% Parse all '/' separated names
parse_names("//"||Xs, Nss) :- \+ Xs = "/"||_, !, % (exception required by POSIX)
	Nss = ["/"|Nss0],
	parse_names_(Xs, Nss0).
parse_names(Xs, Nss) :-
	parse_names_(Xs, Nss).

parse_names_([], []) :- !.
parse_names_(Xs, [Ns|Nss]) :-
	parse_name(Xs, Ns, Xs0),
	parse_names_(Xs0, Nss).

% Parse a name ended with a separator (collapsing '/') or end of string
parse_name([], Ns, []) :- !, Ns = [].
parse_name("/"||Xs, Ns, Ys) :- !, Ns = [], anysep(Xs, Ys).
parse_name([X|Xs], [X|Ns], Ys) :-
	parse_name(Xs, Ns, Ys).

% Zero or more '/'
anysep("/"||Xs, Ys) :- !, anysep(Xs, Ys).
anysep(Xs, Xs).

% Parse a separator (collapsing '/')
getsep("/"||Xs, Ys) :- getsep_(Xs, Ys).

getsep_("/"||Xs, Ys) :- !, getsep_(Xs, Ys).
getsep_(Xs, Xs).

% ---------------------------------------------------------------------------

:- export(path_splitext/3).
:- pred path_splitext(+Path,?NoExt,?Ext) ::
	pathname * pathname * pathname
   # "Split @var{Path} into its extension @var{Ext} and the rest of
      the pathname @var{NoExt}.".

:- pred path_splitext(?Path,+NoExt,+Ext) ::
	pathname * pathname * pathname
   # "Compose @var{Path} by concatenating the extension @var{Ext} to
      @var{NoExt} pathname.".

:- doc(path_splitext(Path,NoExt,Ext),
   "The extension @var{Ext} is the shortest suffix that begins with
    @tt{'.'} of @var{Path}. The rest of the pathname is @var{NoExt},
    which cannot be empty (@tt{''}). The extension is @tt{''} if the
    pathname has no extension.

    In this example, all the following goals succeed:
@begin{verbatim}
path_splitext('a/foo.', 'a/foo', '.')
path_splitext('a/foo.c', 'a/foo', '.c')
path_splitext('a/foo.c.d', 'a/foo.c', '.d')
path_splitext('a/.foo.', 'a/.foo', '.')
path_splitext('a/.foo.c', 'a/.foo', '.c')
path_splitext('a/.foo.c.d', 'a/.foo.c', '.d')
@end{verbatim}").

path_splitext(Path, NoExt, Ext) :-
	var(Path),
	!,
	NoExt \== '', % not valid % TODO: document
	% TODO: any missing bad case? (e.g., cannot add extensions do 'all dots' NoExt)
	atom_concat(NoExt, Ext, Path).
path_splitext(Path, NoExt, Ext) :-
	atom_codes(Path, PathS),
	path_splitext_(PathS, NoExtS, ExtS),
	atom_codes(NoExt, NoExtS),
	atom_codes(Ext, ExtS).

path_splitext_(Path, NoExt, Ext) :-
	reverse(Path, R),
	( append(ExtR, "."||NoExtR, R),
	  \+ alldots(NoExtR)
	->
	    reverse(NoExtR, NoExt),
	    reverse(ExtR, Ext0),
	    Ext = "."||Ext0
	; NoExt = Path,
	  Ext = ""
	).

% all dots until / or end
alldots([]).
alldots("/"||_) :- !.
alldots("."||Xs) :- !, alldots(Xs).

% ---------------------------------------------------------------------------

:- export(path_basename/2).
:- pred path_basename(+Path,?Base) :: pathname * pathname
   # "@var{Base} is the basename corresponding to the
      @var{Path} (equivalent to @tt{path_split(Path,_,Base)}).".

path_basename(Path, Base) :-
	path_split(Path, _, Base).

:- export(path_dirname/2).
:- pred path_dirname(+Path,?Dir) :: pathname * pathname
   # "@var{Dir} is the directory part corresponding to the @var{Path}
      (equivalent to @tt{path_split(Path,Dir,_)}).".

path_dirname(Path, Dir) :-
	path_split(Path, Dir, _).

% ---------------------------------------------------------------------------

:- export(path_relocate/4).
:- pred path_relocate(+FromDir, +ToDir, +FromPath, -ToPath)
	:: pathname * pathname * pathname * pathname
   # "Replace @var{FromDir} prefix by @var{DestDir} in @var{FromDir}
      to generate @var{ToDir}".

path_relocate(FromDir, ToDir, FromPath, ToPath) :-
	FromDir = FromPath, !,
	ToPath = ToDir.
path_relocate(FromDir, ToDir, FromPath, ToPath) :-
	path_concat(FromDir, '', FromDir2), % ensure we have a trailing /
	atom_concat(FromDir2, Suffix, FromPath),
	path_concat(ToDir, Suffix, ToPath).

:- export(path_get_relative/3).
:- pred path_get_relative(+BaseDir, +Path, -RelPath)
	:: pathname * pathname * pathname
   # "Obtain path @var{RelPath} such that @tt{path_concat(BaseDir,
      RelPath, Path)} @var{RelPath} will not contain any trailing
      @tt{'/'}".

path_get_relative(BaseDir, Path, RelPath) :-
	path_concat(BaseDir, '', BaseDir2),
	atom_concat(BaseDir2, RelPath, Path).

% ---------------------------------------------------------------------------

:- export(path_split_list/2).
:- pred path_split_list(+Path, ?Bases) ::
	pathname * list(pathname)
   # "Split @var{Path} into its components @var{Bases}, calling
      @pred{path_split/3} recursively.".

path_split_list(Path, Ns) :-
	path_split_list_(Path, [], Ns).

path_split_list_(Path, Ns0, Ns) :-
	path_split(Path, Path0, N),
	( Path = Path0, N = '' -> % Path is root
	    Ns = [Path0|Ns0]
	; Path0 = '' -> % Base case
	    Ns = [N|Ns0]
	; Ns1 = [N|Ns0],
	  path_split_list_(Path0, Ns1, Ns)
	).

:- export(path_concat_list/2).
:- pred path_concat_list(+Bases, ?Path)
	:: list(pathname) * pathname
   # "Concatenate all components in @var{Bases} a new path @var{Path},
      calling @pred{path_concat/3} recursively.  ".

path_concat_list(Ns, Path) :-
	path_concat_list_(Ns, '', Path).

path_concat_list_(Ns, Path0, Path) :-
	( Ns = [] ->
	    Path = Path0
	; Ns = [N|Ns0] ->
	    path_concat(Path0, N, Path1),
	    path_concat_list_(Ns0, Path1, Path)
	; fail
	).

% ---------------------------------------------------------------------------

:- doc(bug, "@pred{path_norm/2} and @tt{expand_file_name} in
   @tt{os_utils.c} should be equivalent (disabling expansion of home
   and env vars)").

:- doc(bug, "Windows paths (drive letters) are not supported (see
   @tt{core/bugs/Pending/expand_file_trailing_slash/test.pl} for some
   test cases)").

:- doc(bug, "This library offers no reversible versions of many of its
   predicates (e.g., @pred{path_concat/3}) due to nondeterminism of
   reverse normalization. E.g., @tt{path_concat(a,B,'a/b'),B='b///'}
   would fail unless all unnormalized paths are enumerated by the
   first call.

   Reversible path operations can be implemented by decomposing paths
   in list of names (using @pred{path_split/2} recursively),
   manipulating the lists, and composing back the paths (using
   @pred{path_concat/2} recursively).").

% ---------------------------------------------------------------------------




