:- module(paths_extra, [], [assertions, fsyntax, hiord, regtypes, isomodes]).

:- doc(title, "Extended symbolic path names (for bundles)").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Mapping between symbolic path names (bundle source
   code, build areas, compiled code, and documentation) and the
   underlying file system. This is parameterized by some configuration
   options").

:- use_module(engine(internals),
	['$bundle_id'/1,
	 '$bundle_srcdir'/2]).
:- use_module(library(system), [working_directory/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), 
	[path_relocate/4, 
	 path_split_list/2, 
	 path_split/3,
	 path_get_relative/3,
	 path_splitext/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [append/3]).

% ---------------------------------------------------------------------------

:- export(sourcenameB/1).
:- regtype sourcenameB(X) # "@var{X} is an extended symbolic reference
   to a file name (see @pred{fsR/2}).".

sourcenameB(_).

:- export(sourcenameX/1).
:- regtype sourcenameX(X) # "@var{X} is either a
   @regtype{sourcenameB/1} or @regtype{sourcename/1}".

sourcenameX(_).

% ---------------------------------------------------------------------------

:- export(fsR/2).
:- pred fsR(+Path, -AbsFile) :: sourcenameB * atm 
   # "@var{AbsFile} is the (absolute) file system path assigned to the
      symbolic file or directory name @var{Path}".

fsR(A) := A :- atom(A), !.
fsR(A) := _ :- var(A), !, throw(bad_fsR(A)). % TODO: fix exception
fsR(builddir(BldId)) := R :- !,
	% Directory where code and documentation is built
	( BldId = inpath(Path) -> R = ~fsR(Path/build)
	; relbuild(BldId, RelBuildDir),
	  R = ~fsR(bundle_src(ciao)/RelBuildDir)
	).
fsR(builddir_bin(BldId)) := R :- !,
	% Directory to build (executable) binaries
	R = ~fsR(builddir(BldId)/bin).
fsR(builddir_doc(BldId)) := R :- !,
	% Directory to build documentation
	R = ~fsR(builddir(BldId)/doc).
%
fsR(bundle_src(Bundle)) := R :- !,
	'$bundle_srcdir'(Bundle, R).
%
fsR(A/B) := ~concat_dir(~fsR(A), B) :- atom(B), !.
fsR(A) := _ :- throw(bad_fsR(A)). % TODO: fix exception

% TODO: use path_concat, be aware of ''
concat_dir(Dir, '') := Dir :- !. % TODO: document
concat_dir(Dir, File) := ~atom_concat([Dir, '/', File]).

% relative directory names for builddir
% TODO: see @apl{ciao_builder} scripts!
relbuild(build, 'build'). % normal build
relbuild(bootbuild, 'build-boot'). % bootstrap build

% ---------------------------------------------------------------------------

:- export(reverse_fsR/2).
:- pred reverse_fsR(+F, -P) :: atm * sourcenameB
   # "Find most specific @var{P} such that @tt{fsR(P, F)}.".
% E.g.,
%   .../core/foo/lists.pl => bundle_src(core)/foo/'lists.pl'
%   (otherwise, P = F)

reverse_fsR(F, P) :- atom(F), !,
	findall(P0, reverse_fsR_(F, P0), Ps),
	largest(Ps, bundle_dir_len, t(Bundle, _, R)),
	path_split_list(R, Ns),
	list_to_slash([bundle_src(Bundle)|Ns], P).
reverse_fsR(F, P) :- P = F.

reverse_fsR_(F, t(Bundle, BundleDir, R)) :-
	'$bundle_id'(Bundle),
	fsR(bundle_src(Bundle), BundleDir),
	path_relocate(BundleDir, '', F, R).

bundle_dir_len(t(_, BundleDir, _), Len) :-
	atom_length(BundleDir, Len).

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

:- export(reverse_absolute_file_name/2).
:- pred reverse_absolute_file_name(+F, -P) :: atm * sourcename
   # "Find most specific @var{P} such that @tt{relative_absname(P, F)}.".
% E.g.,
%   .../ciao/foo/lists.pl => library(lists)
%     if 'foo' is a 'library' alias path of ciao
%   (otherwise, P = F)

reverse_absolute_file_name(F, P) :-
	( atom(F),
	  findall(P0, reverse_absolute_file_name_(F, P0), Ps),
	  largest(Ps, aliaspath_len, t(Alias, _, Rel)) ->
	    % Remove .pl extension (if needed)
	    ( path_splitext(Rel, Rel1, '.pl') -> true
	    ; Rel1 = Rel
	    ),
	    % Split in names
	    path_split_list(Rel1, Ns),
	    % Collapse same dir/mod name (if possible)
	    ( append(Ns0, [N,N], Ns) ->
	        append(Ns0, [N], Ns1)
	    ; Ns1 = Ns
	    ),
	    % Transform to slashpath
	    list_to_slash(Ns1, Xs),
	    P =.. [Alias, Xs]
	; P = F
	).

reverse_absolute_file_name_(F, t(Alias, AliasPath, R)) :-
	file_search_path(Alias, AliasPath),
	path_get_relative(AliasPath, F, R).

aliaspath_len(t(_, AliasPath, _), Len) :-
	atom_length(AliasPath, Len).

:- export(reverse_fsRx/2).
:- pred reverse_fsRx(+F, -P) :: atm * sourcenameX
   # "Find most specific @var{P} such that @tt{fsRx(P, F)}.".
% E.g.,
%   .../core/foo/lists.pl => bundle_src(core)/foo/'lists.pl'
%     if 'foo' is not in an alias path of 'core' bundle
%     
%   .../core/foo/lists.pl => library(lists))
%     if 'foo' is a 'library' alias path of 'core' bundle
%
%   (otherwise, P = F)

reverse_fsRx(F, P) :-
	reverse_absolute_file_name(F, P1),
	( F = P1 ->
	    reverse_fsR(P1, P)
	; P = P1
	).

% ---------------------------------------------------------------------------

% TODO: integrate in fsR/2 and relative_absname/3

:- export(fsRx/2).
:- pred fsRx(+P, -F) :: sourcenameX * atm
   # "Resolve a @regtype{sourcenameB/1} or @regtype{sourcename/1}.".

fsRx(Mod, Path) :-
	( Mod = (_/_) -> fsR(Mod, Path)
	; relative_absname('', Mod, Path)
	).

:- export(fsRx_get_bundle_and_basename/3).
:- pred fsRx_get_bundle_and_basename(+P, -Bundle, -Basename) ::
     sourcenameX * atm * atm
   # "@var{Bundle} and @var{Basename} are the bundle and basename
      corresponding to @var{P}".

fsRx_get_bundle_and_basename(Spec, Bundle, ModName) :-
	fsRx(Spec, AbsFile),
	reverse_fsR(AbsFile, Path),
	slash_to_list(Path, Path1),
	Path1 = [bundle_src(Bundle)|_],
	( append(_, [ModName], Path1) -> true ; fail ).

:- export(relative_absname/3).
% IMAbs is the absolute file name of IM module spec, resolved relative
% to Base pathname.
relative_absname(Base, IM, IMAbs) :-
	( Base = '' -> CWD = '.'
	; path_split(Base, CWD, _),
	  working_directory(OldCWD,CWD)
	),
	absolute_file_name(IM, '_opt', '.pl', CWD, _, IMAbs, _),
	( Base = '' -> true
	; working_directory(_,OldCWD)
	).

% ---------------------------------------------------------------------------

% TODO: Move to another library

% TODO: Type for slashpaths is ambiguous (in A/B, A can be a term
%   '/'/2 or another slashpath)

:- regtype slashpath(X) # "@var{X} is a slashpath (@tt{_/.../_})".
slashpath(A) :- term(A). % TODO: ambiguous case (it should not be '/'/2)
slashpath(A/_) :- slashpath(A).

:- export(list_to_slash/2).
:- pred list_to_slash(+Xs, -Y) :: list * slashpath
   # "@var{Y} is the slashpath corresponding to @var{Xs}".

list_to_slash([X|Xs], Y) :-
	list_to_slash_(Xs, X, Y).

list_to_slash_([], Y, Y).
list_to_slash_([X|Xs], Y, Z) :-
	list_to_slash_(Xs, Y/X, Z).

:- export(slash_to_list/2).
:- pred slash_to_list(+X, -Ys) :: slashpath * list
   # "@var{Ys} is the list corresponding to @var{X} slashpath".

slash_to_list(X, Ys) :-
	slash_to_list_(X, Ys, []).

slash_to_list_(Y/X, Zs, Zs0) :- !,
	slash_to_list_(Y, Zs, [X|Zs0]).
slash_to_list_(X, [X|Zs], Zs).

% ---------------------------------------------------------------------------

% TODO: move to a library
% X with maximum L, so that SizeP(X,L).
:- meta_predicate largest(?, pred(2), ?).
largest([X|Xs], SizeP, R) :-
	SizeP(X, Len),
	largest_(Xs, X, Len, SizeP, R).

:- meta_predicate largest_(?, ?, pred(2), ?).
largest_([], R, _Len, _SizeP, R).
largest_([X|Xs], A, Len, SizeP, R) :-
	SizeP(X, XLen),
	( XLen > Len ->
	    A2 = X, Len2 = XLen
	; A2 = A, Len2 = Len
	),
	largest_(Xs, A2, Len2, SizeP, R).

% ---------------------------------------------------------------------------
% TODO: merge with fsR/2?

:- export(bundle_metasrc/3).
% Path to bundle definition files
bundle_metasrc(Bundle, bundle_manifest, F) :-
	% TODO: does not consider 'Manifest.pl' (without 'Manifest/')
	F = bundle_src(Bundle)/'Manifest'/'Manifest.pl'.
bundle_metasrc(Bundle, bundle_hooks, F) :-
	Actions = ~atom_concat(Bundle, '.hooks.pl'),
	F = bundle_src(Bundle)/'Manifest'/Actions.
bundle_metasrc(Bundle, bundle_config, F) :-
	Config = ~atom_concat(Bundle, '.config.pl'),
	F = bundle_src(Bundle)/'Manifest'/Config.

