:- module(bundle_paths, [], [assertions, fsyntax, hiord, regtypes, isomodes]).

:- doc(title, "Extended symbolic path names (for bundles)").
:- doc(author, "Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "Mapping between symbolic path names (bundle source
   code, build areas, compiled code, and documentation) and the
   underlying file system.").

:- use_module(engine(internals),
	[ciao_path/1,
	 '$bundle_id'/1,
	 '$bundle_srcdir'/2]).
:- use_module(library(system), [working_directory/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), 
	[path_concat/3,
	 path_relocate/4, 
	 path_split_list/2, 
	 path_split/3,
	 path_get_relative/3,
	 path_splitext/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [append/3]).

% ===========================================================================
:- doc(section, "Paths inside a bundle").

:- export(bundle_path/3).
:- pred bundle_path(+Bundle, +Rel, -Path) # "Obtain the absolute path
   @var{Path} for the path @var{Rel} relative to the source code
   location of @var{Bundle}".

bundle_path(Bundle, Rel, Path) :-
	bundle_path(Bundle, src, Rel, Path).

:- export(bundle_path/4).
:- pred bundle_path(+Bundle, +Rel, -Path) # "Obtain the absolute path
   @var{Path} for the path @var{Rel} relative to the source code
   location of @var{Bundle}".

bundle_path(Bundle, _Location, _Rel, _Path) :-
	var(Bundle), !, throw(error(bad_bundle, bundle_path/4)).
bundle_path(_Bundle, Location, _Rel, _Path) :-
	var(Location), !, throw(error(bad_location, bundle_path/4)).
bundle_path(_Bundle, _Location, Rel, _Path) :-
	var(Rel), !, throw(error(bad_rel, bundle_path/4)).
bundle_path(Bundle, Location, Rel, Path) :- !,
	loc_path(Location, Bundle, X),
	( Rel = '.' -> Path = X ; path_concat(X, Rel, Path) ).

loc_path(src, Bundle, R) :-
	'$bundle_srcdir'(Bundle, R).
loc_path(builddir, Bundle, R) :-
	% Base for bundle build
	bundle_to_bldid(Bundle, BldId),
	( BldId = inpath(Path) -> R0 = Path ; '$bundle_srcdir'(ciao, R0) ),
	R = ~path_concat(R0, ~relbuild(build)).
loc_path(bootbuilddir, _Bundle, R) :-
	% Base for bundle build (boot)
	'$bundle_srcdir'(ciao, R0),
	R = ~path_concat(R0, ~relbuild(bootbuild)).

% relative directory names for builddir (see sh scripts too)
relbuild(build, 'build'). % normal build
relbuild(bootbuild, 'build-boot'). % bootstrap build

% BldId corresponding to Bundle
bundle_to_bldid(Bundle, BldId) :-
	( '$bundle_srcdir'(Bundle, Dir), % (may fail for 'ciao')
	  ciao_path(Path), % TODO: Must not include CIAOROOT!
	  ( Path = Dir
	  ; path_get_relative(Path, Dir, _)
	  ) -> % Dir is relative to Path
	    BldId = inpath(Path)
	; % otherwise assume local (under CIAOROOT)
	  BldId = build
	).

% ---------------------------------------------------------------------------

:- export(bundle_metasrc/3).
:- pred bundle_metasrc(+Bundle, +What, -Path) :: atm * atm * atm
   # "Path to bundle definition files (manifests and hooks)".

% TODO: merge with bundle_path/4?
bundle_metasrc(Bundle, What) := ~bundle_path(Bundle, ~metasrc_(What, Bundle)).

% TODO: it does not consider 'Manifest.pl' (without 'Manifest/')
metasrc_(bundle_manifest, _) := 'Manifest/Manifest.pl'.
metasrc_(bundle_hooks, Bundle) := ~path_concat('Manifest', ~atom_concat(Bundle, '.hooks.pl')).
metasrc_(bundle_config, Bundle) := ~path_concat('Manifest', ~atom_concat(Bundle, '.config.pl')).

% ---------------------------------------------------------------------------

:- export(reverse_bundle_path/3).
:- pred reverse_bundle_path(+F, -Bundle, -Rel) :: atm * atm * atm
   # "Get @var{Bundle} and relative path @var{R} for absolute path
      @var{F}.  Find best (largest bundle dir) match.".

reverse_bundle_path(F, Bundle, R) :-
	findmax(P0, match_bndl(F, P0), match_len, t(Bundle, _, R)).

match_bndl(F, t(Bundle, BundleDir, R)) :-
	'$bundle_id'(Bundle),
	'$bundle_srcdir'(Bundle, BundleDir),
	path_relocate(BundleDir, '', F, R).

match_len(t(_, X, _), Len) :- atom_length(X, Len).

% ===========================================================================
:- doc(section, "Bundle-aware absolute file name").

:- export(ext_sourcename/1).
:- regtype ext_sourcename(X) # "@var{X} is an extended symbolic
   reference to a file name (see @regtype{sourcename/1}), which
   accepts paths of the form @tt{at_bundle(Bundle,RelPath)}.".

ext_sourcename(at_bundle(Bundle, Rel)) :- atm(Bundle), atm(Rel).
ext_sourcename(X) :- sourcename(X).

:- export(ext_absolute_file_name/3).
:- pred ext_absolute_file_name(+Path, +CurrDir, -AbsFile) :: ext_sourcename * atm * atm 
   # "Like @pred{fixed_absolute_file_name/3} but allows @tt{at_bundle(Bundle,RelPath)} paths.".

ext_absolute_file_name(at_bundle(Bundle, RelPath), _CurrDir, AbsFile) :- !,
	bundle_path(Bundle, RelPath, AbsFile).
ext_absolute_file_name(Path, CurrDir, AbsFile) :-
	fixed_absolute_file_name(Path, CurrDir, AbsFile).

:- export(ext_find_pl_filename/3).
:- pred ext_find_pl_filename(+File, +CurrDir, -AbsFile) :: atm * ext_sourcename * atm
   # "Resolve a @regtype{ext_sourcename/1},  .".

ext_find_pl_filename(File, CurrDir, AbsFile) :-
	( File = at_bundle(Bundle, Rel) -> bundle_path(Bundle, Rel, AbsFile)
	; absolute_file_name(File, '', '.pl', CurrDir, _, AbsFile, _)
	).

% ---------------------------------------------------------------------------

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

% E.g.,
%   .../ciao/foo/lists.pl => library(lists)
%     if 'foo' is a 'library' alias path of ciao
%   (otherwise, P = F)
reverse_find_pl_filename(F, P) :-
	findmax(P0, match_alias(F, P0), match_len, t(Alias, _, Rel)),
	tidy_rel(Rel, Rel2),
	P =.. [Alias, Rel2].

match_alias(F, t(Alias, AliasPath, R)) :-
	file_search_path(Alias, AliasPath),
	path_get_relative(AliasPath, F, R).

tidy_rel(Rel, Rel2) :-
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
	list_to_slash(Ns1, Rel2).

% ---------------------------------------------------------------------------

:- export(reverse_ext_find_pl_filename/2).
:- pred reverse_ext_find_pl_filename(+AbsFile, -Spec) :: atm * ext_sourcename
   # "Find most specific @var{P} such that @tt{ext_find_pl_filename('', P, F)}.".
% E.g.,
%   .../core/foo/lists.pl => library(lists)
%     if 'foo' is a 'library' alias path of 'core' bundle
%
%   .../core/foo/lists.pl => at_bundle(core, 'foo/lists.pl')
%     if 'foo' is not in an alias path of 'core' bundle
%
%   (otherwise, Spec = AbsFile)

reverse_ext_find_pl_filename(AbsFile, Spec) :- atom(AbsFile), !,
	( reverse_find_pl_filename(AbsFile, Spec1) ->
	    Spec = Spec1
	; reverse_bundle_path(AbsFile, Bundle, R) ->
	    Spec = at_bundle(Bundle, R)
	; Spec = AbsFile
	).
reverse_ext_find_pl_filename(AbsFile, P) :- P = AbsFile.

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

:- meta_predicate findmax(?, goal, pred(2), ?).
findmax(V, Goal, SizeP, Max) :-
	findall(V, Goal, Ps),
	largest(Ps, SizeP, Max).

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

