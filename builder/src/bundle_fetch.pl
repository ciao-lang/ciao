:- module(_, [], [dcg, fsyntax, hiord, assertions, regtypes, isomodes, datafacts]).

:- doc(title,  "Bundle fetch").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements support for fetching bundles
   and their dependencies from remote locations (e.g., the network).

   The predicate @pred{bundle_fetch/2} fetches a bundle and its
   dependencies, as follows:

   @begin{itemize}
   @item if the bundle is in a non-top workspace, stops fetching
     (and do not try to fetch dependencies)
   @item if the bundle is missing, retreive and store it at the
     @em{top workspace} (see @tt{CIAOPATH} environment)
   @item if the bundle is in the top workspace, skips fetching
   @item try fetch bundle dependencies
   @end{itemize}

   A fetched bundle is automatically marked to distinguish them from
   @em{user} bundles, which are manually written by the user. It is
   possible to fetch a user bundle, e.g., to retreive all their
   dependencies.

   Fetched bundles can be removed with @pred{bundle_rm/1}. Removing a
   @em{user} bundle is not allowed with @pred{bundle_rm/1} (it must be
   done manually).

   The location and fetch protocol is automatically recognized based
   on @em{bundle alias}es. See @pred{bundle_src_origin/2} for
   translation from bundle aliases to recognized origins (e.g.,
   @tt{ciao-lang.org/foo} downloads and uncompress a @tt{git}
   archive).
").

:- use_module(library(lists), [member/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(http_get), [http_get/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system), [delete_directory/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(source_tree), [remove_dir/1]).

:- use_module(library(bundle/bundle_info), [
  bundle_status/2, bundle_set_status_fetch/1
]).

:- use_module(ciaobld(bundle_scan), [scan_bundles_at_path/1]).
:- use_module(ciaobld(messages_aux), [normal_message/2]).
:- use_module(engine(internals), [top_ciao_path/1]).

% ---------------------------------------------------------------------------
:- doc(section, "Bundle origins for fetching").

% TODO: Multiple origins? Error or try alternatives?
% TODO: Save origin in FETCHED_BUNDLE mark? (or somewhere else)
% TODO: Customize default origin?

:- data bundle_origin/2.

:- export(add_bundle_origin/2).
:- pred add_bundle_origin(Bundle, Origin) 
   # "Select the origin for the given bundle name".

add_bundle_origin(Bundle, Origin) :-
	( current_fact(bundle_origin(Bundle, _)) -> true
	; assertz_fact(bundle_origin(Bundle, Origin))
	).

:- export(get_bundle_origin/2).
:- pred get_bundle_origin(Bundle, Origin) 
   # "Obtain the origin for a bundle (for fetching). Use the
      default origin if unknown (@pred{default_origin/2}).".

get_bundle_origin(Bundle, Origin) :-
	( current_fact(bundle_origin(Bundle, Origin0)) ->
	    Origin = Origin0
	; default_origin(Bundle, Origin)
	  % throw(error_msg("Unknown origin for bundle '~w'", [Bundle]))
	).

:- doc(doinclude, default_origin/2).
:- pred default_origin(Bundle, Origin)
   # "Default origin when none is specified:
     @includedef{default_origin/2}".

default_origin(Bundle, Origin) :-
	path_concat('github.com/ciao-lang', Bundle, Path),
	Origin = github(Path, master).

:- export(bundle_fetch_cleanup/0).
:- pred bundle_fetch_cleanup # "Cleanup the bundle fetch state".
bundle_fetch_cleanup :-
	retractall_fact(bundle_origin(_, _)).

% ---------------------------------------------------------------------------

:- export(check_bundle_alias/3).
:- pred check_bundle_alias(+BundleAlias, -Origin, -Bundle)
   # "Given a URL-like @var{BundleAlias}, obtain its @var{Origin} term
      for fetch and the bundle name @var{Name}. Throws an exception if
      malformed and fails silently if it is not a URL-like alias.".

check_bundle_alias(BundleAlias, Origin, Bundle) :-
	( check_valid_alias(BundleAlias),
	  path_split(BundleAlias, _, Bundle0),
	  bundle_src_origin(BundleAlias, Origin0) ->
	    Bundle = Bundle0,
	    Origin = Origin0
	; fail % Not a bundle alias
	).

% Check that bundle alias is well formed
% TODO: allow? add as normalization?
check_valid_alias(BundleAlias) :-
	invalid_protocol(P),
	atom_concat(P, BundleAlias0, BundleAlias),
	!,
	throw(error_msg("Invalid bundle alias, do you mean ~w? (without ~w).~n", [BundleAlias0, P])).
check_valid_alias(_).

invalid_protocol('http://').
invalid_protocol('https://').
invalid_protocol('ssh://').

% (silently fail if it does not look like an URL)
bundle_src_origin(BundleAlias, github(Path, Ref)) :-
	% Github.com
	atom_concat('github.com/', _, BundleAlias),
	!,
	Ref = master, % TODO: customize Ref
	Path = BundleAlias.
bundle_src_origin(BundleAlias, gitlab(Path, Ref)) :-
	% Some gitlab instance (gitlab.[...]/)
	atom_concat('gitlab.', _, BundleAlias),
	!,
	Ref = master, % TODO: customize Ref
	Path = BundleAlias.
bundle_src_origin(BundleAlias, git_archive(Path, Ref)) :-
	% Git repositories at ciao-lang.org
	atom_concat('ciao-lang.org/', _, BundleAlias),
	!,
	Ref = master, % TODO: customize Ref
	Path = BundleAlias.

% bundle_relative_origin(+ParentOrigin, +Bundle, -Origin):
%   Origin of Bundle based on ParentOrigin.
bundle_relative_origin(github(Path0, _Ref), Bundle, Origin) :-
	path_split(Path0, Path1, _),
	path_concat(Path1, Bundle, Path),
	Origin = github(Path, master).
bundle_relative_origin(gitlab(Path0, _Ref), Bundle, Origin) :-
	path_split(Path0, Path1, _),
	path_concat(Path1, Bundle, Path),
	Origin = gitlab(Path, master).
bundle_relative_origin(git_archive(Path0, _Ref), Bundle, Origin) :-
	path_split(Path0, Path1, _),
	path_concat(Path1, Bundle, Path),
	Origin = git_archive(Path, master).

% ---------------------------------------------------------------------------
:- doc(section, "Bundle fetching").

:- use_module(library(aggregates), [findall/3]).
:- use_module(ciaobld(manifest_compiler),
	[ensure_load_manifest/1, manifest_call/2]).

:- data fetched/1.

:- export(bundle_fetch/2).
:- pred bundle_fetch(+Bundle, -Fetched)
   # "Fetch @var{Bundle} and all its dependencies. @var{Fetched} is
     the list of newly fetched bundles.".

bundle_fetch(Bundle, Fetched) :-
	retractall_fact(fetched(_)),
	bundle_fetch0(Bundle),
	findall(B, retract_fact(fetched(B)), Fetched).

bundle_fetch0(Bundle) :-
	fetched(Bundle), !. % just fetched
bundle_fetch0(Bundle) :-
	bundle_status(Bundle, Status),
	( Status = nontop ->
	    normal_message("fetching ~w stopped (exists in a non-top workspace)", [Bundle])
	; ( Status = fetched ->
	      % TODO: allow silent operation, implement 'ciao rm'
	      normal_message("fetching ~w skipped (already fetched, remove to force upgrade)", [Bundle])
	  ; Status = user ->
	      normal_message("fetching ~w skipped (user bundle)", [Bundle])
	  ; Status = missing ->
	      % Fetch missing
	      fetch_missing(Bundle),
	      assertz_fact(fetched(Bundle))
	  ; throw(bad_status(Status))
	  ),
	  % Fetch dependencies
	  % TODO: scan only one bundle
	  % TODO: rescan here or from clients of this module?
	  top_ciao_path(RootDir),
	  scan_bundles_at_path(RootDir),
	  ensure_load_manifest(Bundle),
	  fetch_deps(Bundle)
	).

% Try fetch all missing dependencies
fetch_deps(Bundle) :-
	findall(Dep, missing_dep(Bundle, Dep), Missing),
	( % (failure-driven loop)
	  member(B, Missing),
	    bundle_fetch0(B),
	    fail
	; true
	).

% Missing dependencies
missing_dep(Bundle, Dep) :-
	manifest_call(Bundle, dep(Dep, Props)),
	bundle_status(Dep, Status),
	Status = missing,
	% TODO: Not missing and not scanned? classify as missing?
	% \+ '$bundle_id'(Dep),
	% Guess an origin
	guess_bundle_origin(Bundle, Dep, Props).

% Guess origin of Dep, used from Bundle
guess_bundle_origin(Bundle, Dep, Props) :- % TODO: support origin=... in Props?
	( current_fact(bundle_origin(Dep, _)) ->
	    % Dep already has origin
	    true
	; member(origin=DepOrigin, Props) ->
	    % get origin from Props
	    add_bundle_origin(Dep, DepOrigin)
	; % get origin relative to Bundle
	  get_bundle_origin(Bundle, Origin),
	  bundle_relative_origin(Origin, Dep, DepOrigin),
	  add_bundle_origin(Dep, DepOrigin)
	).

% ---------------------------------------------------------------------------

fetch_missing(Bundle) :-
	top_ciao_path(RootDir),
	path_concat(RootDir, Bundle, BundleDir),
	get_bundle_origin(Bundle, Origin),
	( fetch_missing_(Origin, Bundle, BundleDir) ->
	    % Mark as fetched
	    bundle_set_status_fetch(Bundle)
	; throw(error_msg("Bundle fetch failed for `~w'.", [Origin]))
	).

fetch_missing_(Origin, Bundle, BundleDir) :-
	% Fetch source
	mktemp_in_tmp('ciao-fetch-XXXXXX', File),
	fetch_src(Origin, Bundle, File),
	% Uncompress
	mkpath(BundleDir),
	process_call(path(tar), [
          '-x', '--strip-components', '1',
	  '-C', BundleDir,
	  '-f', File], [status(S)]),
	( S = 0 -> true
	; delete_directory(BundleDir), % delete on error
	  fail
	).

fetch_src(github(Path, Ref), Bundle, File) :-
	atom_concat(['https://', Path, '/archive/', Ref, '.tar.gz'], URL),
	normal_message("fetching ~w source (~w) from ~w", [Bundle, Ref, URL]),
	catch(http_get(URL, file(File)), _E, fail).
fetch_src(gitlab(Path, Ref), Bundle, File) :-
	atom_concat(['https://', Path, '/-/archive/', Ref, '.tar.gz'], URL),
	normal_message("fetching ~w source (~w) from ~w", [Bundle, Ref, URL]),
	catch(http_get(URL, file(File)), _E, fail).
fetch_src(git_archive(Path, Ref), Bundle, File) :-
	atom_concat(['ssh://gitolite@', Path], URL),
	normal_message("fetching ~w source (~w) from ~w", [Bundle, Ref, URL]),
	process_call(path(git),
	             ['archive', '--format', 'tgz', '--remote', URL,
		      '--prefix', ~atom_concat(Bundle, '/'),
		      '--output', File, Ref],
		     [status(0)]).

% ---------------------------------------------------------------------------
:- doc(section, "Remove fetched bundle").

% TODO: 'ciao rm' just removes, it does not:
%  - check broken dependencies
%  - uninstalls
%
% We must use $bundle_prop(_, dep(_)) at least to detect broken
% dependencies at runtime (while reading bundleregs).

:- export(bundle_rm/1).
% Remove the specified bundle (if it was downloaded)
bundle_rm(Bundle) :-
	top_ciao_path(RootDir),
	bundle_rm0(Bundle, RootDir),
	scan_bundles_at_path(RootDir). % TODO: rescan here or from clients of this module?

bundle_rm0(Bundle, RootDir) :-
	bundle_status(Bundle, Status),
	% Compute bundle dir and check status
	( Status = fetched ->
	    path_concat(RootDir, Bundle, BundleDir),
	    bundle_rm1(Bundle, BundleDir)
	; Status = user ->
	    normal_message("user bundle, ignoring rm", [])
	; Status = nontop ->
	    throw(error_msg("bundle `~w' exists in a non-top CIAOPATH", [Bundle]))
	; Status = missing ->
	    throw(error_msg("bundle `~w' does not exist", [Bundle]))
	; fail
	).

bundle_rm1(Bundle, BundleDir) :-
	bundle_rm2(Bundle, BundleDir),
	!.
bundle_rm1(Bundle, _) :-
	throw(error_msg("Bundle rm failed for `~w'.", [Bundle])).

bundle_rm2(_Bundle, BundleDir) :-
	remove_dir(BundleDir).

