:- module(_, [], [dcg, fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Bundle Fetch").
:- doc(author, "Jose F. Morales").

:- doc(module, "Fetch bundle code from different locations (e.g., the
   internet).").

:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(http_get), [http_get/2]).
:- use_module(library(system), [mktemp_in_tmp/2, touch/1]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(source_tree), [remove_dir/1]).

:- use_module(ciaobld(bundle_scan), [scan_bundles_at_path/1]).
:- use_module(ciaobld(messages_aux), [cmd_message/3]).
:- use_module(engine(internals), [
	top_ciao_path/1,
	'$bundle_id'/1]).

:- export(bundle_status/2).
% Status of a bundle (w.r.t. 'ciao get'):
%
%  - fetched: fetched in top workspace
%  - user: in top workspace, but not fetched by 'ciao get'
%  - nontop: available as a system bundle or in another workspace
%  - missing: bundle not available
%
bundle_status(Bundle, Status) :-
	top_ciao_path(RootDir),
	path_concat(RootDir, Bundle, BundleDir),
	( file_exists(BundleDir) ->
	    ( has_fetch_mark(BundleDir) -> Status = fetched
	    ; Status = user
	    )
	; '$bundle_id'(Bundle) -> Status = nontop
	; Status = missing
	).

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
bundle_src_origin(BundleAlias, http_get(URL)) :-
	% Github.com
	atom_concat('github.com/', _, BundleAlias),
	!,
	Ref = master, % TODO: customize Ref
	atom_concat(['https://', BundleAlias, '/archive/', Ref, '.tar.gz'], URL).
bundle_src_origin(BundleAlias, git_archive(URL, Ref)) :-
	% Git repositories at ciao-lang.org
	atom_concat('ciao-lang.org/', _, BundleAlias),
	!,
	Ref = master, % TODO: customize Ref
	atom_concat(['ssh://gitolite@', BundleAlias], URL).

% ---------------------------------------------------------------------------

:- export(bundle_fetch/2).
% bundle_fetch(+Origin, +Bundle): Fetch and build the bundle from Origin
bundle_fetch(Origin, Bundle) :-
	top_ciao_path(RootDir),
	bundle_fetch0(Origin, Bundle, RootDir),
	scan_bundles_at_path(RootDir). % TODO: rescan here or from clients of this module?

% Fetch from Origin the code for Bundle, store at RootDir
bundle_fetch0(Origin, Bundle, RootDir) :-
	bundle_status(Bundle, Status),
	( Status = fetched ->
	    % TODO: allow silent operation, implement 'ciao rm'
	    cmd_message(Bundle, "already exists, skipping fetch (remove to force upgrade)", [])
	; Status = user ->
	    cmd_message(Bundle, "user bundle, skipping fetch", [])
	; Status = nontop ->
	    throw(error_msg("Bundle `~w' exists in a non-top CIAOPATH.", [Bundle]))
	; Status = missing ->
	    path_concat(RootDir, Bundle, BundleDir),
	    bundle_fetch1(Origin, Bundle, BundleDir)
	; fail
	).

bundle_fetch1(Origin, Bundle, BundleDir) :-
	bundle_fetch2(Origin, Bundle, BundleDir),
	!.
bundle_fetch1(Origin, _, _) :-
	throw(error_msg("Bundle fetch failed for `~w'.", [Origin])).

bundle_fetch2(Origin, Bundle, BundleDir) :-
	% Fetch source
	mktemp_in_tmp('ciao-fetch-XXXXXX', File),
	fetch_src(Origin, Bundle, File),
	% Uncompress
	mkpath(BundleDir),
	process_call(path(tar), [
          '-x', '--strip-components', '1',
	  '-C', BundleDir,
	  '-f', File], [status(0)]),
	% Mark as fetched
	set_fetch_mark(BundleDir).

fetch_src(http_get(URL), Bundle, File) :-
	cmd_message(Bundle, "fetching source from ~w", [URL]),
	catch(http_get(URL, file(File)), _E, fail).
fetch_src(git_archive(URL, Ref), Bundle, File) :-
	cmd_message(Bundle, "fetching source from ~w", [URL]),
	process_call(path(git),
	             ['archive', '--format', 'tgz', '--remote', URL,
		      '--prefix', ~atom_concat(Bundle, '/'),
		      '--output', File, Ref],
		     [status(0)]).

% ---------------------------------------------------------------------------
% TODO: 'ciao rm' does not check dependencies, uninstalls, or do warnings

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
	    cmd_message(Bundle, "user bundle, skipping rm", [])
	; Status = nontop ->
	    throw(error_msg("Bundle `~w' exists in a non-top CIAOPATH.", [Bundle]))
	; Status = missing ->
	    throw(error_msg("Bundle `~w' does not exist.", [Bundle]))
	; fail
	).

bundle_rm1(Bundle, BundleDir) :-
	bundle_rm2(Bundle, BundleDir),
	!.
bundle_rm1(Bundle, _) :-
	throw(error_msg("Bundle rm failed for `~w'.", [Bundle])).

bundle_rm2(_Bundle, BundleDir) :-
	remove_dir(BundleDir).

% ---------------------------------------------------------------------------

fetch_mark(Dir, F) :-
 	path_concat(Dir, 'FETCHED_BUNDLE', F).

set_fetch_mark(Dir) :-
 	touch(~fetch_mark(Dir)).

has_fetch_mark(Dir) :-
	file_exists(~fetch_mark(Dir)).



