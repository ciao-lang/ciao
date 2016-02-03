:- module(_, [], [dcg, fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Bundle Management").
:- doc(author, "Jose F. Morales").

:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(format), [format/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(http_get), [http_get/2]).
:- use_module(library(system), [mktemp_in_tmp/2, touch/1]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(source_tree), [remove_dir/1]).

:- use_module(ciaobld(bundle_scan), [bundle_scan/2]).
:- use_module(ciaobld(ciaoc_aux), [clean_bundlereg/1]).
:- use_module(ciaobld(messages_aux), [cmd_message/3]).
:- use_module(engine(internals), [
	reload_bundleregs/0,
	top_ciao_path/1,
	'$bundle_id'/1]).

% Status of a bundle (w.r.t. 'ciao get'):
%
%  - fetched: fetched in top workspace
%  - user: in top workspace, but not fetched by 'ciao get'
%  - nontop: available as a system bundle or in another workspace
%  - missing: bundle not available
%
bundle_status(BundleAlias, Bundle, Status) :-
	top_ciao_path(RootDir),
	path_split(BundleAlias, _, Bundle),
	path_concat(RootDir, Bundle, BundleDir),
	( file_exists(BundleDir) ->
	    ( has_fetch_mark(BundleDir) -> Status = fetched
	    ; Status = user
	    )
	; '$bundle_id'(Bundle) -> Status = nontop
	; Status = missing
	).

% ---------------------------------------------------------------------------

:- export(bundle_fetch/2).
% bundle_fetch(+BundleAlias, -Bundle): Fetch and build the bundle
%   specified in BundleAlias
bundle_fetch(BundleAlias, Bundle) :-
	% Fetch and rescan
	top_ciao_path(RootDir),
	bundle_fetch0(BundleAlias, RootDir, Bundle),
	update_bundleregs(RootDir).

% Fetch at RootDir source code of bundle specified in BundleAlias
bundle_fetch0(BundleAlias, RootDir, Bundle) :-
	bundle_status(BundleAlias, Bundle, Status),
	% Compute bundle dir and check status
	( Status = fetched ->
	    % TODO: allow silent operation, implement 'ciao rm'
	    cmd_message(Bundle, "already exists, skipping fetch (remove to force upgrade)", [])
	; Status = user ->
	    cmd_message(Bundle, "user bundle, skipping fetch", [])
	; Status = nontop ->
	    format(user_error, "ERROR: Bundle `~w' exists in a non-top CIAOPATH.~n", [Bundle]),
	    halt(1)
	; Status = missing ->
	    path_concat(RootDir, Bundle, BundleDir),
	    bundle_fetch1(BundleAlias, Bundle, BundleDir)
	; fail
	).

bundle_fetch1(BundleAlias, Bundle, BundleDir) :-
	bundle_fetch2(BundleAlias, Bundle, BundleDir),
	!.
bundle_fetch1(BundleAlias, _, _) :-
	format(user_error, "ERROR: Bundle fetch failed for `~w'.~n", [BundleAlias]),
	halt(1).

bundle_fetch2(BundleAlias, Bundle, BundleDir) :-
	% Fetch source
	bundle_src_origin(BundleAlias, Origin),
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
bundle_src_origin(BundleAlias, _Origin) :-
	format(user_error, "ERROR: Unrecognized bundle alias path `~w'.~n", [BundleAlias]),
	halt(1).

% ---------------------------------------------------------------------------
% TODO: 'ciao rm' does not check dependencies, uninstalls, or do warnings

:- export(bundle_rm/1).
% Remove the downloaded bundle specified in BundleAlias
bundle_rm(BundleAlias) :-
	top_ciao_path(RootDir),
	bundle_rm0(BundleAlias, RootDir, _Bundle),
	update_bundleregs(RootDir).

bundle_rm0(BundleAlias, RootDir, Bundle) :-
	bundle_status(BundleAlias, Bundle, Status),
	% Compute bundle dir and check status
	( Status = fetched ->
	    path_concat(RootDir, Bundle, BundleDir),
	    bundle_rm1(BundleAlias, Bundle, BundleDir)
	; Status = user ->
	    cmd_message(Bundle, "user bundle, skipping rm", [])
	; Status = nontop ->
	    format(user_error, "ERROR: Bundle `~w' exists in a non-top CIAOPATH.~n", [Bundle]),
	    halt(1)
	; Status = missing ->
	    format(user_error, "ERROR: Bundle `~w' does not exist.~n", [Bundle]),
	    halt(1)
	; fail
	).

bundle_rm1(BundleAlias, Bundle, BundleDir) :-
	bundle_rm2(BundleAlias, Bundle, BundleDir),
	!.
bundle_rm1(BundleAlias, _, _) :-
	format(user_error, "ERROR: Bundle rm failed for `~w'.~n", [BundleAlias]),
	halt(1).

bundle_rm2(_BundleAlias, _Bundle, BundleDir) :-
	remove_dir(BundleDir).

% ---------------------------------------------------------------------------

fetch_mark(Dir, F) :-
 	path_concat(Dir, 'FETCHED_BUNDLE', F).

set_fetch_mark(Dir) :-
 	touch(~fetch_mark(Dir)).

has_fetch_mark(Dir) :-
	file_exists(~fetch_mark(Dir)).

% ---------------------------------------------------------------------------

update_bundleregs(RootDir) :-
	clean_bundlereg(inpath(RootDir)),
	bundle_scan(inpath(RootDir), RootDir),
	reload_bundleregs. % (reload, bundles has been scanned)
