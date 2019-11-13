:- module(_, [], [assertions, basicmodes, nativeprops, fsyntax, hiord, regtypes]).

:- doc(title,  "Bundle information").
:- doc(author, "Jose F. Morales").
:- doc(author, "The Ciao Development Team").

:- doc(module, "Obtain information about the registered bundles.").
% TODO: This is a higher level interface over bundlereg data that is
%   read in engine(internals). Do not include here information that
%   requires the full Manifest (use manifest_compiler instead).

% ---------------------------------------------------------------------------
% Bundle name and version numbers

:- use_module(engine(internals), ['$bundle_prop'/2]).

:- export(bundle_name/2).
% TODO: use (and fix, not always the identity if we want to
% distinguish between the loaded bundle and the built bundle)
bundle_name(Bundle) := Bundle.

:- export(bundle_version/2).
% Bundle version (major, minor, patch and prerelease)
bundle_version(Bundle) := Version :-
    '$bundle_prop'(Bundle, version(Version)).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(engine(internals), [
    top_ciao_path/1,
    '$bundle_id'/1]).

% TODO: Generalize to include other flags (binary-only bundles, etc.)
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

:- export(bundle_set_status_fetch/1).
% Set 'fetch' status for a newly fetched bundle
bundle_set_status_fetch(Bundle) :-
    top_ciao_path(RootDir),
    path_concat(RootDir, Bundle, BundleDir),
    set_fetch_mark(BundleDir).

:- use_module(library(system), [touch/1]).

fetch_mark(Dir, F) :-
    path_concat(Dir, 'FETCHED_BUNDLE', F).

set_fetch_mark(Dir) :-
    touch(~fetch_mark(Dir)).

has_fetch_mark(Dir) :-
    file_exists(~fetch_mark(Dir)).



