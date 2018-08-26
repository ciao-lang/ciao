:- module(site_aux, [], [assertions, regtypes, dcg, fsyntax, basicmodes]).

:- doc(title, "Auxiliary for preparing build/site directory").

:- use_module(ciaobld(config_common), [site_root_dir/1]).

:- use_module(library(system), [file_exists/1]).
:- use_module(ciaobld(third_party_custom), [third_party_custom_path/2]).

:- use_module(library(persdb/datadir), [ensure_datadir/2]). % TODO: sure?

:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(library(pathnames), [path_is_absolute/1]).
:- use_module(library(pathnames), [path_concat/3, path_split/3]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(glob), [glob/3]).
:- use_module(library(lists), [member/2]).

glob_cp(InDir, Glob, OutDir) :-
	Fs = ~glob(InDir, Glob),
	( member(F, Fs),
	    cp(~path_concat(InDir, F), ~path_concat(OutDir, F)),
	    fail
	; true
	).

:- export(site_glob_cp/4).
% Copy bundle files to a site-relative To path
site_glob_cp(Bundle, From, Glob, To) :-
	AbsFrom = ~bundle_path(Bundle, From),
	AbsTo = ~bundle_site_path(Bundle, To),
	mkpath(AbsTo),
	glob_cp(AbsFrom, Glob, AbsTo).

bundle_site_path(Bundle, To) := AbsTo :-
	SiteDir = ~site_root_dir,
	bundle_site_url(Bundle, To, URL),
	% URL begins with '/' so we must concat strings instead of 
	% using path_concat/3 here
	AbsTo = ~atom_concat(SiteDir, URL). % concat string! (see above)

:- export(bundle_site_url/3).
% Site-relative path for the given URL, e.g.:
%   foo/bar ==> /ciao/bndls/Bundle/foo/bar
%   /foo/bar ==> /foo/bar
bundle_site_url(Bundle, URL0, URL) :-
	( path_is_absolute(URL0) ->
	    URL = URL0
	; % (local to bundle)
	  path_concat('/ciao/bndls', Bundle, BaseURL),
	  path_concat(BaseURL, URL0, URL)
	).

:- export(site_link_bower_components/0).
% Symbolic link from 3rd party bower_components to build/site
site_link_bower_components :-
	Path = ~third_party_custom_path(bower_components),
	site_link_dir_(core, Path, '/bower_components').
 
:- export(site_link_builddoc/0).
% Symbolic link from build/doc to build/site
site_link_builddoc :-
	DocDir = ~bundle_path(core, builddir, 'doc'),
	site_link_dir_(core, DocDir, '/ciao/build/doc').

:- export(site_link_datadir/2).
% Symbolic link from custom datadir (at build/data) to build/site
site_link_datadir(RelPath, To) :-
	AbsFrom = ~ensure_datadir(RelPath),
	site_link_dir_(core, AbsFrom, To).

% TODO: use Bundle (or add workspace)
site_link_dir_(Bundle, AbsFrom, To) :-
	AbsTo = ~bundle_site_path(Bundle, To),
	path_split(AbsTo, AbsToDir, _),
	mkpath(AbsToDir),
	ensure_ln_sf(AbsFrom, AbsTo).

ensure_ln_sf(From, To) :-
	( file_exists(To) ->
	    true
	; ln_sf(From, To)
	).

% ---------------------------------------------------------------------------
% TODO: use system preds

:- use_module(library(process), [process_call/3]).

cp(A, B) :- process_call(path(cp), [A, B], []).
ln_sf(A, B) :- process_call(path(ln), ['-sf', A, B], []).

