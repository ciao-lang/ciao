% (Included from engine(internals), keep it small and simple!)

:- doc(section, "Bundle registry (bundlereg) load").
% Location and read of bundleregs (bundle registry files produced from
% human-readable Manifest.pl files). See @lib{bundle/bundlereg_gen} for
% details about the bundle registry.

% IMPORTANT: Keep this code as small as possible. This code must not
%   depend on library predicates not provided by the engine (as C
%   code).

:- export(bundlereg_filename/3).
:- pred bundlereg_filename(Bundle, BundleRegDir, File)
   # "@var{File} is the bundlereg file for bundle @var{Bundle}, where
      @var{BundleRegDir} is the directory where bundleregs are
      stored".

bundlereg_filename(Bundle, BundleRegDir, File) :-
	atom_concat(Bundle, '.bundlereg', File0),
	path_concat(BundleRegDir, File0, File).

:- export(bundlereg_version/1).
bundlereg_version(3). % Version of the bundlereg file

% :- use_module(engine(system_info), [ciao_lib_dir/1]).
% :- use_module(engine(internals), [bundlereg_version/1]).

% (see bundle_scan:rootprefix_bundle_reg_file/3 for InsType=global)
:- export(bundle_reg_dir/2).
bundle_reg_dir(InsType, BundleRegDir) :- InsType = local,
	% (heuristic to detect running from a global installation)
	ciao_lib_dir(LibDir),
	path_concat(LibDir, 'bundlereg', BundleRegDir0),
	file_exists(BundleRegDir0, 0),
	!,
	BundleRegDir = BundleRegDir0.
bundle_reg_dir(InsType, BundleRegDir) :- InsType = local, !,
	% (heuristic to detect builddir in local installation)
	ciao_lib_dir(LibDir),
	path_split(LibDir, CiaoRoot, _),
	path_concat(CiaoRoot, 'build', BuildDir),
	path_concat(BuildDir, 'bundlereg', BundleRegDir).
bundle_reg_dir(InsType, BundleRegDir) :- InsType = inpath(Path), !,
	path_concat(Path, 'build', BuildDir),
	path_concat(BuildDir, 'bundlereg', BundleRegDir).

:- import(system, [extract_paths/2]).
:- import(system, [c_get_env/2]).

:- export(ciao_path/1).
:- data ciao_path/1.

:- export(get_ciaopath/0).
% Update ciao_path/1 with paths in CIAOPATH (or add default if none)
get_ciaopath :-
	get_ciaopath_(CiaoPaths),
	% assert in ciao_path/1
	retractall_fact(ciao_path(_)),
	( member(X, CiaoPaths),
	    assertz_fact(ciao_path(X)),
	    fail
	; true
	).

get_ciaopath_(CiaoPaths) :-
	( c_get_env('CIAOPATH', CiaoPathList) ->
	    extract_paths(CiaoPathList, CiaoPaths0)
	; CiaoPaths0 = []
	),
	( CiaoPaths0 = [] ->
	    % No CIAOPATH or no paths, use default
	    '$expand_file_name'('~/.ciao', true, DefCiaoPath),
	    CiaoPaths = [DefCiaoPath]
	; CiaoPaths = CiaoPaths0
	).

:- export(top_ciao_path/1).
% First path in ciao_path/1 (target directory for 'ciao get')
top_ciao_path(Dir) :-
	ciao_path(Dir0),
	!,
	Dir = Dir0.

% :- use_module(library(system), [file_exists/2, directory_files/2]).
:- import(system, [file_exists/2, directory_files/2]).

:- export(reload_bundleregs/0).
% (Re)Load all bundleregs (registered bundles)
reload_bundleregs :-
	clean_bundlereg_db,
	% Load bundle regs in ciao_path/1 and local (or installed) path
	( % (failure-driven loop)
	  ( ciao_path(Dir),
	    bundle_reg_dir(inpath(Dir), BundleRegDir)
	  ; bundle_reg_dir(local, BundleRegDir)
	  ),
	    reload_bundleregs_(BundleRegDir),
	    fail
	; true
	).

reload_bundleregs_(BundleRegDir) :-
	( file_exists(BundleRegDir, 0) ->
	    directory_files(BundleRegDir, Files),
	    ( % (failure-driven loop)
              member(File, Files),
	      atom_concat(_, '.bundlereg', File),
	      path_concat(BundleRegDir, File, BundleRegFile),
	      load_bundlereg(BundleRegFile),
	      fail
	    ; true
	    )
	; true
	).

% :- use_module(library(fastrw), [fast_read/1]).
:- import(fastrw, [fast_read/1]).
% :- use_module(engine(internals), ['$open'/3]).

:- export(load_bundlereg/1). % (exported for explicit loads)
load_bundlereg(File) :-
	'$open'(File, r, Stream),
        current_input(OldIn),
        set_input(Stream),
	( catch(load_bundlereg_(File), E, true) -> true
	; E = error(load_bundlereg/1, unexpected_failure(File))
	),
	close(Stream),
	set_input(OldIn),
	( nonvar(E) -> throw(E) ; true ).

load_bundlereg_(File) :-
	% Get version
	fast_read(Data),
	( Data = bundlereg_version(Version) -> true
	; throw(error(load_bundlereg/1, not_a_bundlereg(File)))
	),
	% Load items
	load_bundlereg_v(Version, File).

load_bundlereg_v(3, _File) :- !,
	fast_read(Data),
	Data = bundle_id(Bundle),
	( '$bundle_id'(Bundle) -> 
	    % silently ignore if already loaded
	    true
	; assertz_fact('$bundle_id'(Bundle)),
	  load_bundlereg_loop
	).
% load_bundlereg_v(2, _File) :- !,
% 	load_bundlereg_loop_v2.
load_bundlereg_v(Version, File) :- !,
	throw(error(load_bundlereg/1, bad_bundlereg_version(File, Version))).

load_bundlereg_loop :-
	repeat,
	( fast_read(Data) ->
	    process_bundlereg(Data),
	    fail % loop
	; true % finish
	),
	!.

process_bundlereg(bundle_prop(Bundle, Prop)) :- !,
	assertz_fact('$bundle_prop'(Bundle, Prop)).
process_bundlereg(bundle_srcdir(Bundle, Path)) :- !,
	assertz_fact('$bundle_srcdir'(Bundle, Path)).
process_bundlereg(bundle_alias_path(Alias, Bundle, Path)) :- !,
	assertz_fact('$bundle_alias_path'(Alias, Bundle, Path)).
process_bundlereg(X) :-
	throw(error(load_bundlereg/1, unrecognized_data(X))).

% load_bundlereg_loop_v2 :-
% 	repeat,
% 	( fast_read(Data) ->
% 	    process_bundlereg_v2(Data),
% 	    fail % loop
% 	; true % finish
% 	),
% 	!.
% 
% process_bundlereg_v2(bundle_id(Bundle)) :- !,
% 	assertz_fact('$bundle_id'(Bundle)).
% process_bundlereg_v2(bundle_prop(Bundle, Prop)) :- !,
% 	assertz_fact('$bundle_prop'(Bundle, Prop)).
% process_bundlereg_v2(bundle_srcdir(Bundle, Path)) :- !,
% 	assertz_fact('$bundle_srcdir'(Bundle, Path)).
% process_bundlereg_v2(bundle_alias_path(Alias, Bundle, Path)) :- !,
% 	assertz_fact('$bundle_alias_path'(Alias, Bundle, Path)).
% process_bundlereg_v2(X) :-
% 	throw(error(load_bundlereg/1, unrecognized_data(X))).

% [in basic_props]
% member(X, [X0|Xs]) :- ( X = X0 ; member(X, Xs) ).

