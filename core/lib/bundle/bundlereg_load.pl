% (Included from engine(internals), keep it small and simple!)

:- doc(section, "Bundle registry (bundlereg) load").
% Location and read of bundleregs (bundle registry files produced from
% human-readable Manifest.pl files). See @lib{bundle/bundlereg_gen} for
% details about the bundle registry.

% IMPORTANT: Keep this code as small as possible. This code must not
%   depend on library predicates not provided by the engine (as C
%   code).

:- export(bundle_reg_dir/2).
:- pred bundle_reg_dir(LibDir, BundleRegDir) # "Given the Ciao library
   directory @var{LibDir}, obtain the directory @var{BundleRegDir}
   where registered bundles are stored.".

bundle_reg_dir(LibDir, BundleRegDir) :-
	atom_concat(LibDir, '/lib/bundlereg__auto/', BundleRegDir).

:- export(bundlereg_filename/3).
:- pred bundlereg_filename(Bundle, BundleRegDir, File)
   # "@var{File} is the bundlereg file for bundle @var{Bundle}, where
      @var{BundleRegDir} is the directory where bundleregs are
      stored".

bundlereg_filename(Bundle, BundleRegDir, File) :-
	atom_concat(Bundle, '.bundlereg', File0),
	path_concat(BundleRegDir, File0, File).

:- export(bundlereg_version/1).
bundlereg_version(2). % Version of the bundlereg file

% :- use_module(engine(system_info), [ciao_lib_dir/1]).
% :- use_module(engine(internals), [bundle_reg_dir/2]).
% :- use_module(engine(internals), [bundlereg_version/1]).

% TODO: move to system_info (like ciao_lib_dir/1)
:- export(current_bundle_reg_dir/1).
current_bundle_reg_dir(BundleRegDir) :-
	ciao_lib_dir(LibDir),
	bundle_reg_dir(LibDir, BundleRegDir).

% :- use_module(library(system), [file_exists/2, directory_files/2]).
:- import(system, [file_exists/2, directory_files/2]).

:- export(reload_bundleregs/0).
% (Re)Load all bundleregs (registered bundles)
reload_bundleregs :-
	clean_bundlereg_db,
	%
	current_bundle_reg_dir(BundleRegDir),
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
	catch(load_bundlereg_(File), E, true),
	close(Stream),
	set_input(OldIn),
	( nonvar(E) -> throw(E) ; true ).

load_bundlereg_(File) :-
	check_bundlereg_version(File),
	load_bundlereg_loop.

check_bundlereg_version(File) :-
	bundlereg_version(V),
	fast_read(Data),
	( Data = bundlereg_version(V0) ->
	    ( V == V0 ->
	        true
	    ; throw(error(load_bundlereg/1, bad_bundlereg_version(V0)))
	    )
	; throw(error(load_bundlereg/1, not_a_bundlereg(File)))
	).

load_bundlereg_loop :-
	repeat,
	( fast_read(Data) ->
	    process_bundlereg(Data),
	    fail % loop
	; true % finish
	),
	!.

process_bundlereg(bundle_id(Bundle)) :- !,
	assertz_fact('$bundle_id'(Bundle)).
process_bundlereg(bundle_prop(Bundle, Prop)) :- !,
	assertz_fact('$bundle_prop'(Bundle, Prop)).
process_bundlereg(bundle_srcdir(Bundle, Path)) :- !,
	assertz_fact('$bundle_srcdir'(Bundle, Path)).
process_bundlereg(bundle_alias_path(Alias, Bundle, Path)) :- !,
	assertz_fact('$bundle_alias_path'(Alias, Bundle, Path)).
process_bundlereg(X) :-
	throw(error(load_bundlereg/1, unrecognized_data(X))).

% [in basic_props]
% member(X, [X0|Xs]) :- ( X = X0 ; member(X, Xs) ).

