:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title, "Loader of bundle meta-rules").
:- doc(author, "Jose F. Morales").

:- doc(module, "Complex bundles require Meta-rules ").

:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).
:- use_module(library(system), [cd/1, working_directory/2, file_exists/1]).
:- use_module(engine(internals), ['$bundle_id'/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_metasrc/3]).

:- use_module(ciaobld(bundlehooks_holder)).

% ---------------------------------------------------------------------------
:- include(ciaobld(bundlehooks/bundlehooks_defs)).
% ---------------------------------------------------------------------------

% ---------------------------------------------------------------------------
% Load bundle metasrc (configuration rules and build hooks)

:- data metasrc_loaded/2.

% TODO: Can this be encoded as a simple use_module of config/hook rules?
%   (we are duplicating functionality from the compiler)

% TODO: add reference counting?

:- export(ensure_load_bundle_metasrc/2).
% Metasrc =
%  bundle_config    % <bndl>.hooks.pl file 
%  bundle_hooks     % <bndl>.config.pl file
%  config_and_hooks % .config.pl files (including dependencies) and <bndl>.hooks.pl
ensure_load_bundle_metasrc(Bundle, config_and_hooks) :- !,
	% Load all config
	% TODO: make it fine-grained: only dependencies
	( % (failure-driven loop)
	  '$bundle_id'(BundleCfg),
	    ensure_load_bundle_metasrc(BundleCfg, bundle_config),
	    fail
	; true
	),
	% Load hooks
	ensure_load_bundle_metasrc(Bundle, bundle_hooks).
ensure_load_bundle_metasrc(Bundle, Metasrc) :-
	( metasrc_loaded(Bundle, Metasrc) ->
	    true
	; assertz_fact(metasrc_loaded(Bundle, Metasrc)),
	  load_bundle_metasrc(Bundle, Metasrc)
	).

% ---------------------------------------------------------------------------

% Load metasrc of Bundle
load_bundle_metasrc(Bundle, Metasrc) :-
	( '$bundle_id'(Bundle), BundleDir = ~bundle_path(Bundle, '.') ->
	    true
	; throw(unknown_bundle(Bundle))
	),
	% TODO: make it optional (some bundles may not need customized hooks)
	bundle_metasrc(Bundle, Metasrc, BundleMetasrcFile),
	( file_exists(BundleMetasrcFile) ->
	    working_directory(PWD, BundleDir), % TODO: Needed here?
	    once_port_reify(bundlehooks_holder:do_use_module(BundleMetasrcFile), Port),
	    cd(PWD),
	    port_call(Port)
	; optional_metasrc(Metasrc) ->
	    true
	; % TODO: write handler?
	  throw(no_bundle_metasrc(Bundle, Metasrc))
	).

% TODO: use
% TODO: use optional_metasrc/1 (do not unload if it was optional)
unload_bundle_metasrc(Bundle, Metasrc) :-
	bundle_metasrc(Bundle, Metasrc, BundleMetasrcFile),
	bundlehooks_holder:do_unload(BundleMetasrcFile).

optional_metasrc(bundle_config).
