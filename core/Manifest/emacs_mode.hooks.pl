% (included file)

:- doc(section, "Emacs Mode").

:- use_module(ciaobld(config_common), [
    instype/1,
    instciao_bindir/1,
    instciao_bundledir/2
]).

:- use_module(library(llists), [flatten/2, append/2]).
:- use_module(library(system), [using_windows/0]).
:- use_module(library(system), [touch/1]).
:- use_module(library(system_extra), [move_if_diff/2]).
:- use_module(library(system_extra),
	[del_file_nofail/1,
	 del_files_nofail/1]).

:- use_module(library(bundle/bundle_info),
	[root_bundle/1,
	 enum_sub_bundles/2,
	 bundle_version/2,
	 bundle_version_patch/2]).

:- use_module(library(emacs/emacs_batch), [
	emacs_style_path/2,
	emacs_type/1,
        emacs_batch_call/3,
	emacs_batch_byte_compile/3,
	emacs_update_autoloads/3,
        emacs_clean_log/2]).

% (in order to share code, make the emacs kind a parameter)

% TODO: use --set-flag to avoid this, fix environment_and_windows_bats
get_bundle_param_or_flag(Flag, Value) :-
	bundle_param_value(Flag, Value),
	!.
get_bundle_param_or_flag(Flag, Value) :-
	get_bundle_flag(Flag, Value).

with_emacs_mode := ~get_bundle_param_or_flag(core:with_emacs_mode).

emacsmode_dir := ~bundle_path(ide, 'emacs-mode').

% Here is how this all works: 
% 
% - During build and install of 'ide/emacs_mode'
%
%   * The ciao-config.el.skel file is filled with configuration
%     parameters for the installed system to produce ciao-config.el,
%     which is installed in the libraries. The current Ciao version is
%     also included in ciao-config.el at this time.
%
%   * All .el files in the libraries are byte-compiled.
%    
% - Generating the documentation ('ciao build_docs'):
%
%   * CiaoMode.lpdoc is generated from ciao-documentation.el using
%     emacs (contains and extracts the documentation for all the elisp
%     functions). CiaoMode.pl is included as a chapter in the Ciao
%     manual.

% TODO: define third-party plugin for emacs code (so that we can add extensions to the emacs mode easily?)
% TODO: define one .el per bundle or collect all, during dot_emacs build/installation
%       That may solve @bug{lpdoclibdir_emacs_mode}

% (for installation)
'$builder_hook'(emacs_mode:item_def(Desc)) :-
	( with_emacs_mode(yes) ->
	    Desc = [
              switch_to_bundle(ide, files_from('emacs-mode/icons', ~icon_dir, [del_rec])),
	      switch_to_bundle(ide, lib_file_list('emacs-mode', ~emacs_mode_files))]
	; Desc = []
	).
%
'$builder_hook'(emacs_mode:prebuild_docs) :-
	( with_emacs_mode(yes) -> prebuild_docs_emacs_mode
	; true
	).
'$builder_hook'(emacs_mode:build_nodocs) :-
	( with_emacs_mode(yes) -> build_emacs_mode
	; true
	).
'$builder_hook'(emacs_mode:build_docs) :- !.
'$builder_hook'(emacs_mode:clean_norec) :- clean_emacs_mode.

build_emacs_mode :-
	% TODO: Use better log names (append them?)
	% First, generate 'ciao-config.el' from 'ciao-config.el.skel'
	generate_emacs_config,
	% Generate autoloads automatically with 'batch-update-autoloads'
	Dir = ~emacsmode_dir,
	Init = ~path_concat(Dir, 'ciao-site-file.el'),
	emacs_update_autoloads(Dir, 'emacs_mode3', Init),
	% Compile to elisp bytecode the .el files
        EL = ~ciao_mode_el_files,
	emacs_batch_byte_compile(Dir, 'emacs_mode', EL).

get_bindir_elisp(EmacsDir) :- % (for CIAOBINDIR)
	( emacs_type('Win32') ->
	    % TODO: Why?
	    Dir = ~bundle_path(~root_bundle, builddir, 'bin')
	; Dir = ~instciao_bindir
	),
	get_dir_elisp(Dir, EmacsDir).

% TODO: use ciao-root-dir for all emacs_type?
get_dir_elisp(Dir, EmacsDir) :-
	emacs_type('MacOSBundle'),
	!,
	flatten(["(concat ciao-root-dir \"", ~atom_codes(Dir), "\")"], EmacsDir).
get_dir_elisp(Dir, EmacsDir) :-
	% TODO: missing escape of Dir2
	Dir2 = ~emacs_style_path(Dir),
	flatten(["\"", ~atom_codes(Dir2), "\""], EmacsDir).

% Path to a bundle command binary, as elisp expression
% (see cmdname_ver/5)
cmdpath_elisp(Bundle, Cmd, Kind, Expr) :-
	( Kind = plexe -> K = plexe
	; Kind = script -> K = ext(~script_extension)
	),
	CmdName = ~cmdname_ver(yes, Bundle, Cmd, K),
	Expr = ~flatten(["(concat ciao-bin-dir \"/", ~atom_codes(CmdName), "\")"]).

% TODO: Why .bat? (only for 'ciaocl' at this moment)
script_extension('.bat') :- ( using_windows ; emacs_type('Win32') ), !.
script_extension('').

:- use_module(ciaobld(builder_meta), [ensure_load_bundle_metasrc/2]).
:- use_module(ciaobld(builder_cmds), [bundle_manual_base/2]).

% Enumerate all manuals of Bundle
get_bundle_manual_base_elisp(Bundle, NameVersion):-
	ensure_load_bundle_metasrc(Bundle, bundle_hooks),
	bundle_manual_base(Bundle, NameVersion).

% ---------------------------------------------------------------------------
% Generate ciao-config.el (from ciao-config.el.skel)

% TODO: remove or reduced as much as possible (emacs-mode should talk
% with Ciao to query this information)

:- use_module(library(text_template), [eval_template_file/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(bundle/doc_flags), [docformatdir/2]).

generate_emacs_config :-
	In = ~path_concat(~emacsmode_dir, 'ciao-config.el.skel'),
	Out = ~path_concat(~emacsmode_dir, 'ciao-config.el'),
	% manual bases (name and version)
	root_bundle(RootBundle),
	Bases = ~findall(B, ((Bundle = RootBundle ; enum_sub_bundles(RootBundle, Bundle)),
	                     get_bundle_manual_base_elisp(Bundle, B))),
	elisp_string_list(Bases, BasesStr, []),
	%
	( instype(local) ->
	    BundleDirCore = ~bundle_path(core, '.'),
	    BundleDirLPDoc = ~bundle_path(lpdoc, '.')
	; BundleDirCore = ~instciao_bundledir(core),
	  BundleDirLPDoc = ~instciao_bundledir(lpdoc)
	),
	%
	eval_template_file(In, [
	    % Emacs type (for ciao mode)
            'CIAO_EMACS_TYPE' = ~emacs_type,
	    %
            'CIAO_VERSION' = ~bundle_version_patch(ciao),
	    % Paths
	    'CIAOBINDIR' = ~get_bindir_elisp,
	    'BUNDLEDIR_CORE' = ~get_dir_elisp(BundleDirCore),
	    'LPDOCDIR' = ~get_dir_elisp(~docformatdir(any)),
	    'LPDOCLIBDIR' = ~get_dir_elisp(BundleDirLPDoc),
	    % Manual bases (for ciao-help.el)
            'MANUAL_BASES' = BasesStr,
	    % Command binaries
	    'PLINDENT' = ~cmdpath_elisp(core, 'plindent', plexe),
	    'CIAOSHELL' = ~cmdpath_elisp(core, 'ciao', script),
	    'CIAOPPSHELL' = ~cmdpath_elisp(ciaopp, 'ciaopp', plexe),
	    'LPDOCEXEC' = ~cmdpath_elisp(lpdoc, 'lpdoc', plexe)
        ], Out).

elisp_string_list(Xs) --> "(", elisp_string_list_(Xs), ")".

elisp_string_list_([X]) --> !,
	% TODO: missing escape of X
	"\"", emit_atom(X), "\"".
elisp_string_list_([X|Xs]) -->
	% TODO: missing escape of X
	"\"", emit_atom(X), "\"", " ", elisp_string_list_(Xs).
elisp_string_list_([]) --> [].

% ---------------------------------------------------------------------------

% Generation of LPdoc documentation source for emacs-mode based on
% `ciao-documentation.el`. Update `CiaoMode.pl` timestamp if
% `CiaoMode.lpdoc` has changed.
prebuild_docs_emacs_mode :-
	EmacsModeDir = ~emacsmode_dir,
	emacs_batch_call(EmacsModeDir, 'emacs_mode2', % TODO: right log name?
	  ['--eval', '(setq load-path (cons "." load-path))',
	   '-l', 'ciao-documentation.el',
	   '-f', 'ciao-mode-documentation']),
	%
	( move_if_diff(~path_concat(EmacsModeDir, 'CiaoMode.new.lpdoc'),
	               ~path_concat(EmacsModeDir, 'CiaoMode.lpdoc')) ->
	    touch(~path_concat(EmacsModeDir, 'CiaoMode.pl'))
	; true
	).

%-----------------------------------------------------------------------------

emacs_mode_files :=
    ~append([
      ~addprops(['ciao-site-file.el'], [copy_and_link]),
      ~addprops(~ciao_mode_el_files, [copy_and_link]),
      ~addprops(~ciao_mode_elc_files, [copy_and_link])
    ]).

icon_dir := ~path_concat(~instciao_bundledir(core), 'icons').

% TODO: Remember to 'update builder/src/win32/Ciao.iss.skel'
%       if the files here are modified. Ideally, that file should not
%       contain any hardwired list of files.

ciao_mode_lisp_files := [
	'word-help',
	'ciao-help',
	'ciao-faces',
	'ciao-syntax',
	'ciao-parsing',
	'ciao-aux',
	'ciao-font-lock',
	'ciao-vc',
	'ciao-scratchpad',
	'ciao-process',
	'ciao-compile',
	'ciao-loading',
	'ciao-testing',
	'ciao-debugger',
	'ciao-lpdoc',
	'ciao-ciaopp',
	'java-ciaopp',
	'ciao-builder',
	'ciao-optim-comp',
	'ciao-org',
	'ciao-widgets',
	'ciao-common',
	'ciao-config',
	'ciao-splash',
	'ciao-bindings',
	'ciao'].

ciao_mode_el_files := ~add_suffix(~ciao_mode_lisp_files, '.el').
ciao_mode_elc_files := ~add_suffix(~ciao_mode_lisp_files, '.elc').

add_suffix([],     _Suffix, []).
add_suffix([L|Ls], Suffix,  [R|Rs]) :-
	atom_concat(L, Suffix, R),
	add_suffix(Ls, Suffix, Rs).

% ----------------------------------------------------------------------------

:- use_module(library(glob), [glob/3]).
:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).

clean_emacs_mode :-
	EmacsModeDir = ~emacsmode_dir,
	clean_tree(EmacsModeDir),
	% TODO: necessary? repeated?
	del_file_nofail(~path_concat(EmacsModeDir, 'ciao-site-file.el')),
	% clean log files
	emacs_clean_log(EmacsModeDir, 'emacs_mode'),
	emacs_clean_log(EmacsModeDir, 'emacs_mode2'),
	emacs_clean_log(EmacsModeDir, 'emacs_mode3'),
	%
	% (automatically generated files)
	del_file_nofail(~path_concat(EmacsModeDir, 'CiaoMode.lpdoc')),
	del_file_nofail(~path_concat(EmacsModeDir, 'ciao-config.el')),
	% TODO: Use common implementation, do not specify suffixes by hand
	del_files_nofail(~add_prefix(~glob(EmacsModeDir, '*.itf|*.po|*.asr|*.testout|*.elc'),
	                             ~atom_concat(EmacsModeDir, '/'))).

add_prefix([],     _Preffix, []).
add_prefix([L|Ls], Preffix,  [R|Rs]) :-
	atom_concat(Preffix, L, R),
	add_prefix(Ls, Preffix, Rs).

% ---------------------------------------------------------------------------
% (Auxiliary predicates)

% Add the property list Prop to each of Xs (for bundle description)
addprops([], _Props) := [].
addprops([X|Xs], Props) := [X-Props | ~addprops(Xs, Props)].

