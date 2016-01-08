% (included file)

:- doc(section, "Emacs Mode").

:- use_module(ciaobld(config_common), [
    instype/1,
    bundle_to_bldid/2,
    instciao_bindir/1,
    instciao_bundledir/2
]).

:- use_module(library(llists), [flatten/2]).
:- use_module(library(system), [using_windows/0]).

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

update_dotemacs := ~get_bundle_flag(core:update_dotemacs).

dotemacs := ~get_bundle_flag(core:dotemacs).

emacs_site_start := ~get_bundle_flag(core:emacs_site_start).

emacsmode_dir := bundle_src(ide)/'emacs-mode'.

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

% TODO: Move to a specific installation for emacs-mode
% (used only from 'ciao_builder')
'$builder_hook'(emacs_mode:build_nodocs) :- !, bundleitem_do(emacs_mode, core, build_nodocs).
'$builder_hook'(emacs_mode:build_docs) :- !.
'$builder_hook'(emacs_mode:install) :- !, bundleitem_do(only_global_ins(~emacs_mode_desc), core, install).
'$builder_hook'(emacs_mode:uninstall) :- !, bundleitem_do(only_global_ins(~emacs_mode_desc), core, uninstall).

emacs_mode_desc := [emacs_mode].

% TODO: Each bundle should be able to register parts of the CiaoMode
%       That will solve @bug{lpdoclibdir_emacs_mode}
'$builder_hook'(emacs_mode:item_build_nodocs) :-
	( with_emacs_mode(yes) -> build_emacs_mode
	; true
	).

build_emacs_mode :-
	% TODO: Use better log names (append them?)
	% First, generate 'ciao-config.el' from 'ciao-config.el.skel'
	generate_emacs_config,
	% Generate autoloads automatically with 'batch-update-autoloads'
	Dir = ~emacsmode_dir,
	Init = ~fsR(Dir/'ciao-site-file.el'),
	emacs_update_autoloads(~fsR(Dir), 'emacs_mode3', Init),
	% Compile to elisp bytecode the .el files
        EL = ~ciao_mode_el_files,
	emacs_batch_byte_compile(~fsR(Dir), 'emacs_mode', EL).

get_bindir_elisp(EmacsDir) :- % (for CIAOBINDIR)
	( emacs_type('Win32') ->
	    % TODO: Why?
	    Bundle = core,
	    bundle_to_bldid(Bundle, BldId),
	    Dir = ~fsR(builddir_bin(BldId))
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

:- use_module(ciaobld(builder_cmds),
	[ensure_load_bundlehooks/1, bundle_manual_base/2]).

% Enumerate all manuals of Bundle
get_bundle_manual_base_elisp(Bundle, NameVersion):-
	ensure_load_bundlehooks(Bundle),
	bundle_manual_base(Bundle, NameVersion).

% ---------------------------------------------------------------------------
% Generate ciao-config.el (from ciao-config.el.skel)

% TODO: remove or reduced as much as possible (emacs-mode should talk
% with Ciao to query this information)

:- use_module(library(text_template), [eval_template_file/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(bundle/doc_flags), [docformatdir/2]).

generate_emacs_config :-
	In = ~fsR(~emacsmode_dir/'ciao-config.el.skel'),
	Out = ~fsR(~emacsmode_dir/'ciao-config.el'),
	% manual bases (name and version)
	root_bundle(RootBundle),
	Bases = ~findall(B, ((Bundle = RootBundle ; enum_sub_bundles(RootBundle, Bundle)),
	                     get_bundle_manual_base_elisp(Bundle, B))),
	elisp_string_list(Bases, BasesStr, []),
	%
	( instype(local) ->
	    BundleDirCore = ~fsR(bundle_src(core)),
	    BundleDirLPDoc = ~fsR(bundle_src(lpdoc))
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

:- use_module(library(system_extra), [(-)/1]).
:- use_module(ciaobld(register_in_script), [
 	register_in_script/3, unregister_from_script/2]).

% (called from 'ciao_builder' (for testing))
'$builder_hook'(emacs_mode:register) :- bundleitem_do(emacs_mode, core, register).
% (called from 'ciao_builder' (for testing))
'$builder_hook'(emacs_mode:unregister) :- bundleitem_do(emacs_mode, core, unregister).

'$builder_hook'(emacs_mode:item_register) :-
	( with_emacs_mode(yes) ->
	    ( emacs_init_file(InitFile) ->
	        (-register_in_script(InitFile, ";", ~emacs_config))
	    ; % Do not register
	      true
	    )
	; true
	).

'$builder_hook'(emacs_mode:item_unregister) :-
	( with_emacs_mode(yes) ->
	    ( emacs_init_file(InitFile) ->
	        (-unregister_from_script(InitFile, ";"))
	    ; true
	    )
	; true
	).

emacs_config(S) :-
	Lib = ~ciaolibemacs,
	Lib2 = ~emacs_style_path(Lib),
	emacs_config_(Lib2, S, []).

emacs_config_(Lib) -->
	"(if (file-exists-p \"", emit_atom(Lib), "\")\n"||
	"(load-file \"", emit_atom(Lib), "\")\n"||
	")\n".

% The absolute path for the 'ciao-site-file.el' file
ciaolibemacs(LibEmacs) :-
	( instype(local) ->
	    LibEmacs = ~fsR(~emacsmode_dir/'ciao-site-file.el')
	; % TODO: Place the version in the right place automatically?
	  % TODO: Verify that the rest of .el files are in the correct directory.
	  LibEmacs = ~fsR(~instciao_bundledir(core)/'ciao-site-file.el')
	).

% ---------------------------------------------------------------------------

:- use_module(library(system), [touch/1]).

% Generation of LPdoc documentation source for emacs-mode based on
% ciao-documentation.el.
'$builder_hook'(emacs_mode:prebuild_docs) :-
	EmacsModeDir = ~emacsmode_dir,
	emacs_batch_call(~fsR(EmacsModeDir), 'emacs_mode2', % TODO: right log name?
	  ['--eval', '(setq load-path (cons "." load-path))',
	   '-l', 'ciao-documentation.el',
	   '-f', 'ciao-mode-documentation']),
	%
	touch(~fsR(EmacsModeDir/'CiaoMode.pl')).

%-----------------------------------------------------------------------------

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(system_extra),
	[del_file_nofail/1,
	 del_files_nofail/1]).

% (only for instype=global)
'$builder_hook'(emacs_mode:item_install) :-
	( with_emacs_mode(yes) ->
	    % (only for global installation)
	    Dir = ~emacsmode_dir,
	    Loader = ~fsR(Dir/'ciao-mode-init.el'),
	    string_to_file(~emacs_config, Loader),
	    bundleitem_do(~emacs_mode_desc2, core, install),
	    del_file_nofail(Loader) % (not needed after install)
	; true
	).

% (only for instype=global)
'$builder_hook'(emacs_mode:item_uninstall) :-
	( with_emacs_mode(yes) ->
	    bundleitem_do(~emacs_mode_desc2, core, uninstall)
	; true
	).

emacs_mode_desc2 := [
	  dir(~icon_dir, [files_from(~emacsmode_dir/'icons'), del_rec]),
	  ~ciao_mode_lisp_desc,
	  ~ciao_mode_init_desc
	].

icon_dir := ~fsR(~instciao_bundledir(core)/'icons').

% TODO: Remember to 'update builder/src/win32/Ciao.iss.skel'
%       if the files here are modified. Ideally, that file should not
%       contain any hardwired list of files.

% TODO: Merge this part with register. Make emacsinitfile a particular
% case.
% (needs with_emacs_mode(yes))
ciao_mode_init_desc := Desc :-
        MidDir = ~emacs_site_start,
        is_site_start_d(MidDir),
	!,
        Mid = ~fsR(MidDir/(~emacsinitfile)),
	Desc = [
          dir(MidDir, [do_not_del]),
          lib_file_list(core, ~emacsmode_dir, [
            'ciao-mode-init.el'-[to_abspath(Mid)] % TODO: why?
          ])
        ].
ciao_mode_init_desc := [].

emacsinitfile := ~get_bundle_flag(core:emacsinitfile).

:- use_module(ciaobld(builder_aux), [rootprefix/1]).

% Obtain the appropriate configuration file for this system or
% installation (.emacs or site-start.el). This predicate fails if no
% change is required, because the mode is installed through the
% site-start.d/ directory (Debian only?), or because the Ciao Emacs
% Mode is disabled.
emacs_init_file := InitFile :-
        ( % Local installation, register in your .emacs file
	  instype(local), update_dotemacs(yes) ->
	    InitFile = ~dotemacs
	; % Register in the site-start.el file:
	  %  - if the site-start.d directory was not found
	  %  - and, we are not using rootprefix (--destdir=DIR in
	  %    install)
	  \+ (rootprefix(Prefix), \+ Prefix = ''),
	  Dir = ~emacs_site_start,
	  \+ is_site_start_d(Dir) ->
	    InitFile = ~fsR(Dir/'site-start.el')
	; % No init file has to be modified
	  % (see ciao_mode_init_desc/1 for site-start.d installation)
	  fail
	).

% Check that Dir is a site-start.d directory
is_site_start_d(Dir) :-
        atom_concat(_, '/site-start.d', Dir).

:- use_module(library(llists), [append/2]).

ciao_mode_lisp_desc :=
	lib_file_list(core, ~emacsmode_dir, 
          ~append([
            ~addprops(['ciao-site-file.el'], [copy_and_link]),
            ~addprops(~ciao_mode_el_files, [copy_and_link]),
            ~addprops(~ciao_mode_elc_files, [copy_and_link])
          ])
        ).

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

'$builder_hook'(emacs_mode:clean_norec) :- clean_emacs_mode.
clean_emacs_mode :-
	EmacsModeDir = ~emacsmode_dir,
	clean_tree(~fsR(EmacsModeDir)),
	% TODO: necessary? repeated?
	del_file_nofail(~fsR(EmacsModeDir/'ciao-site-file.el')),
	% clean log files
	emacs_clean_log(~fsR(EmacsModeDir), 'emacs_mode'),
	emacs_clean_log(~fsR(EmacsModeDir), 'emacs_mode2'),
	emacs_clean_log(~fsR(EmacsModeDir), 'emacs_mode3'),
%	del_file_nofail(~fsR(EmacsModeDir/'*~' pattern?)).
	%
	% (automatically generated files)
	del_file_nofail(~fsR(EmacsModeDir/'CiaoMode.lpdoc')),
	del_file_nofail(~fsR(EmacsModeDir/'ciao-config.el')),
	% TODO: Use common implementation, do not specify suffixes by hand
	del_files_nofail(~add_prefix(~glob(~fsR(EmacsModeDir), '*.itf|*.po|*.asr|*.testout|*.elc'),
	                             ~atom_concat(~fsR(EmacsModeDir), '/'))).

add_prefix([],     _Preffix, []).
add_prefix([L|Ls], Preffix,  [R|Rs]) :-
	atom_concat(Preffix, L, R),
	add_prefix(Ls, Preffix, Rs).

% ---------------------------------------------------------------------------
% (Auxiliary predicates)

% Add the property list Prop to each of Xs (for bundle description)
addprops([], _Props) := [].
addprops([X|Xs], Props) := [X-Props | ~addprops(Xs, Props)].

