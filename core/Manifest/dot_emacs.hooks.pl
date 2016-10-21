% (included file)

:- doc(section, "Emacs Environment Configuration").

:- use_module(ciaobld(config_common), [
    instype/1,
    instciao_bundledir/2
]).

:- use_module(library(file_utils), [string_to_file/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [warn_on_nosuccess/1]).
:- use_module(ciaobld(builder_aux),
	[rootprefix/1, storedir_install/1, storedir_uninstall/1]).
:- use_module(ciaobld(register_in_script), [
 	register_in_script/3, unregister_from_script/2]).

emacsinitfile := ~get_bundle_flag(core:emacsinitfile).
update_dotemacs := ~get_bundle_flag(core:update_dotemacs).
dotemacs := ~get_bundle_flag(core:dotemacs).
emacs_site_start := ~get_bundle_flag(core:emacs_site_start).

'$builder_hook'(dot_emacs:build_nodocs) :- !.
'$builder_hook'(dot_emacs:build_docs) :- !.
'$builder_hook'(dot_emacs:register) :-
	( with_emacs_mode(yes) ->
	    ( emacs_init_file(InitFile) ->
	        % Register in some .emacs
	        warn_on_nosuccess(register_in_script(InitFile, ";", ~emacs_load_script))
	    ; has_site_start_d(SiteStartD) ->
	        % Register at emacs site start dir
	        mktemp_in_tmp('emacsloaderXXXXXX', Loader),
		string_to_file(~emacs_load_script, Loader),
		InitFile = ~path_concat(SiteStartD, ~emacsinitfile),
		storedir_install(dir(SiteStartD)),
		storedir_install(file_noexec(Loader, InitFile)),
		del_file_nofail(Loader)
	    ; % Do not register
	      true
	    )
	; true
	).
'$builder_hook'(dot_emacs:unregister) :-
	( with_emacs_mode(yes) ->
	    ( emacs_init_file(InitFile) ->
	        warn_on_nosuccess(unregister_from_script(InitFile, ";"))
	    ; has_site_start_d(SiteStartD) ->
	        InitFile = ~path_concat(SiteStartD, ~emacsinitfile),
		storedir_uninstall(file(InitFile))
            ; % Do not unregister
	      true
	    )
	; true
	).

has_site_start_d(SiteStartD) :-
	SiteStartD = ~emacs_site_start,
	is_site_start_d(SiteStartD).

emacs_load_script(S) :-
	Lib = ~ciaolibemacs,
	Lib2 = ~emacs_style_path(Lib),
	emacs_load_script_(Lib2, S, []).

emacs_load_script_(Lib) -->
	"(if (file-exists-p \"", emit_atom(Lib), "\")\n"||
	"  (load-file \"", emit_atom(Lib), "\"))\n".

% The absolute path for the 'ciao-site-file.el' file
ciaolibemacs(LibEmacs) :-
	( instype(local) ->
	    LibEmacs = ~path_concat(~emacsmode_dir, 'ciao-site-file.el')
	; % TODO: Place the version in the right place automatically?
	  % TODO: Verify that the rest of .el files are in the correct directory.
	  LibEmacs = ~path_concat(~instciao_bundledir(core), 'ciao-site-file.el')
	).

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
	    InitFile = ~path_concat(Dir, 'site-start.el')
	; % No init file has to be modified
	  % (see ciao_mode_init_desc/1 for site-start.d installation)
	  fail
	).

% Check that Dir is a site-start.d directory
is_site_start_d(Dir) :-
        atom_concat(_, '/site-start.d', Dir).

