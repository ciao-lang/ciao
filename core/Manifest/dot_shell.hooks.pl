% (included file)

:- doc(section, "Shell Script Configuration").
% :- doc(section, "sh and csh scripts for environment setup").
% TODO: add description? "shell initialization scripts"

% The configuration of shell scripts defines the necessary environment
% variables to make the system locate the installed version of Ciao
% code and binaries (executables, libraries, documentation, etc.) in
% Unix systems.

:- use_module(ciaobld(config_common), [
    instype/1,
    instciao_bindir/1,
    instciao_storedir/1
]).

:- use_module(library(system_extra), [warn_on_nosuccess/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(ciaobld(register_in_script), [
 	register_in_script/3, unregister_from_script/2]).

:- use_module(library(bundle/doc_flags), [docformatdir/2]).
:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(library(lists), [append/3]).

'$builder_hook'(dot_shell:item_def([ % (for installation)
  dot_shell_csh,
  dot_shell_sh
])).

% TODO: Generate in builddir instead
etc_dir := ~bundle_path(core, 'etc').

% Generate shell initialization files
'$builder_hook'(dot_shell_sh:item_def( % (for installation)
    lib_file_list('etc', [
      'DOTprofile'-[copy_and_link]
    ]))).
'$builder_hook'(dot_shell_sh:build_nodocs) :-
	dot_shell_gen(sh).
'$builder_hook'(dot_shell_sh:clean_norec) :-
	del_file_nofail(~bundle_path(core, 'etc/DOTprofile')).

'$builder_hook'(dot_shell_csh:item_def( % (for installation)
    lib_file_list('etc', [
      'DOTcshrc'-[copy_and_link]
    ]))).
'$builder_hook'(dot_shell_csh:build_nodocs) :-
	dot_shell_gen(csh).
'$builder_hook'(dot_shell_csh:clean_norec) :-
	del_file_nofail(~bundle_path(core, 'etc/DOTcshrc')).
	
dot_shell_gen(Sh) :-
	verbose_message("Creating ~w", [~dot_shell_file(Sh)]),
	wr_template(origin, ~etc_dir, ~dot_shell_file(Sh), [
	    'CiaoDocDir' = ~docformatdir(any),
	    'CiaoBinDir' = ~instciao_bindir
        ]).

dot_shell_file(csh) := 'DOTcshrc'.
dot_shell_file(sh) := 'DOTprofile'.

update_bashrc := ~get_bundle_flag(core:update_bashrc).
dotbashrc := ~get_bundle_flag(core:dotbashrc).

'$builder_hook'(dot_shell_sh:register) :-
	( update_bashrc(yes) ->
	    warn_on_nosuccess(register_in_script(~dotbashrc, "#", ~bashrc_lines))
	; true
	).
'$builder_hook'(dot_shell_sh:unregister) :-
	( update_bashrc(yes) ->
	    warn_on_nosuccess(unregister_from_script(~dotbashrc, "#"))
	; true
	).

update_cshrc := ~get_bundle_flag(core:update_cshrc).
dotcshrc := ~get_bundle_flag(core:dotcshrc).

'$builder_hook'(dot_shell_csh:register) :-
	( update_cshrc(yes) ->
	    warn_on_nosuccess(register_in_script(~dotcshrc, "#", ~cshrc_lines))
	; true
	).
'$builder_hook'(dot_shell_csh:unregister) :- !,
	( update_cshrc(yes) ->
	    warn_on_nosuccess(unregister_from_script(~dotcshrc, "#"))
	; true
	).

bashrc_lines(S) :-
	register_etc_dir(EtcDir),
	DotFile = ~path_concat(EtcDir, 'DOTprofile'),
	shell_config_code(bash, DotFile, S, []).
cshrc_lines(S) :-
	register_etc_dir(EtcDir),
	DotFile = ~path_concat(EtcDir, 'DOTcshrc'),
	shell_config_code(csh, DotFile, S, []).

register_etc_dir(EtcDir) :-
	( instype(local) ->
	    EtcDir = ~etc_dir
	; EtcDir = ~instciao_storedir
	).

% Configuration code for the shell script interpreters
shell_config_code(bash, DotFile) -->
	"if [ -f ", emit_atom(DotFile), " ] ; then\n"||
	"  . ", emit_atom(DotFile), "\n"||
	"fi\n".
shell_config_code(csh, DotFile) -->
	"if ( -e ", emit_atom(DotFile), " ) then\n"||
	"  source ", emit_atom(DotFile), "\n"||
	"endif\n".

% (emit an atom codes in a DCG)
emit_atom(X, S, S0) :-
	atom_codes(X, Codes),
	append(Codes, S0, S).

