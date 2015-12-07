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

:- use_module(library(system_extra), [(-)/1]). % (for register in script)
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(ciaobld(register_in_script), [
 	register_in_script/3, unregister_from_script/2]).

:- use_module(library(bundle/doc_flags), [docformatdir/2]).
:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(library(lists), [append/3]).

'$builder_hook'(dot_shell:build_nodocs) :-
	% TODO: build_nodocs or prebuild_nodocs?
	% TODO: those files should be generated in builddir
	bundleitem_do(dot_shell_csh, core, build_nodocs),
	bundleitem_do(dot_shell_sh, core, build_nodocs).
'$builder_hook'(dot_shell:clean_norec) :-
	del_file_nofail(~fsR(bundle_src(core)/etc/'DOTprofile')),
	del_file_nofail(~fsR(bundle_src(core)/etc/'DOTcshrc')).

% TODO: Generate in builddir instead
etc_dir := ~fsR(bundle_src(core)/'etc').

% Generate shell initialization files
'$builder_hook'(dot_shell_sh:item_build_nodocs) :-
	dot_shell_gen(sh).

'$builder_hook'(dot_shell_csh:item_build_nodocs) :-
	dot_shell_gen(csh).
	
dot_shell_gen(Sh) :-
	verbose_message("Creating ~w", [~dot_shell_file(Sh)]),
	wr_template(origin, ~etc_dir, ~dot_shell_file(Sh), [
	    'CiaoDocDir' = ~docformatdir(any),
	    'CiaoBinDir' = ~instciao_bindir
        ]).

dot_shell_file(csh) := 'DOTcshrc'.
dot_shell_file(sh) := 'DOTprofile'.

'$builder_hook'(dot_shell_:item_def(
    lib_file_list(core, ~etc_dir, [
      'DOTprofile'-[copy_and_link],
      'DOTcshrc'-[copy_and_link]
    ]))).

update_bashrc := ~get_bundle_flag(core:update_bashrc).
dotbashrc := ~get_bundle_flag(core:dotbashrc).

'$builder_hook'(bashrc:item_register) :-
	( update_bashrc(yes) ->
	    (-register_in_script(~dotbashrc, "#", ~bashrc_lines))
	; true
	).
'$builder_hook'(bashrc:item_unregister) :-
	( update_bashrc(yes) ->
	    (-unregister_from_script(~dotbashrc, "#"))
	; true
	).

update_cshrc := ~get_bundle_flag(core:update_cshrc).
dotcshrc := ~get_bundle_flag(core:dotcshrc).

'$builder_hook'(cshrc:item_register) :-
	( update_cshrc(yes) ->
	    (-register_in_script(~dotcshrc, "#", ~cshrc_lines))
	; true
	).
'$builder_hook'(cshrc:item_unregister) :- !,
	( update_cshrc(yes) ->
	    (-unregister_from_script(~dotcshrc, "#"))
	; true
	).

bashrc_lines(S) :-
	register_etc_dir(EtcDir),
	shell_config_code(bash, EtcDir, S, []).
cshrc_lines(S) :-
	register_etc_dir(EtcDir),
	shell_config_code(csh, EtcDir, S, []).

register_etc_dir(EtcDir) :-
	( instype(local) ->
	    EtcDir = ~etc_dir
	; EtcDir = ~instciao_storedir
	).

% Configuration code for the shell script interpreters
shell_config_code(bash, EtcDir) -->
	"if [ -f ", emit_atom(EtcDir), "/DOTprofile ] ; then\n"||
	"  . ", emit_atom(EtcDir), "/DOTprofile\n"||
	"fi\n".
shell_config_code(csh, EtcDir) -->
	"if ( -e ", emit_atom(EtcDir), "/DOTcshrc ) then\n"||
	"  source ", emit_atom(EtcDir), "/DOTcshrc\n"||
	"endif\n".

% (emit an atom codes in a DCG)
emit_atom(X, S, S0) :-
	atom_codes(X, Codes),
	append(Codes, S0, S).

