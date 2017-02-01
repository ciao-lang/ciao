% (included file)

:- doc(section, "Shell Script Configuration").
% :- doc(section, "sh and csh scripts for environment setup").
% TODO: add description? "shell initialization scripts"

% The configuration of shell scripts defines the necessary environment
% variables to make the system locate the installed version of Ciao
% code and binaries (executables, libraries, documentation, etc.) in
% Unix systems.

% ===========================================================================
% Configuration

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [file_exists/1, get_home/1]).

:- bundle_flag(update_bashrc, [
    comment("Update bash init file"),
    details(
      % .....................................................................
      "Set to \"no\" if you do not wish to configure bash to work with Ciao or \n"||
      "if you wish to configure it by hand."),
    valid_values(['yes', 'no']),
    %
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_update_sh(SysregType, DefValue))),
    %
    interactive
]).

% ---------------------------------------------------------------------------

:- bundle_flag(dotbashrc, [
    comment("Bash initialization file"),
    details(
      % .....................................................................
      "The bash initialization file where the Ciao variables are set."),
    needed_if(flag(update_bashrc(yes))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_bashrc(SysregType, DefValue))),
    %
    interactive
]).

get_bashrc(all, F) :-
	( member(F, ['/etc/bash.bashrc', '/etc/bashrc']),
	    file_exists(F) ->
	    true
	; F = '/etc/bashrc'
	).
get_bashrc(user) := ~path_concat(~get_home, '.bashrc').

% ---------------------------------------------------------------------------

:- bundle_flag(update_cshrc, [
    comment("Update csh init file"),
    details(
      % .....................................................................
      "Set to \"no\" if you do not wish to configure csh/tcsh to work with\n"||
      "Ciao or if you wish to configure it by hand."),
    valid_values(['yes', 'no']),
    %
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_update_sh(SysregType, DefValue))),
    %
    interactive
]).

% ---------------------------------------------------------------------------

:- bundle_flag(dotcshrc, [
    comment("Csh/Tcsh initialization file"),
    details(
      % .....................................................................
      "The csh/tcsh initialization file where the Ciao variables are set.\n"||
      "Note that on some systems tcsh reads \"~/.tcshrc\"."),
    needed_if(flag(update_cshrc(yes))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_cshrc(SysregType, DefValue))),
    %
    interactive
]).

% by default, assume /etc/csh.cshrc
get_cshrc(all, F) :-
	( member(F, ['/etc/csh.cshrc', '/etc/tcsh.tcshrc']),
	    file_exists(F) ->
	    true
	; F = '/etc/csh.cshrc'
	).
get_cshrc(user) := ~get_cshrc_name.

% by default, assume .cshrc
get_cshrc_name(C) :-
	( ( member(F, ['.tcshrc', '.cshrc']),
	    C = ~path_concat(~get_home, F),
	    file_exists(C)
	  ) ->
	    true
	; F = '.cshrc',
	  C = ~path_concat(~get_home, F)
	).

get_update_sh('all',  'no').
get_update_sh('user', 'yes').

% ===========================================================================
% The ciao-env commmand (setups the environment for a given shell)

:- use_module(ciaobld(config_common), [default_eng_def/1]).

'$builder_hook'(item_nested(ciao_env)).
% (definition for installation)
% (merge below?)
%'$builder_hook'(ciao_env:cmd('ciao_env', [main='NONE_AUTOGEN', shscript])). % TODO: only for installation
'$builder_hook'(ciao_env:bin_copy_and_link(shscript, 'ciao-env', [])) :- !. % TODO: only for installation
'$builder_hook'(ciao_env:build_bin) :- % (overrides build)
	% NOTE: paths only valid for ciaoroot (not for bundles at CIAOPATH)
	( '$bundle_id'(lpdoc) ->
	    DocDirMan = ~docformatdir(manl),
	    DocDirInfo = ~docformatdir(info)
	; % TODO: incorrect! (we do not have lpdoc...)
	  DocDirMan = ~instciao_storedir,
	  DocDirInfo = ~instciao_storedir
	),
	Eng = ~default_eng_def,
	%
	wr_template(as_cmd(core, shscript), ~bundle_path(core, 'cmds'), 'ciao-env', [
            % 'CiaoDocDir' = DocDir,
	    'DocDirMan' = DocDirMan,
	    'DocDirInfo' = DocDirInfo,
	    'BinDir' = ~instciao_bindir,
	    %
	    'EngBin' = ~env_eng_path(exec_anyarch, Eng),
	    'EngHDir' = ~env_eng_path(hdir, Eng),
	    'DefaultLibDir' = ~get_bundle_flag(ciao:defaultlibdir)
        ]).

:- use_module(ciaobld(eng_defs),
	[inst_eng_path/3,
	 active_bld_eng_path/3,
	 active_inst_eng_path/3]).

env_eng_path(exec_anyarch, Eng) := Path :-
	( instype(local) ->
	    Path = ~active_bld_eng_path(exec_anyarch, Eng)
	; Path = ~active_inst_eng_path(exec_anyarch, Eng)
	).
env_eng_path(EngLoc, Eng) := Path :-
	( instype(local) ->
	    Path = ~bld_eng_path(EngLoc, Eng)
	; Path = ~inst_eng_path(EngLoc, Eng)
	).

% ===========================================================================
% Register in shell

:- use_module(ciaobld(config_common), [
    instype/1,
    instciao_bindir/1,
    instciao_storedir/1
]).

:- use_module(ciaobld(messages_aux), [verbose_message/2]).

:- use_module(library(system_extra), [warn_on_nosuccess/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(ciaobld(register_in_script), [
 	register_in_script/3, unregister_from_script/2]).
:- use_module(ciaobld(builder_aux), [wr_template/4]).

:- use_module(library(bundle/doc_flags), [docformatdir/2]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(lists), [append/3]).

% (for installation)
'$builder_hook'(dot_shell:item_dep(dot_shell_csh)).
'$builder_hook'(dot_shell:item_dep(dot_shell_sh)).

% TODO: Generate in builddir instead
etc_dir := ~bundle_path(core, 'etc').

% Shell initialization files

'$builder_hook'(dot_shell_sh:lib_file_list('etc', [ % (for installation)
  'DOTprofile'-[copy_and_link] 
])).
'$builder_hook'(dot_shell_sh:build_bin) :-
	dot_shell_gen(sh).
'$builder_hook'(dot_shell_sh:clean_bin) :-
	del_file_nofail(~bundle_path(core, 'etc/DOTprofile')).

'$builder_hook'(dot_shell_csh:lib_file_list('etc', [ % (for installation)
  'DOTcshrc'-[copy_and_link]
])).
'$builder_hook'(dot_shell_csh:build_bin) :-
	dot_shell_gen(csh).
'$builder_hook'(dot_shell_csh:clean_bin) :-
	del_file_nofail(~bundle_path(core, 'etc/DOTcshrc')).
	
dot_shell_gen(Sh) :-
	verbose_message("Creating ~w", [~dot_shell_file(Sh)]),
	% NOTE: paths only valid for ciaoroot (not for bundles at CIAOPATH)
	( '$bundle_id'(lpdoc) ->
	    DocDirMan = ~docformatdir(manl),
	    DocDirInfo = ~docformatdir(info)
	; % TODO: incorrect! (we do not have lpdoc...)
	  DocDirMan = ~instciao_storedir,
	  DocDirInfo = ~instciao_storedir
	),
	wr_template(origin, ~etc_dir, ~dot_shell_file(Sh), [
%	    'CiaoDocDir' = DocDir,
	    'DocDirMan' = DocDirMan,
	    'DocDirInfo' = DocDirInfo,
	    'BinDir' = ~instciao_bindir
        ]).

dot_shell_file(sh) := 'DOTprofile'.
dot_shell_file(csh) := 'DOTcshrc'.

update_shell(sh) := ~get_bundle_flag(core:update_bashrc).
update_shell(csh) := ~get_bundle_flag(core:update_cshrc).

dotshell(sh) := ~get_bundle_flag(core:dotbashrc).
dotshell(csh) := ~get_bundle_flag(core:dotcshrc).

'$builder_hook'(dot_shell_sh:register) :-
	register_shell(sh).
'$builder_hook'(dot_shell_sh:unregister) :-
	unregister_shell(sh).

'$builder_hook'(dot_shell_csh:register) :-
	register_shell(csh).
'$builder_hook'(dot_shell_csh:unregister) :- !,
	unregister_shell(csh).

register_shell(Sh) :-
	( update_shell(Sh, yes) ->
	    warn_on_nosuccess(register_in_script(~dotshell(Sh), "#", ~shell_lines(Sh)))
	; true
	).
unregister_shell(Sh) :-
	( update_shell(Sh, yes) ->
	    warn_on_nosuccess(unregister_from_script(~dotshell(Sh), "#"))
	; true
	).

shell_lines(Sh, S) :-
	register_etc_dir(EtcDir),
	DotFile = ~path_concat(EtcDir, ~dot_shell_file(Sh)),
	shell_config_code(Sh, DotFile, S, []).

register_etc_dir(EtcDir) :-
	( instype(local) ->
	    EtcDir = ~etc_dir
	; EtcDir = ~instciao_storedir
	).

% Configuration code for the shell script interpreters
shell_config_code(sh, DotFile) -->
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

