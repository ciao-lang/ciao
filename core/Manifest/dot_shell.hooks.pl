% (included file)

:- doc(section, "Shell Script Configuration").

% The configuration of shell scripts defines the necessary environment
% variables to make the system locate the installed version of Ciao
% code and binaries (executables, libraries, documentation, etc.) in
% Unix systems.

% ===========================================================================
% Configuration

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [file_exists/1, get_home/1]).

:- bundle_flag(update_bashrc, [
    comment("Update bash initialization file"),
    details(
      % .....................................................................
      "Enable automatically this Ciao installation in bash shells."),
    valid_values(['yes', 'no']),
    %
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_update_sh(SysregType, DefValue))),
    %
    interactive
]).
:- bundle_flag(update_cshrc, [
    comment("Update csh initialization file"),
    details(
      % .....................................................................
      "Enable automatically this Ciao installation in csh shells."),
    valid_values(['yes', 'no']),
    %
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_update_sh(SysregType, DefValue))),
    %
    interactive
]).

get_update_sh('all',  'no').
get_update_sh('user', 'yes').

:- bundle_flag(dotbashrc, [
    comment("Shell initialization file for bash"),
    details(
      % .....................................................................
      "Initialization file for bash that will be updated."),
    needed_if(flag(update_bashrc(yes))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      locate_rc(SysregType, sh, DefValue))),
    %
    interactive
]).
:- bundle_flag(dotcshrc, [
    comment("Csh/Tcsh initialization file"),
    details(
      % .....................................................................
      "Initialization file for csh/tcsh that will be updated."),
    needed_if(flag(update_cshrc(yes))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      locate_rc(SysregType, csh, DefValue))),
    %
    interactive
]).

locate_rc(SysregType, Sh) := F :-
	( F= ~rcfile(SysregType, Sh),
	  file_exists(F) -> % first that exists
	    true
	; % or just first
	  F0 = ~rcfile(SysregType, Sh), !,
	  F = F0
	).

rcfile(all, sh) := '/etc/bash.bashrc'.
rcfile(all, sh) := '/etc/bashrc'.
rcfile(all, csh) := '/etc/csh.cshrc'.
rcfile(all, csh) := '/etc/tcsh.tcshrc'.
rcfile(user, sh) := ~path_concat(~get_home, '.bashrc').
rcfile(user, csh) := ~path_concat(~get_home, '.cshrc').
rcfile(user, csh) := ~path_concat(~get_home, '.tcshrc').

% ===========================================================================
% The ciao-env commmand (setups the environment for a given shell)

:- use_module(ciaobld(config_common), [default_eng_def/1]).
:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

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

% ---------------------------------------------------------------------------
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

:- use_module(library(bundle/doc_flags), [docformatdir/2]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(lists), [append/3]).

'$builder_hook'(dot_shell:item_dep(dot_shell_csh)).
'$builder_hook'(dot_shell:item_dep(dot_shell_sh)).

update_shell(sh) := ~get_bundle_flag(core:update_bashrc).
update_shell(csh) := ~get_bundle_flag(core:update_cshrc).

dotshell(sh) := ~get_bundle_flag(core:dotbashrc).
dotshell(csh) := ~get_bundle_flag(core:dotcshrc).

'$builder_hook'(dot_shell_sh:register) :- register_shell(sh).
'$builder_hook'(dot_shell_sh:unregister) :- unregister_shell(sh).

'$builder_hook'(dot_shell_csh:register) :- register_shell(csh).
'$builder_hook'(dot_shell_csh:unregister) :- unregister_shell(csh).

register_shell(Sh) :-
	( update_shell(Sh, yes) ->
	    CiaoEnv = ~env_cmd_path(core, shscript, 'ciao-env'),
	    eval_ciao_env(Sh, CiaoEnv, Str, []),
	    warn_on_nosuccess(register_in_script(~dotshell(Sh), "#", Str))
	; true
	).
unregister_shell(Sh) :-
	( update_shell(Sh, yes) ->
	    warn_on_nosuccess(unregister_from_script(~dotshell(Sh), "#"))
	; true
	).

% Configuration code for the shell script interpreters
% (evaluates output of ciao-env)
eval_ciao_env(sh, CiaoEnv) -->
	"# You should customize CIAOPATH before this chunk\n",
	"if [ -x ", emit_atom(CiaoEnv), " ] ; then\n"||
	"  eval \"$(", emit_atom(CiaoEnv), " --sh)\"\n"||
	"fi\n".
eval_ciao_env(csh, CiaoEnv) -->
	"# You should customize CIAOPATH before this chunk\n",
	"if ( -x ", emit_atom(CiaoEnv), " ) then\n"||
	"  eval `", emit_atom(CiaoEnv), " --csh`\n"||
	"endif\n".

% (emit an atom codes in a DCG)
emit_atom(X, S, S0) :-
	atom_codes(X, Codes),
	append(Codes, S0, S).

% ---------------------------------------------------------------------------
% Paths to engine and commands for the given installation type

:- use_module(ciaobld(eng_defs),
	[inst_eng_path/3,
	 active_bld_eng_path/3,
	 active_inst_eng_path/3]).

env_eng_path(exec_anyarch, Eng) := Path :- !,
	( instype(local) ->
	    Path = ~active_bld_eng_path(exec_anyarch, Eng)
	; Path = ~active_inst_eng_path(exec_anyarch, Eng)
	).
env_eng_path(EngLoc, Eng) := Path :-
	( instype(local) ->
	    Path = ~bld_eng_path(EngLoc, Eng)
	; Path = ~inst_eng_path(EngLoc, Eng)
	).

:- use_module(ciaobld(config_common),
	[bld_cmd_path/4,
	 inst_cmd_path/4]).

env_cmd_path(Bundle, Kind, File) := Path :-
	( instype(local) ->
	    Path = ~bld_cmd_path(Bundle, Kind, File)
	; Path = ~inst_cmd_path(Bundle, Kind, File)
	).

