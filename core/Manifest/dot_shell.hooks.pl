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

:- bundle_flag(update_shell, [
    comment("Update dot shell initialization files"),
    details(
      % .....................................................................
      "Enable automatically this Ciao installation in bash/zsh/csh shells."),
    valid_values(['yes', 'no']),
    %
    rule_default(DefValue, (
      flag(builder:registration_type(SysregType)),
      get_update_shell(SysregType, DefValue))),
    %
    interactive
]).

get_update_shell('all',  'no').
get_update_shell('user', 'yes').

:- bundle_flag(dotbashrc, [
    comment("Shell initialization file for bash"),
    details(
      % .....................................................................
      "Initialization file for bash that will be updated."),
    needed_if(flag(update_shell(yes))),
    rule_default(DefValue, (
      flag(builder:registration_type(SysregType)),
      locate_rc(SysregType, bash, DefValue))),
    %
    interactive
]).
:- bundle_flag(dotzshrc, [
    comment("Shell initialization file for zsh"),
    details(
      % .....................................................................
      "Initialization file for zsh that will be updated."),
    needed_if(flag(update_shell(yes))),
    rule_default(DefValue, (
      flag(builder:registration_type(SysregType)),
      locate_rc(SysregType, zsh, DefValue))),
    %
    interactive
]).
:- bundle_flag(dotcshrc, [
    comment("Shell initialization file for csh/tcsh"),
    details(
      % .....................................................................
      "Initialization file for csh/tcsh that will be updated."),
    needed_if(flag(update_shell(yes))),
    rule_default(DefValue, (
      flag(builder:registration_type(SysregType)),
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

rcfile(all, bash) := '/etc/bash.bashrc'.
rcfile(all, bash) := '/etc/bashrc'.
rcfile(all, zsh) := '/etc/zshrc'.
rcfile(all, csh) := '/etc/csh.cshrc'.
rcfile(all, csh) := '/etc/tcsh.tcshrc'.
rcfile(user, bash) := ~path_concat(~get_home, '.bashrc').
rcfile(user, zsh) := ~path_concat(~get_home, '.zshrc').
rcfile(user, csh) := ~path_concat(~get_home, '.cshrc').
rcfile(user, csh) := ~path_concat(~get_home, '.tcshrc').

% ===========================================================================
% The ciao-env commmand (setups the environment for a given shell)

:- use_module(ciaobld(config_common), [default_eng_def/1]).
:- use_module(ciaobld(install_aux), [final_ciao_root/1]).
:- use_module(ciaobld(builder_aux), [wr_template/4]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(ciaobld(eng_defs), [eng_mainmod/2]).

'$builder_hook'(item_nested(ciao_env)).
% (definition for installation)
% (merge below?)
%'$builder_hook'(ciao_env:cmd('ciao_env', [main='NONE_AUTOGEN', shscript])). % TODO: only for installation
'$builder_hook'(ciao_env:cmd_raw(shscript, 'ciao-env', [])) :- !. % TODO: only for installation
'$builder_hook'(ciao_env:build_bin) :- % (overrides build)
    Eng = ~default_eng_def,
    wr_template(as_cmd(core, shscript), ~bundle_path(core, 'cmds'), 'ciao-env', [
        'CiaoRoot' = ~final_ciao_root,
        'EngMainMod' = ~eng_mainmod(Eng)
    ]).

% ---------------------------------------------------------------------------
% Register in shell

:- use_module(ciaobld(messages_aux), [verbose_message/2]).

:- use_module(library(system_extra), [warn_on_nosuccess/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(ciaobld(register_in_script), [
    register_in_script/3, unregister_from_script/2]).
:- use_module(ciaobld(install_aux), [final_cmd_path/4]).

:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(lists), [append/3]).

update_shell := ~get_bundle_flag(core:update_shell).

dotshell(bash) := ~get_bundle_flag(core:dotbashrc).
dotshell(zsh) := ~get_bundle_flag(core:dotzshrc).
dotshell(csh) := ~get_bundle_flag(core:dotcshrc).

'$builder_hook'(dot_shell:register) :-
    ( update_shell(yes) ->
        register_shell(bash),
        register_shell(zsh),
        register_shell(csh)
    ; true
    ).
'$builder_hook'(dot_shell:unregister) :-
    ( update_shell(yes) ->
        unregister_shell(bash),
        unregister_shell(zsh),
        unregister_shell(csh)
    ; true
    ).

register_shell(Sh) :-
    CiaoEnv = ~final_cmd_path(core, shscript, 'ciao-env'),
    eval_ciao_env(Sh, CiaoEnv, Str, []),
    warn_on_nosuccess(register_in_script(~dotshell(Sh), "#", Str)).
unregister_shell(Sh) :-
    warn_on_nosuccess(unregister_from_script(~dotshell(Sh), "#")).

% Configuration code for the shell script interpreters
% (evaluates output of ciao-env)
eval_ciao_env(bash, CiaoEnv) -->
    env_note,
    "if [ -x ", emit_atom(CiaoEnv), " ] ; then\n"||
    "  eval \"$(", emit_atom(CiaoEnv), " --sh)\"\n"||
    "fi\n".
eval_ciao_env(zsh, CiaoEnv) -->
    eval_ciao_env(bash, CiaoEnv). % (same as bash)
eval_ciao_env(csh, CiaoEnv) -->
    env_note,
    "if ( -x ", emit_atom(CiaoEnv), " ) then\n"||
    "  eval `", emit_atom(CiaoEnv), " --csh`\n"||
    "endif\n".

env_note -->
    "# You should customize CIAOPATH before this chunk if you place bundles in\n",
    "# places other than ~/.ciao\n".

% (emit an atom codes in a DCG)
emit_atom(X, S, S0) :-
    atom_codes(X, Codes),
    append(Codes, S0, S).


