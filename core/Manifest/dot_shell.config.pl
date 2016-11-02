% (included file)

:- use_module(library(pathnames), [path_concat/3]).

:- doc(section, "Command-line Enviroment").

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

