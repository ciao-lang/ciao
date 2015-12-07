% (included file)

:- doc(section, "Command-line Enviroment").

:- bundle_flag(update_bashrc, [
    comment("Update bash init file"),
    valid_values(['yes', 'no']),
    %
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_update_sh(SysregType, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Set to \"no\" if you do not wish to configure bash to work with Ciao or \n"||
      "if you wish to configure it by hand.")
]).

% ---------------------------------------------------------------------------

:- bundle_flag(dotbashrc, [
    comment("Bash initialization file"),
    needed_if(flag(update_bashrc(yes))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_bashrc(SysregType, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "The bash initialization file where the Ciao variables are set.")
]).

get_bashrc(all, F) :-
	( member(F, ['/etc/bash.bashrc', '/etc/bashrc']),
	    file_exists(F) ->
	    true
	; F = '/etc/bashrc'
	).
get_bashrc(user) := ~fsR(~get_home/'.bashrc').

% ---------------------------------------------------------------------------

:- bundle_flag(update_cshrc, [
    comment("Update csh init file"),
    valid_values(['yes', 'no']),
    %
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_update_sh(SysregType, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Set to \"no\" if you do not wish to configure csh/tcsh to work with\n"||
      "Ciao or if you wish to configure it by hand.")
]).

% ---------------------------------------------------------------------------

:- bundle_flag(dotcshrc, [
    comment("Csh/Tcsh initialization file"),
    needed_if(flag(update_cshrc(yes))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_cshrc(SysregType, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "The csh/tcsh initialization file where the Ciao variables are set.\n"||
      "Note that on some systems tcsh reads \"~/.tcshrc\".")
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
	    C = ~fsR((~get_home)/F),
	    file_exists(C)
	  ) ->
	    true
	; F = '.cshrc',
	  C = ~fsR((~get_home)/F)
	).

get_update_sh('all',  'no').
get_update_sh('user', 'yes').

