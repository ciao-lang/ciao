% (included file)

:- doc(section, "Options for Emacs Mode").

:- bundle_flag(with_emacs_mode, [
    comment("Enable Emacs-based IDE"),
    valid_values(['yes', 'no']),
    %
    default_comment("Emacs detected"),
    rule_default(VerifyEmacs, verify_emacs(VerifyEmacs)),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Set to \"yes\" if you wish to install the Ciao emacs libraries which\n"||
      "implement the Emacs-based IDE (integrated development environment)\n"||
      "(highly recommended).  It should be set to no if emacs is not\n"||
      "installed in the system.  It is safe to leave as \"yes\" otherwise.")
]).

verify_emacs(Value) :-
	( emacs_installed -> Value = yes ; Value = no ).

emacs_installed :- find_emacs(_).

% TODO: it should consider auto_install option!
find_emacs(File) :- find_executable('emacs', File).

% ---------------------------------------------------------------------------

:- bundle_flag(emacs_for_ciao, [
    comment("Emacs version to be used"),
    needed_if(flag(with_emacs_mode(yes))),
    rule_default(DefValue, find_emacs(DefValue)),
    %
    interactive([minimum, extended],
      % .....................................................................
      "The version of emacs that you wish to use with Ciao. The development\n"||
      "environment will be compiled for use with this version.")
]).

% ---------------------------------------------------------------------------

% TODO: Change name
:- bundle_flag(update_dotemacs, [
    comment("Modify emacs init file"),
    valid_values(['yes', 'no']),
    %
    needed_if(flag(with_emacs_mode(yes))),
    % TODO: This set value means that no question is really asked
    rule_set_value(Value, (
      flag(ciao:registration_type(SysregType)),
      update_dotemacs_(SysregType, Value))),
    rule_default('yes'),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Set to \"yes\" if you wish to configure emacs to work with Ciao\n"||
      "(modify emacs initialization file).")
]).

% update_dotemacs_(InsType, VerifyEmacs, UpdateEmacs)

update_dotemacs_(all,  no) :- !.
update_dotemacs_(user, yes).

% ---------------------------------------------------------------------------

:- bundle_flag(dotemacs, [
    comment("Emacs initialization file"),
    needed_if(flag(update_dotemacs(yes))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_dotemacs(SysregType, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Define the emacs initialization file where the Ciao settings will be\n"||
      "added.")
]).

get_dotemacs(user) := ~fsR(~get_home/'.emacs').

% ---------------------------------------------------------------------------

% TODO: Do not customize this
:- bundle_flag(emacsinitfile, [
    comment("Emacs init file"),
    needed_if(flag(with_emacs_mode(yes))),
    rule_set_value(Value, (
      flag(ciao:registration_type(SysregType)),
      get_emacs_init_file(SysregType, Value))),
    interactive([], % TODO: configurable?
      % .....................................................................
      "Specify the name of the emacs lisp file defining the Ciao mode.")
]).

get_emacs_init_file(all, '65ciao-mode-init.el') :-
	% Use if it is debian based
	get_os('LINUX'),
	file_exists('/etc/debian_version'),
	!.
get_emacs_init_file(_, 'ciao-mode-init.el').

% ---------------------------------------------------------------------------

:- bundle_flag(emacs_site_start, [
    comment("Emacs site start"),
    needed_if(flag(with_emacs_mode(yes))),
    rule_default(Value, (
      flag(ciao:registration_type(SysregType)),
      flag(ciao:instype(InsType)),
      get_emacs_site_start(emacs, SysregType, InsType, Value))),
    %
    interactive([extended],
      % .....................................................................
      "Specify in what file/directory you want to insert/copy the Ciao Emacs\n"||
      "Mode initialization code.")
]).

:- use_module(ciaobld(config_common), [instciao_bundledir/2]).
:- use_module(library(system), [winpath/2]).

get_emacs_site_start(EmacsKind, all, global, Value) :-
	emacs_site_start_(EmacsKind, Value),
	!.
get_emacs_site_start(_, _, InsType, Value) :-
	( InsType = local -> Value0 = ~fsR(bundle_src(core)) % TODO: strange
	; InsType = global -> Value0 = ~instciao_bundledir(core)
	; fail
	),
	winpath(Value, Value0). % (translate in Unix format, if necessary)

emacs_site_start_(EmacsKind, SiteStart) :-
	possible_emacs_site_start(EmacsKind, SiteStart),
	file_exists(SiteStart),
	!.

% Note: this returns files or directories, as follows:
%  .../site-lisp/site-start.d  ==> must put a script in that directory
%  .../site-lisp               ==> must use/create a site-start.el
possible_emacs_site_start(emacs) :=
	'/Applications/Emacs.app/Contents/Resources/site-lisp/site-start.d'|
	'/Applications/Emacs.app/Contents/Resources/site-lisp'|
	'/usr/share/emacs/site-lisp/site-start.d'|
	'/usr/share/emacs/site-lisp'|
	'/etc/emacs/site-start.d'.

