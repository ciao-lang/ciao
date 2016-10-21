% (included file)

:- use_module(library(pathnames), [path_concat/3]).

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

