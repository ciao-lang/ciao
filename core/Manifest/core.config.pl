:- module(_, [], [ciaobld(bundleconfig)]).

:- doc(title, "Configuration rules for Ciao core").
:- doc(author, "Ciao Development Team").

:- use_module(library(system), [file_exists/1, find_executable/2, get_home/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(lists), [append/3]).

:- use_module(library(bundle/paths_extra), [fsR/2]).
:- use_module(ciaobld(detcheader), [detect_c_headers/1]).

% ===========================================================================

:- include(.('engine.config')).
:- include(.('compiler.config')).
:- include(.('dot_shell.config')).
:- include(.('emacs_mode.config')).
:- include(.('pillow.config')).
:- include(.('persdb_mysql.config')).
:- include(.('java.config')).
:- include(.('ant.config')).

% ===========================================================================

:- doc(section, "Options for Ciao/Toplevel").

:- bundle_flag(install_prolog_name, [
    comment("Symbolic link from Ciao to 'prolog' executable"),
    valid_values(['yes', 'no']),
    %
    rule_default('yes'),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Set to \"yes\" if you wish to create a link that brings up Ciao \n"||
      "when you type \"prolog\". You may want to say no if there are other\n"||
      "systems that support the Prolog language in your machine and you\n"||
      "do not want to make Ciao the default.")
]).

