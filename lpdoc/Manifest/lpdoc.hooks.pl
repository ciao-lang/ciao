:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for LPdoc").

% ===========================================================================
:- doc(section, "Configuration rules").

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [get_pwnam/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/4]).

% TODO: default doc installation dir for 'local' inttype; it should not be used!
local_inst_doc_dir := ~bundle_path(core, builddir, 'doc').

:- bundle_flag(docdir, [
    comment("Installation directory for documentation"),
    details(
      % .....................................................................
      "Define this to be the directory in which you wish the documentation\n"||
      "to be installed."),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', local_inst_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      flag(ciao:install_prefix(Prefix)),
      get_docdir(SysregType, Prefix, DefValue))),
    %
    interactive
]).
get_docdir(all, Prefix) := ~path_concat(Prefix, 'share/doc/ciao').

:- bundle_flag(htmldir, [
    comment("Installation directory for HTML manuals"),
    details(
      % .....................................................................
      "Define this to be the directory in which you wish the documentation\n"||
      "in HTML format to be installed."),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', local_inst_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_htmldir(SysregType, DefValue))),
    %
    interactive
]).
% TODO: trailing /?
get_htmldir(all) := '/var/www/html/ciao'.
% get_htmldir(user) := ~atom_concat(~path_concat(~get_home, 'public_html/Ciao'), '/').

:- bundle_flag(htmlurl, [
    comment("URL for installed HTML documents"),
    details(
      % .....................................................................
      "Define the URL to access the previous directory via WWW."),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', local_inst_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_htmlurl(SysregType, DefValue))),
    %
    interactive
]).
% TODO: trailing /?
get_htmlurl(all) := '/ciao/'.
get_htmlurl(user) := ~path_concat(~atom_concat('/~', ~get_pwnam), 'Ciao/').

:- bundle_flag(mandir, [
    comment("Installation directory for 'man' pages"),
    details(
      % .....................................................................
      "Define this to be the directory in which you wish the man (unix manual\n"||
      "entry) file to be installed.\n"),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', local_inst_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      flag(ciao:install_prefix(Prefix)),
      get_mandir(SysregType, Prefix, DefValue))),
    %
    interactive
]).
get_mandir(all, Prefix) := ~path_concat(Prefix, 'share/man').

:- bundle_flag(infodir, [
    comment("Installation directory for 'info' files"),
    details(
      % .....................................................................
      "Define this to be the directory in which you wish the info file\n"||
      "installed.  Ideally, this directory should be accesible via emacs.\n"),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', local_inst_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      flag(ciao:install_prefix(Prefix)),
      get_infodir(SysregType, Prefix, DefValue))),
    %
    interactive
]).
get_infodir(all, Prefix) := ~path_concat(Prefix, 'share/info').

% ============================================================================
:- doc(section, "Build rules").

'$builder_hook'(prebuild_bin) :-
	generate_version_auto_lpdoc.

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(builder_aux), [generate_version_auto/2]).

% TODO: Add a version package instead?
% TODO: generate a config_auto.pl and put there some config flags (for condcomp)

% TODO: generalize for all bundles
% TODO: change modiftime only if there are changes
% TODO: include config, etc. (for runtime)?
generate_version_auto_lpdoc :-
	Bundle = lpdoc,
	File = ~bundle_path(Bundle, 'src/version_auto.pl'),
	generate_version_auto(Bundle, File).

% ===========================================================================
:- doc(section, "Tests and Benchmarks").

:- use_module(ciaobld(ciaoc_aux), [runtests_dir/2]).

'$builder_hook'(test) :- !,
	runtests_dir(lpdoc, 'src').


