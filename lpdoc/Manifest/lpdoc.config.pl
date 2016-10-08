:- module(_, [], [ciaobld(bundleconfig)]).

:- doc(title, "Configuration rules for LPdoc").
:- doc(author, "Ciao Development Team").

:- use_module(library(system), [get_pwnam/1]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/4]).
:- use_module(library(bundle/bundle_info), [root_bundle/1]).

% ===========================================================================

% Default paths for LPdoc output
build_doc_dir := ~bundle_path(~root_bundle, builddir, 'doc').

:- bundle_flag(docdir, [
    comment("Installation directory for documentation"),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', build_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      flag(ciao:install_prefix(Prefix)),
      get_docdir(SysregType, Prefix, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Define this to be the directory in which you wish the documentation\n"||
      "to be installed.")
]).
get_docdir(all, Prefix) := ~path_concat(Prefix, 'share/doc/ciao').

:- bundle_flag(htmldir, [
    comment("Installation directory for HTML manuals"),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', build_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_htmldir(SysregType, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Define this to be the directory in which you wish the documentation\n"||
      "in HTML format to be installed.")
]).
% TODO: trailing /?
get_htmldir(all) := '/var/www/html/ciao'.
% get_htmldir(user) := ~atom_concat(~path_concat(~get_home, 'public_html/Ciao'), '/').

:- bundle_flag(htmlurl, [
    comment("URL for installed HTML documents"),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', build_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      get_htmlurl(SysregType, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Define the URL to access the previous directory via WWW.")
]).
% TODO: trailing /?
get_htmlurl(all) := '/ciao/'.
get_htmlurl(user) := ~path_concat(~atom_concat('/~', ~get_pwnam), 'Ciao/').

:- bundle_flag(mandir, [
    comment("Installation directory for 'man' pages"),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', build_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      flag(ciao:install_prefix(Prefix)),
      get_mandir(SysregType, Prefix, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Define this to be the directory in which you wish the man (unix manual\n"||
      "entry) file to be installed.\n")
]).
get_mandir(all, Prefix) := ~path_concat(Prefix, 'share/man').

:- bundle_flag(infodir, [
    comment("Installation directory for 'info' files"),
    rule_set_value(Value, (
      flag(ciao:instype(InsType)),
      InsType == 'local', build_doc_dir(Value))),
    rule_default(DefValue, (
      flag(ciao:registration_type(SysregType)),
      flag(ciao:install_prefix(Prefix)),
      get_infodir(SysregType, Prefix, DefValue))),
    %
    interactive([minimum, extended],
      % .....................................................................
      "Define this to be the directory in which you wish the info file\n"||
      "installed.  Ideally, this directory should be accesible via emacs.\n")
]).
get_infodir(all, Prefix) := ~path_concat(Prefix, 'share/info').

% ---------------------------------------------------------------------------
