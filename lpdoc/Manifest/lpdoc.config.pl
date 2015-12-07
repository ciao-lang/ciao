:- module(_, [], [ciaobld(bundleconfig)]).

:- doc(title, "Configuration rules for LPdoc").
:- doc(author, "Ciao Development Team").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [get_pwnam/1]).

:- use_module(library(bundle/paths_extra), [fsR/2]).

% ===========================================================================

% Default paths for LPdoc output
build_doc_dir := ~fsR(builddir_doc(build)).

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
get_docdir(all,  Prefix) := ~fsR(Prefix/'share'/'doc'/'ciao').

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
% get_htmldir(user) := ~atom_concat(~fsR(~get_home/'public_html'/'Ciao'), '/').

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
get_htmlurl(user) := ~atom_concat(['/~', ~get_pwnam, '/Ciao/']).

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
get_mandir(all,  Prefix) := ~fsR(Prefix/'share'/'man').

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
get_infodir(all, Prefix) := ~fsR(Prefix/'share'/'info').

% ---------------------------------------------------------------------------

%% The .bib files are in the repository (i.e. as SVN external),
%% it makes no sense configuring this. (JFMC)
% :- bundle_flag(bibfiles, [
%     interactive([],
%       % .....................................................................
%       "Specifies the bibtex files used to create the bibliography of the\n" ||
%       "documentation."),
%     rule_default('/home/clip/bibtex/clip/clip,/home/clip/bibtex/clip/general')
% ]).

% ---------------------------------------------------------------------------





