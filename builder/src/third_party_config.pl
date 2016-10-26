:- module(third_party_config, [], [assertions, basicmodes, fsyntax, hiord]).

:- doc(title, "Query configuration of third-party components").
:- doc(author, "Ciao Development Team").

:- doc(module, "This module queries configuration values of installed
   third-party libraries (currently only @tt{GNU pkg-config} based
   libraries are supported). These libraries can be available in
   standard locations or auto-installed under @tt{third-party/}.").

:- use_module(library(lists), [append/3]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [file_exists/1, find_executable/2]).

:- use_module(library(bundle/bundle_flags), [current_bundle_flag/2]).
:- use_module(ciaobld(third_party_install), [third_party_path/2]).

:- include(ciaobld(bundleconfig/bundleconfig_defs)). % (only for m_bundle_foreign_config_tool/3)

% (Support for GNU pkg-config based libraries)

:- export(foreign_config_var/3).
% The configuration for foreign library @var{Foreign} has value
% @var{Value} in variable @var{Var}.
foreign_config_var(Foreign, Var, Value) :-
	foreign_config_tool_path(_, Foreign, CfgToolPath),
	process_call(CfgToolPath, [~atom_concat('--', Var)],
	       [stdout(line(Value)), status(0)]).

% TODO: cache path
foreign_config_tool_path(Bundle, Foreign, CfgToolPath) :-
	% TODO: do not use m_bundle_foreign_config_tool? use third party decls instead?
	m_bundle_foreign_config_tool(Bundle, Foreign, CfgTool),
	( % TODO: hack! make it nicer by unifying names of third_party,
	  % foreign libraries, and AUTO_INSTALL_* params (make it a functor)
	  atom_concat('auto_install_', Foreign, AutoInstallOpt),
	  % (do not throw exception if does not exists)
	  current_bundle_flag(Bundle:AutoInstallOpt, 'yes') ->
	    % Look in third-party bin
	    third_party_path(bindir, ThirdPartyBinDir),
	    path_concat(ThirdPartyBinDir, CfgTool, CfgToolPath),
	    file_exists(CfgToolPath)
	; find_executable(CfgTool, CfgToolPath)
	).

:- export(foreign_config_version/2).
foreign_config_version(Foreign, Version) :-
	foreign_config_var(Foreign, 'version', Str),
	foreign_config_parse_version(Str, Version).

% from "Major.Minor" string to [Major,Minor]
foreign_config_parse_version(Str, L) :-
	( append(StrH, "." || StrT, Str) ->
	    L = [H|T],
	    number_codes(H, StrH),
	    foreign_config_parse_version(StrT, T)
	; L = [H],
	  number_codes(H, Str)
	).

