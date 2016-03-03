:- package(doccfg).

% Package to define a LPdoc manual configuration settings
:- use_package(fsyntax).
:- use_package(assertions).
:- use_package(regtypes).

%:- use_module(lpdoclib(doccfg_props)).

:- use_module(library(bundle/paths_extra), [fsR/2]).

:- include(lpdoclib('SETTINGS_schema')).

:- doc(filetype, user).

