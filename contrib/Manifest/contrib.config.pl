:- module(_, [], [ciaobld(bundleconfig)]).

:- doc(title, "Configuration rules for Contrib").
:- doc(author, "Ciao Development Team").

:- use_module(library(process), [process_call/3]).
:- use_module(library(system), [find_executable/2]).
:- use_module(library(bundle/paths_extra), [fsR/2]).

:- discontiguous(m_bundle_foreign_config_tool/3).

:- include(.('gsl.config')).
:- include(.('ppl.config')).
:- include(.('mathematica.config')).

