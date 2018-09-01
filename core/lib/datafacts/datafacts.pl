:- package(datafacts).
:- if('$with_compiler_version'(108)).
:- use_module(engine(data_facts)).
:- else.
:- use_module(library(datafacts/datafacts_rt)).
:- endif.

