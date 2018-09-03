:- package(iso).

:- use_package(dcg).
%:- use_package('dcg/dcg_phrase').
:- use_package(dynamic). % TODO: refine, not all are ISO (and includes datafacts)

:- use_module(library(aggregates)).
:- use_module(library(iso_misc)).
:- use_module(library(iso_char)).
:- use_module(library(iso_incomplete)).
:- use_module(library(operators)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(terms_check), [subsumes_term/2]).
:- use_module(library(terms_vars), [term_variables/2]).
:- use_module(library(cyclic_terms), [acyclic_term/1]).

:- use_module(engine(stream_basic)). % TODO: refine, not all are ISO
:- use_module(engine(io_basic)). % TODO: refine, not all are ISO
:- use_module(engine(runtime_control)). % TODO: refine, not all are ISO
:- use_module(engine(hiord_rt), [call/1]).
