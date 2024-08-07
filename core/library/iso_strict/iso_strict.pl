:- package(iso_strict).

:- use_package(dcg).
%:- use_package('dcg/dcg_phrase').
%:- use_package(dynamic). % TODO: refine imports to ISO subset (and includes datafacts)
:- use_package(datafacts). % TODO: refine imports to ISO subset
:- op(1150, fx, [(dynamic)]). % TODO: dynamic.pl conflicts with dynamic_rt, fix
:- use_package(dynamic_clauses). % TODO: refine imports to ISO subset (and includes datafacts)

%:- use_module(engine(hiord_rt), [call/1]).
:- use_package(hiord). % TODO: refine imports to ISO subset (required for call/N)
:- use_module(engine(basic_props), [callable/1]). % TODO: move to engine(term_typing)

%:- use_package(runtime_ops). % TODO: avoid loading operators:op/3
:- load_compilation_module(library(runtime_ops/runtime_ops_tr)).
:- add_sentence_trans(runtime_ops_tr:runtime_op/2, 210).


:- use_module(engine(runtime_control)). % TODO: refine imports to ISO subset

:- use_module(library(aggregates)).
:- use_module(library(sort), [sort/2, keysort/2]).
:- use_module(library(iso_misc)).
:- use_module(library(terms_check), [subsumes_term/2]).
:- use_module(library(terms_vars), [term_variables/2]).
:- use_module(library(cyclic_terms), [acyclic_term/1]).

:- use_module(library(format)).

:- if(defined(optim_comp)).
:- else.
:- use_module(library(compiler)). % TODO: refine imports to ISO subset
:- endif.
:- use_module(library(system)). % TODO: refine imports to ISO subset

:- use_module(library(iso_incomplete)).

% IO predicates
:- use_module(engine(stream_basic), [
    % TODO: ISO?
    character_count/2, line_count/2, line_position/2,
    flush_output/0,
    current_stream/3
]).
:- use_module(engine(io_basic), [
    % TODO: refine imports to ISO subset
    put_code/1,
    nl/0,
    tab/1, 
    at_end_of_stream/0,
    display/1,
    displayq/1
]).
:- use_module(library(iso_char), [
    char_code/2,
    atom_chars/2,
    number_chars/2,
    char_codes/2
]).

:- use_module(library(write), [
    writeq/1,
    write_canonical/1,
    print/1, printq/1,
    portray_clause/1,
    numbervars/3
]).

