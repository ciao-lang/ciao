:- package(iso_strict).

:- use_package(dcg).
%:- use_package('dcg/dcg_phrase').
:- use_package(dynamic). % TODO: refine, not all are ISO (and includes datafacts)

%:- use_module(engine(hiord_rt), [call/1]).
:- use_package(hiord). % TODO: refine, not all are ISO (required for call/N)
:- use_module(engine(basic_props), [callable/1]).

:- use_package(runtime_ops).
:- use_module(library(operators)).

:- use_module(engine(runtime_control)). % TODO: refine, not all are ISO

:- use_module(library(aggregates)).
:- use_module(library(sort), [sort/2, keysort/2]).
:- use_module(library(iso_misc)).
:- use_module(library(terms_check), [subsumes_term/2]).
:- use_module(library(terms_vars), [term_variables/2]).
:- use_module(library(cyclic_terms), [acyclic_term/1]).

:- use_module(library(format)).

:- if(defined(optim_comp)).
:- else.
:- use_module(library(compiler)). % TODO: refine, not all are ISO
:- endif.
:- use_module(library(system)). % TODO: refine, not all are ISO

% IO predicates
:- use_module(library(iso_incomplete)).
:- use_module(engine(stream_basic), [
    open/3,
    current_input/1,
    current_output/1,
    % TODO: ISO?
    character_count/2, line_count/2, line_position/2,
    flush_output/1, flush_output/0,
    current_stream/3
]).
:- use_module(engine(io_basic), [
    % TODO: refine, not all are ISO
    get_code/1,
    peek_code/1,
    put_code/1,
    nl/0,
    tab/1,
    get_byte/1,
    peek_byte/1,
    put_byte/1, 
    at_end_of_stream/0,
    display/1,
    displayq/1
]).
:- use_module(library(iso_char), [
    char_code/2, atom_chars/2, number_chars/2,char_codes/2,
    get_char/1,
    peek_char/1,
    put_char/1
]).
:- use_module(library(read), [
    read/1,
    read_term/2
]).
:- use_module(library(write), [
    write_term/2,
    write/1, writeq/1,
    write_canonical/1,
    print/1, printq/1,
    portray_clause/1,
    numbervars/3
]).

