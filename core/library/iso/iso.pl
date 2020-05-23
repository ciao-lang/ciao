:- package(iso).

:- use_package(dcg).
%:- use_package('dcg/dcg_phrase').
:- use_package(dynamic). % TODO: refine, not all are ISO (and includes datafacts)

%:- use_module(engine(hiord_rt), [call/1]).
:- use_package(hiord). % TODO: refine, not all are ISO (required for call/N)
:- use_module(engine(basic_props), [callable/1]).

:- use_package(runtime_ops).

:- use_module(library(aggregates)).
:- use_module(library(sort), [sort/2, keysort/2]).
:- use_module(library(iso_misc)).
:- use_module(library(iso_char)).
:- use_module(library(iso_incomplete)).
:- use_module(library(operators)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(terms_check), [subsumes_term/2]).
:- use_module(library(terms_vars), [term_variables/2]).
:- use_module(library(cyclic_terms), [acyclic_term/1]).

:- use_module(library(format)).

:- if(defined(optim_comp)).
:- else.
:- use_module(library(compiler)). % TODO: refine
:- endif.
:- use_module(library(system)). % TODO: refine

:- use_module(engine(stream_basic), [
    % TODO: refine, not all are ISO
    open/3,
    %open/4,
    open_option_list/1,
    %close/1,
    set_input/1, current_input/1,
    set_output/1, current_output/1,
    character_count/2, line_count/2, line_position/2,
    flush_output/1, flush_output/0,
    %clearerr/1,
    current_stream/3, stream_code/2,
    %absolute_file_name/2,
    absolute_file_name/7,
    pipe/2,
    sourcename/1, stream/1, stream_alias/1
    %io_mode/1, 
    %atm_or_int/1
]).
:- use_module(engine(io_basic)). % TODO: refine, not all are ISO
:- use_module(engine(runtime_control)). % TODO: refine, not all are ISO
