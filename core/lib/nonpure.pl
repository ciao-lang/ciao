:- package(nonpure).
:- use_package(initial).

% Package implicitly used in Ciao modules defined with module/3
% (except if 'pure' package is used).

:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
%K :- use_module(engine(basic_props)). % TODO: only in assertions
:- use_module(engine(basiccontrol)).
:- use_module(engine(data_facts)).
:- use_module(engine(exceptions)).
%K :- use_module(engine(messages_basic)). % TODO: not by default
%K :- use_module(engine(io_basic)). % TODO: not by default
%K :- use_module(engine(prolog_flags)). % TODO: not by default
%K :- use_module(engine(stream_basic)). % TODO: not by default
%K :- use_module(engine(system_info)). % TODO: not by default
:- use_module(engine(term_basic)).
:- use_module(engine(term_compare)).
:- use_module(engine(term_typing)).
%K :- use_module(engine(hiord_rt), [call/1]). % TODO: not by default
:- use_module(engine(debugger_support), [srcdbg_spy/7]). % TODO: internal, make it optional or hide

