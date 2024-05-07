:- package(pure).
:- use_package(initial).

:- use_module(engine(term_basic), [(=)/2]).
:- use_module(engine(basiccontrol), [
    ','/2,
    ';'/2,
    fail/0,
    true/0,
    false/0
]).
:- if(defined(optim_comp)).
:- else.
:- use_module(engine(debugger_support), [srcdbg_spy/7]). % TODO: internal, make it optional or hide
:- endif.

%:- set_prolog_flag(unknown, fail).
