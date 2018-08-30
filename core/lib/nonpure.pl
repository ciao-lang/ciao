:- package(nonpure).
:- use_package(initial).

% Package implicitly used in Ciao modules defined with module/3
% (except if 'pure' package is used).

:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(basiccontrol)).
:- use_module(engine(exceptions)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_compare)).
:- use_module(engine(term_typing)).
:- use_module(engine(debugger_support), [srcdbg_spy/7]). % TODO: internal, make it optional or hide

