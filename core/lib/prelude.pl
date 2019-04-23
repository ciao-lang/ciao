:- package(prelude).
:- use_package(initial).

% Package implicitly used in Ciao modules defined with module/3
% (except if 'noprelude' or 'pure' package is used).
% 
% This package is documented in BasicLang.lpdoc

:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(basiccontrol)).
:- if(defined(optim_comp)).
:- use_module(engine(interpreter)). % TODO: make it optional?
:- endif.
:- use_module(engine(exceptions)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_compare)).
:- use_module(engine(term_typing)).
:- if(defined(optim_comp)).
:- else.
:- use_module(engine(debugger_support), [srcdbg_spy/7]). % TODO: internal, make it optional or hide
:- endif.

