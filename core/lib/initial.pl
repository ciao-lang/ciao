:- package(initial).
% Initial package (support for the basic language and other packages).

:- use_package(condcomp). % (builtin, only needed for some compilation facts)

:- if(defined('SHELL')).

% (Empty)

:- else. % not SHELL

:- if(defined(optim_comp)).
:- else.
:- use_module(engine(term_basic), [functor/3]). % TODO: make it optional too?
:- endif.

% Module initialization and on_abort hooks
:- if(defined(optim_comp)).
:- '$props'('$initialization'/1, [impnat=bytecode]).
:- multifile('$initialization'/1).
:- '$props'('$on_abort'/1, [impnat=bytecode]).
:- multifile('$on_abort'/1).
:- else.
:- multifile('$initialization'/1).
:- multifile('$on_abort'/1).
:- endif.

% Internal module metadata
:- if(defined(optim_comp)).
:- else.
:- multifile('$current_module'/1).
:- dynamic('$current_module'/1).
:- multifile('$ldlibs'/1).
:- dynamic('$multifile'/3). % TODO:[oc-merge] new!
:- multifile('$multifile'/3).
:- multifile('$load_libs'/0).
:- multifile('$meta_args'/2).
:- dynamic('$meta_args'/2).
:- multifile('$u'/2).
:- multifile('$imports'/5).
:- dynamic('$imports'/5).
:- multifile('$defines'/3).
:- endif.

% Definitions for ptoc and ptoc__analysis
:- if(defined(optim_comp)).
% TODO: include in every module? or include ciao/eng.h?
:- '$native_weak_inline'(include('ciao/basiccontrol.native.h')).
:- include(engine(ptoc__prelude_types)).
:- include(engine(ptoc__prelude)).
%:- '$pragma'(analyze_all).
%
:- '$pragma'(analyze_idet).
:- endif.

% TODO: make 'dynamic','multifile', and other directives optional,
%   once 'initial' is loaded

% Forbid declaration of the following predicates
:- if(defined(optim_comp)).
:- '$forbid_def'(fail/0).
:- '$forbid_def'(true/0).
:- '$forbid_def'(','/2).
:- '$forbid_def'((;)/2).
:- '$forbid_def'((->)/2).
:- '$forbid_def'((\+)/1).
:- '$forbid_def'(if/3).
:- '$forbid_def'((^)/2).
:- '$forbid_def'((=)/2).
:- else.
:- endif.

:- endif. % not SHELL
