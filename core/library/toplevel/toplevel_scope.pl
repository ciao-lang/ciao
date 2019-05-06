:- use_package([]).

:- use_module(library(toplevel)).

:- if(defined(optim_comp)).
% TODO: enable the rest when possible
:- use_module(compiler(dynload), [unload/1]).
:- else.
:- use_module(library(compiler), 
        [make_po/1, unload/1,
         set_debug_mode/1, set_nodebug_mode/1]).
:- use_module(library(compiler/exemaker), 
        [force_lazy/1, undo_force_lazy/1,
         dynamic_search_path/1]).
:- endif.

:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), [multifile/1]).
:- else.
:- use_module(library(compiler/c_itf), [multifile/1]).
:- endif.

:- use_module(library(dynamic/dynamic_rt), [assertz/1]). % needed by use_package

:- use_module(library(debugger), 
        [trace/0, notrace/0, debug/0, nodebug/0, spy/1, nospy/1,
 	nospyall/0, debugging/0, leash/1, maxdepth/1, 
	breakpt/6,nobreakpt/6,nobreakall/0,list_breakpt/0, 
	call_in_module/2]).
:- use_module(library(operators), [op/3]).

:- use_module(engine(hiord_rt), [call/1, this_module/1]).
:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), [rt_pgcall/2, rt_modexp/4, rt_exp/6]). % TODO: put a $ in the name
:- endif.

'$shell_module'(ThisModule) :- this_module(ThisModule).

:- if(defined(optim_comp)).
:- use_module(engine(debugger_support), ['$start_trace'/0, '$stop_trace'/0]).
:- '$pragma'(allow_runtime_expansions).
'$shell_call'(X) :- '$start_trace', call(X), '$stop_trace'.
:- else.
'$shell_call'(X) :- call(X).
:- endif.

aborting :- '$shell_abort'.
