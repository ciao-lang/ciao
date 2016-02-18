:- module(difference_constraints_tab,
        [
	    lookup_attr_call/5,
	    lookup_attr_answer/4,
	    reinstall_gen_space/2,
	    consume_attr_answer/2
	], 
	[assertions,basicmodes,regtypes,foreign_interface]).

 %% :- use_module(library(tabling/tabling_rt), []).
 %% :- use_foreign_library('Win32i686', [tabling_rt_Win32i686]).

:- use_package(assertions).

:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(author,"Pablo Chico de Guzm@'{a}n Huerta").
:- doc(author,"The CLIP Group").

:- doc(title,"Difference Constraints with Tabling Support").

:- doc(module, "This module supports difference constraint
under tabling evaluation.").

:- doc(bug, "This library is a beta version. The constraint store is a
$N\timesN$ matrix.").

:- use_module(library(difference_constraints/difference_constraints_rt), []).
:- use_foreign_library('Win32i686', [difference_constraints_rt_Win32i686]). % TODO: why?

:- extra_compiler_opts('-I..').
:- extra_linker_opts('-L..').

:- true pred lookup_attr_call(+Root,+SF,-Node,-CallSpace,-LNodePrune) :: 
	int * int * int * int *int + foreign_low(lookup_attr_call_c) 
 # "Looks up a constrain tabled call.".

:- true pred lookup_attr_answer(+Root,+Attrs,-Space,-LPruneAns) :: 
	int * int * int * int + foreign_low(lookup_attr_answer_c) 
 # "Looks up a constrain answer.".

:- true pred reinstall_gen_space(+SF,+CallSpace) :: 
	int * int + foreign_low(reinstall_gen_space_c) 
 # "Looks up a constrain tabled call.".

:- true pred consume_attr_answer(+AnsSpace,+AttrsVars) :: 
	int * int + foreign_low(consume_attr_answer_c) 
 # "Looks up a constrain tabled call.".

 %% :- extra_compiler_opts(['-DDEBUG_ALL']).

:- use_foreign_source(['difference_constraints_tab.c']).
