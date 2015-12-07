:- module(compiler, [make_po/1, make_wam/1, ensure_loaded/1, ensure_loaded/2,
                     use_module/1, use_module/2, use_module/3, unload/1,
                     set_debug_mode/1, set_nodebug_mode/1,
                     set_debug_module/1, set_nodebug_module/1,
		     set_debug_module_source/1,
                     mode_of_module/2, module_of/2
                     ], [assertions]).

:- use_module(library(system), [cd/1, working_directory/2]).
:- use_module(library(compiler/c_itf_internal)).

:- use_module(exemaker, [create_interfaces/0]).
make_po(Files) :-
	make_object(Files, po),
	create_interfaces.

make_wam(Files) :-
	make_object(Files, wam).

make_object([], _) :-
	!.
make_object([File|Files], Type) :-
	!,
	make_object_prot(Type, File),
	make_object(Files, Type).
make_object(File, Type) :-
	make_object_prot(Type, File).
	
% make_object_prot: make_object1/2, preserves working directory on error
make_object_prot(Type, File) :-
	working_directory(W,W),
	catch(make_object1(Type, File), Error, (cd(W), handle_exc(Error))).

:- meta_predicate use_module(addmodule).

use_module(Mod,This) :- use_module(Mod,all,This).

:- meta_predicate use_module(+,addmodule).

use_module(File, Imports, ByThisModule) :-
        cleanup_c_itf_data,
        use_mod(File, Imports, ByThisModule),
        check_static_module(File).

:- meta_predicate ensure_loaded(addmodule).
%JF[] ensure_loaded(File) :-
ensure_loaded(File, ByThisModule) :-
        cleanup_c_itf_data,
%JF[]	use_mod_user(File),
	use_mod_user(File, ByThisModule),
        check_static_module(File).

check_static_module(File) :-
        base_name(File, Base),
        defines_module(Base, Module),
        static_module(Module), !,
        message(note, ['module ',Module,
                       ' already in executable, just made visible']).
check_static_module(_).

unload(File) :-
	unload_mod(File).

set_debug_mode(File) :-
        absolute_file_name(File, Source),
        (interpret_file(Source), ! ; assertz_fact(interpret_file(Source))).

set_nodebug_mode(File) :-
        absolute_file_name(File, Source),
        retractall_fact(interpret_file(Source)).

set_debug_module(Mod) :- 
        module_pattern(Mod, MPat),
	retractall_fact(interpret_srcdbg(MPat)),
        (   current_fact(interpret_module(MPat)), ! 
	; 
	    assertz_fact(interpret_module(MPat))
	).

set_nodebug_module(Mod) :-
        module_pattern(Mod, MPat),
        retract_fact(interpret_module(MPat)),
	retractall_fact(interpret_srcdbg(MPat)).

set_debug_module_source(Mod) :- 
        module_pattern(Mod, MPat),
	assertz_fact(interpret_module(MPat)),
        (   
	    current_fact(interpret_srcdbg(MPat)), ! 
	; 
	    assertz_fact(interpret_srcdbg(MPat))
	).

module_pattern(user, user(_)) :- !.
module_pattern(Module, Module) :- atom(Module).

module_of(H, M) :- pred_module(H, M).

mode_of_module(Module, Mode) :- module_loaded(Module, _, _, Mode).
