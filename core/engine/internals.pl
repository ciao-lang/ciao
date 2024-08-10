:- module(internals, [], [noprelude, assertions, basicmodes, nortchecks, regtypes, datafacts]).

:- doc(title, "Internal runtime predicates").  

:- doc(module, "This module defines internal predicates necessary to
   boot and prepare the system for the execution of the user
   @tt{main/{0,1}} predicate, as well as support predicates required
   for other system libraries. Some of the predicates here are just
   declarations with the actual code being defined in C.

@begin{alert}
Users should not use the code in this module.
@end{alert}
   ").

:- use_module(engine(basiccontrol)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(arithmetic)).
:- use_module(engine(exceptions)).
:- use_module(engine(term_compare)).
% TODO:[oc-merge] needed?
:- if(defined(optim_comp)).
:- use_module(engine(attributes)).
:- use_module(engine(io_basic)).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "(common C code)").

:- if(defined(optim_comp)).
:- '$native_include_c_header'(engine(ciao_prolog)). % TODO: backport noalias for this case
:- '$native_include_c_header'(engine(ciao_gluecode)).
:- '$native_include_c_header'(engine(os_signal)).
:- '$native_include_c_header'(engine(os_threads)).
:- '$native_include_c_header'(engine(own_mmap)).
:- '$native_include_c_header'(engine(rune)).
:- '$native_include_c_header'(engine(unicode_tbl)).
:- endif.

% core definitions
:- if(defined(optim_comp)).
:- '$native_include_c_source'(engine(eng_alloc)).
:- '$native_include_c_source'(engine(eng_bignum)).
:- '$native_include_c_source'(engine(ciao_prolog)).
:- '$native_include_c_source'(engine(eng_debug)).
:- '$native_include_c_source'(engine(eng_interrupt)).
:- '$native_include_c_source'(engine(own_mmap)).
:- '$native_include_c_source'(engine(own_malloc)).
:- '$native_include_c_source'(engine(eng_profile)).
:- '$native_include_c_source'(engine(eng_threads)).
:- '$native_include_c_source'(engine(eng_start)).
:- '$native_include_c_source'(engine(eng_main)).
:- '$native_include_c_source'(engine(rune)).
:- '$native_include_c_source'(engine(dtoa_ryu)).
:- '$native_include_c_source'(engine(eng_registry)).
:- '$native_include_c_source'(engine(eng_gc)).
:- endif.

% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- '$native_include_c_header'(engine(incore_rt)).
:- endif.

:- if(defined(optim_comp)).
%:- '$native_include_c_header'(engine(dynamic_rt)).
:- '$native_include_c_source'(engine(dynamic_rt)).
:- endif.

:- if(defined(optim_comp)).
:- '$native_include_c_source'(.(internals)).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Engine version").

:- export('$bootversion'/0).
:- if(defined(optim_comp)).
:- '$props'('$bootversion'/0, [impnat=cbool(prolog_print_emulator_version)]).
:- else.
:- impl_defined('$bootversion'/0).
:- endif.

:- if(defined(optim_comp)).
:- export('$ciao_version'/1).
:- '$props'('$ciao_version'/1, [impnat=cbool(prolog_version)]).
:- else.
:- export('$ciao_version'/7).
:- trust pred '$ciao_version'(Major, Minor, Patch,
                              CommitBranch, CommitId, CommitDate, CommitDesc) =>
    (int(Major), int(Minor), int(Patch),
     atm(CommitBranch), atm(CommitId), atm(CommitDate), atm(CommitDesc)).
:- impl_defined('$ciao_version'/7).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Internal runtime flags").

% TODO:[JF] we need assertion shorthands for these simple cases but
%   very verbose cases.

:- export('$ferror_flag'/2).
:- if(defined(optim_comp)).
:- '$props'('$ferror_flag'/2, [impnat=cbool(ferror_flag)]).
:- else.
:- trust pred '$ferror_flag'(Old,+New) : on_off(New) => on_off(Old).
:- trust pred '$ferror_flag'(-Old,-New) : (Old == New) => (on_off(Old), on_off(New)). 
:- impl_defined('$ferror_flag'/2).

:- regtype on_off/1.
on_off(on).
on_off(off).
:- endif.

:- export('$quiet_flag'/2).
:- if(defined(optim_comp)).
:- '$props'('$quiet_flag'/2, [impnat=cbool(quiet_flag)]).
:- else.
:- trust pred '$quiet_flag'(Old,+New) : quiet_mode(New) => quiet_mode(Old).
:- trust pred '$quiet_flag'(-Old,-New) : (Old == New) => (quiet_mode(Old), quiet_mode(New)). 
:- impl_defined('$quiet_flag'/2).

:- regtype quiet_mode/1.
quiet_mode(on).
quiet_mode(error).
quiet_mode(warning).
quiet_mode(debug).
quiet_mode(off).
:- endif.

% :- export('$prolog_radix'/2).
% :- if(defined(optim_comp)).
% :- '$props'('$prolog_radix'/2, [impnat=cbool(prolog_radix)]).
% :- else.
% :- impl_defined('$prolog_radix'/2).
% :- endif.

:- export('$atom_mode'/2). % (for write.pl)
:- if(defined(optim_comp)).
:- '$props'('$atom_mode'/2, [impnat=cbool(prolog_atom_mode)]).
:- else.
:- trust pred '$atom_mode'(Atom, Context) : atm(Atom) => int(Context).
:- impl_defined('$atom_mode'/2). 
:- endif.

:- export('$unknown'/2). % (for runtime mexpand)
:- if(defined(optim_comp)).
:- '$props'('$unknown'/2, [impnat=cbool(unknown)]).
:- else.
:- trust pred '$unknown'(Old,+New) : unknown_level(New) => unknown_level(Old).
:- trust pred '$unknown'(-Old,-New) : (Old == New) => (unknown_level(Old), unknown_level(New)). 
:- impl_defined('$unknown'/2).

:- regtype unknown_level/1.
unknown_level(error).
unknown_level(fail).
unknown_level(warning).
:- endif.

:- export('$gc_mode'/2).
:- if(defined(optim_comp)).
:- '$props'('$gc_mode'/2, [impnat=cbool(gc_mode)]).
:- else.
:- trust pred '$gc_mode'(Old,+New) : on_off(New) => on_off(Old).
:- trust pred '$gc_mode'(-Old,-New) : (Old == New) => (on_off(Old), on_off(New)). 
:- impl_defined('$gc_mode'/2).
:- endif.

:- export('$gc_trace'/2).
:- if(defined(optim_comp)).
:- '$props'('$gc_trace'/2, [impnat=cbool(gc_trace)]).
:- else.
:- trust pred '$gc_trace'(Old,+New) : gc_trace_modes(New) => gc_trace_modes(Old).
:- trust pred '$gc_trace'(-Old,-New) : (Old == New) => (gc_trace_modes(Old), gc_trace_modes(New)). 
:- impl_defined('$gc_trace'/2).

:- regtype gc_trace_modes/1.
gc_trace_modes(on).
gc_trace_modes(off).
gc_trace_modes(terse).
gc_trace_modes(verbose).
:- endif.

:- export('$gc_margin'/2).
:- if(defined(optim_comp)).
:- '$props'('$gc_margin'/2, [impnat=cbool(gc_margin)]).
:- else.
:- trust pred '$gc_margin'(Old,+New) : int(New) => int(Old).
:- trust pred '$gc_margin'(-Old,-New) : (Old == New) => (int(Old), int(New)). 
:- impl_defined('$gc_margin'/2).
:- endif.

:- export('$gc_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$gc_usage'/1, [impnat=cbool(gc_usage)]).
:- else.
:- trust pred '$gc_usage'(Usage) => gc_list3(Usage).
:- impl_defined('$gc_usage'/1).

:- regtype gc_list3/1.
gc_list3([F1,I2,I3]) :- flt(F1), int(I2), int(I3).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export('$compiling'/2). % TODO: remove
:- trust pred '$compiling'(Old,+New) : compiling_mode(New) => compiling_mode(Old).
:- trust pred '$compiling'(-Old,-New) : (Old == New) => (compiling_mode(Old), compiling_mode(New)). 
:- impl_defined('$compiling'/2).

:- regtype compiling_mode/1.
compiling_mode(profiled).   %jcf% Not used
compiling_mode(unprofiled).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Internal Debugging").

:- if(defined(optim_comp)).
:- else.
:- export('$show_nodes'/2).
:- impl_defined('$show_nodes'/2).

:- export('$show_all_nodes'/0).
:- impl_defined('$show_all_nodes'/0).

:- export('$start_node'/1).
:- impl_defined('$start_node'/1).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Profiler").

:- export('$ddt'/1).
:- if(defined(optim_comp)).
:- '$props'('$ddt'/1, [impnat=cbool(set_predtrace)]).
:- else.
:- trust pred '$ddt'(T) : int(T). %jcf% Check that int is the right type!. Not used in Prolog code.
:- impl_defined('$ddt'/1).
:- endif.

:- if(defined(optim_comp)).
% TODO:[oc-merge] port to OC
:- else.
:- export('$profile_flags_set'/1).
:- trust pred '$profile_flags_set'(T) : int(T).
:- impl_defined('$profile_flags_set'/1).

:- export('$profile_flags_get'/1).
:- trust pred '$profile_flags_get'(T) => int(T).
:- impl_defined('$profile_flags_get'/1).

:- export('$profile_dump'/0).
:- impl_defined('$profile_dump'/0).

:- export('$profile_reset'/0).
:- impl_defined('$profile_reset'/0).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Support for dynamic_rt.pl").

:- if(defined(optim_comp)).
% Force dependency % TODO: polish, some executables may not require them!
:- use_module(library(concurrency)).
:- endif.

:- export('$purge'/1). %jcf% Not used in Prolog code.
:- if(defined(optim_comp)).
:- '$props'('$purge'/1, [impnat=cbool(prolog_purge)]).
:- else.
:- trust pred '$purge'/1.
:- impl_defined('$purge'/1).
:- endif.

:- export('$current_clauses'/2).
:- if(defined(optim_comp)).
:- '$props'('$current_clauses'/2, [impnat=cbool(current_clauses)]).
:- else.
:- trust pred '$current_clauses'(Head,Root) : cgoal(Head) => int(Root).
:- impl_defined('$current_clauses'/2).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- regtype ref/1.
ref('$ref'(A,B)):- int(A), int(B).
:- regtype blocking_mode/1.
blocking_mode(block).
blocking_mode(no_block).
:- regtype list_clause/1.
list_clause([Head|Body]):- cgoal(Head), body(Body).
:- regtype body/1.
body(X) :- cgoal(X).
body((X,Xs)) :- cgoal(X), body(Xs).
:- endif.

:- if(defined(optim_comp)).
% (new version)
:- export('$asserta_root'/3).
:- '$props'('$asserta_root'/3, [impnat=cbool(prolog_asserta_root)]).
:- export('$asserta'/2).
:- '$props'('$asserta'/2, [impnat=cbool(prolog_asserta)]).
:- export('$asserta_ref'/3).
:- '$props'('$asserta_ref'/3, [impnat=cbool(prolog_asserta_ref)]).
:- export('$assertz_root'/3).
:- '$props'('$assertz_root'/3, [impnat=cbool(prolog_assertz_root)]).
:- export('$assertz'/2).
:- '$props'('$assertz'/2, [impnat=cbool(prolog_assertz)]).
:- export('$assertz_ref'/3).
:- '$props'('$assertz_ref'/3, [impnat=cbool(prolog_assertz_ref)]).
%
:- export('$erase'/2).
:- '$props'('$erase'/2, [impnat=cinsnp(prolog_erase)]).
:- export('$erase_nb_root'/3).
:- '$props'('$erase_nb_root'/3, [impnat=cinsnp(prolog_erase_nb_root)]).
:- export('$erase_nb'/2).
:- '$props'('$erase_nb'/2, [impnat=cinsnp(prolog_erase_nb)]).
:- export('$erase_ref'/1).
:- '$props'('$erase_ref'/1, [impnat=cbool(prolog_erase_ref)]).
%
:- export('$current'/2).
:- '$props'('$current'/2, [impnat=cinsnp(prolog_current)]).
:- export('$current_nb_root'/3).
:- '$props'('$current_nb_root'/3, [impnat=cinsnp(prolog_current_nb_root)]).
:- export('$current_nb'/2).
:- '$props'('$current_nb'/2, [impnat=cinsnp(prolog_current_nb)]).
:- export('$current_nb_ref'/3).
:- '$props'('$current_nb_ref'/3, [impnat=cinsnp(prolog_current_nb_ref)]).
:- else. % (not optim_comp)
% (old version)
:- export('$erase'/1).
:- trust pred '$erase'(+Ptr) : int(Ptr).
:- impl_defined('$erase'/1).
%
:- export('$ptr_ref'/2).
:- trust pred '$ptr_ref'(Ptr,Ref) : int(Ptr) => ref(Ref).
:- trust pred '$ptr_ref'(Ptr,Ref) : ref(Ref) => int(Ptr).
:- impl_defined('$ptr_ref'/2).
%
:- export('$inserta'/2).
:- trust pred '$inserta'(Root,Ptr) : (int(Root), int(Ptr)).
:- impl_defined('$inserta'/2).
%
:- export('$insertz'/2).
:- trust pred '$insertz'(Root,Ptr) : (int(Root), int(Ptr)).
:- impl_defined('$insertz'/2).
%
:- export('$make_bytecode_object'/4).
:- trust pred '$make_bytecode_object'(Size,Counters,Tokens,Obj) 
    : (int(Size), int(Counters), list(Tokens)) => int(Obj).
:- impl_defined('$make_bytecode_object'/4).
%
:- export('$compile_term'/2).
:- trust pred '$compile_term'(LClause,Ptr) : list_clause(LClause) => int(Ptr).
:- impl_defined('$compile_term'/2).
%
:- export('$instance'/3).
:- trust pred '$instance'(Head,Body,Ptr) : int(Ptr) => (cgoal(Head), body(Body)).
:- impl_defined('$instance'/3).
%
:- export('$current_instance'/5).
:- trust pred '$current_instance'(Head, Body, Root, Ptr, Blocking) 
    : (cgoal(Head), body(Body), int(Root), blocking_mode(Blocking)) => int(Ptr).
:- impl_defined('$current_instance'/5).
%
:- export('$unlock_predicate'/1).
:- trust pred '$unlock_predicate'(Root) : int(Root).
:- impl_defined('$unlock_predicate'/1).
:- endif.

:- export('$first_instance'/2). % (only used in listing.pl)
:- if(defined(optim_comp)).
:- '$props'('$first_instance'/2, [impnat=cbool(first_instance)]).
:- else.
:- trust pred '$first_instance'(Root,Ptr) : int(Root) => int(Ptr).
:- impl_defined('$first_instance'/2).
:- endif.

:- if(defined(optim_comp)).
:- export('$open_pred'/1).
:- '$props'('$open_pred'/1, [impnat=cbool(prolog_open_pred)]).
:- export('$close_pred'/1).
:- '$props'('$close_pred'/1, [impnat=cbool(prolog_close_pred)]).
:- else.
:- export('$open_predicate'/1).
:- trust pred '$open_predicate'(Root) : int(Root).
:- impl_defined('$open_predicate'/1).
:- export('$close_predicate'/1).
:- trust pred '$close_predicate'(Root) : int(Root).
:- impl_defined('$close_predicate'/1).
:- endif.

:- export('$abolish'/1).
:- if(defined(optim_comp)).
:- '$props'('$abolish'/1, [impnat=cbool(prolog_abolish)]).
:- else.
:- trust pred '$abolish'(Head) : cgoal(Head).
:- impl_defined('$abolish'/1).
:- endif.

:- export('$erase_clause'/1).
:- if(defined(optim_comp)).
:- '$props'('$erase_clause'/1, [impnat=cbool(erase_clause)]).
:- else.
:- trust pred '$erase_clause'/1. %jcf% Not used in Prolog code.
:- impl_defined('$erase_clause'/1).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export('$clause_number'/2).
:- trust pred '$clause_number'(Pred,Number). %jcf% Not used in Prolog code.
:- impl_defined('$clause_number'/2).
%
:- export('$compiled_clause'/4).
:- trust pred '$compiled_clause'(Pred,Obj,Mode,Data) 
    : (predname(Pred), int(Obj), pred_mode(Mode)). %jcf% No info about Data.
:- impl_defined('$compiled_clause'/4).
:- endif.

:- export('$empty_gcdef_bin'/0).
:- if(defined(optim_comp)).
:- '$props'('$empty_gcdef_bin'/0, [impnat=cbool(empty_gcdef_bin)]).
:- else.
:- impl_defined('$empty_gcdef_bin'/0).
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export('$interpreted_clause'/2).
:- trust pred '$interpreted_clause'(Pred,LClause) 
    : (predname(Pred), list_clause(LClause)). %jcf% Check that these are the right types!
:- impl_defined('$interpreted_clause'/2).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "QL bytecode loader").
% (not in optim-comp)

:- if(defined(optim_comp)).
% Force dependency % TODO: polish, some executables may not require them!
:- use_module(engine(ql_inout)).
:- else.
:- export('$qread'/2).
:- trust pred '$qread'(Stream, Term) : stream(Stream) => int(Term). %jcf%Check that int is the right type!.
:- impl_defined('$qread'/2).

:- export('$push_qlinfo'/0).
:- impl_defined('$push_qlinfo'/0).

:- export('$pop_qlinfo'/0).
:- impl_defined('$pop_qlinfo'/0).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Attributed variables").

:- include('.'(attributed_variables)).

% ---------------------------------------------------------------------------
:- doc(section, "Internal for setarg").

% TODO: move to terms_basic?
:- export('$setarg'/4).
:- if(defined(optim_comp)).
:- '$props'('$setarg'/4, [impnat=cbool(setarg)]).
:- else.
:- trust pred '$setarg'(I, +Term, +Newarg, Mode) : (int(I), setarg_mode(Mode)).
:- impl_defined('$setarg'/4).

:- regtype setarg_mode/1.
setarg_mode(on).
setarg_mode(off).
setarg_mode(true).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Internal for control").

% TODO: move to basiccontrol?

:- export('$undo_goal'/1).
:- if(defined(optim_comp)).
:- '$props'('$undo_goal'/1, [impnat=cbool(undo)]).
:- else.
:- trust pred '$undo_goal'(Goal) : cgoal(Goal).
:- impl_defined('$undo_goal'/1). 
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export('$yield'/0). % (see ciao_query_resume())
:- impl_defined('$yield'/0). 
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export('$exit'/1).
:- trust pred '$exit'(A) => int(A).
:- impl_defined('$exit'/1).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Atom GC"). % (incomplete)

:- if(defined(optim_comp)).
% :- export('$erase_atom'/1).
% :- '$props'('$erase_atom'/1, [impnat=cbool(prolog_erase_atom)]).
:- else.
:- export('$erase_atom'/1).
:- trust pred '$erase_atom'/1. %jcf% Not used in Prolog code.
:- impl_defined('$erase_atom'/1).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "(Outdated/unused)").

:- export('$frozen'/2).
:- if(defined(optim_comp)).
:- '$props'('$frozen'/2, [impnat=cbool(frozen)]).
:- else.
:- trust pred '$frozen'/2. %jcf% Not used in Prolog code.
:- impl_defined('$frozen'/2).
:- endif.

:- export('$defrost'/2).
:- if(defined(optim_comp)).
:- '$props'('$defrost'/2, [impnat=cbool(defrost)]).
:- else.
:- trust pred '$defrost'/2. %jcf% Not used in Prolog code.
:- impl_defined('$defrost'/2).
:- endif.

:- export('$constraint_list'/2).
:- if(defined(optim_comp)).
:- '$props'('$constraint_list'/2, [impnat=cbool(constraint_list)]).
:- else.
:- trust pred '$constraint_list'/2. %jcf% Not used in Prolog code.
:- impl_defined('$constraint_list'/2).
:- endif.

:- export('$eq'/2).
:- if(defined(optim_comp)).
:- '$props'('$eq'/2, [impnat=cbool(prolog_eq)]).
:- else.
:- trust pred '$eq'/2. %jcf% Not used in Prolog code.
:- impl_defined('$eq'/2).
:- endif.

% % (for gauge.pl profiler)
% :- export('$emulated_clause_counters'/4).
% :- trust pred '$emulated_clause_counters'/4.
% :- impl_defined('$emulated_clause_counters'/4).
% 
% % (for gauge.pl profiler)
% :- export('$counter_values'/3).
% :- trust pred '$counter_values'/3.
% :- impl_defined('$counter_values'/3).
% 
% % (for gauge.pl profiler)
% :- export('$reset_counters'/2).
% :- trust pred '$reset_counters'/2. %jcf% Not used in Prolog code.
% :- impl_defined('$reset_counters'/2).

%:- export('$blob_data'/3).
%:- '$props'('$blob_data'/3, [impnat=cbool(blob_data)]).

% ---------------------------------------------------------------------------
:- doc(section, "Internal for statistics").

:- if(defined(optim_comp)).
:- else.
:- regtype flt_list2/1.
flt_list2([F1,F2]) :- flt(F1), flt(F2).
:- regtype int_list2/1.
int_list2([I1,I2]) :- int(I1), int(I2).
:- regtype int_list3/1.
int_list3([I1,I2,I3]) :- int(I1), int(I2), int(I3).
:- endif.

:- if(defined(optim_comp)).
% TODO:[oc-merge] defined somewhere else
:- else.
:- export('$runtime'/1).
:- trust pred '$runtime'(Time) => flt_list2(Time).
:- impl_defined('$runtime'/1).
:- export('$runtick'/1).
:- trust pred '$runtick'(Tick) => flt_list2(Tick).
:- impl_defined('$runtick'/1).
:- export('$runclockfreq'/1).
:- trust pred '$runclockfreq'(Freq) => flt(Freq).
:- impl_defined('$runclockfreq'/1).
:- export('$usertime'/1).
:- trust pred '$usertime'(Time) => flt_list2(Time).
:- impl_defined('$usertime'/1).
:- export('$usertick'/1).
:- trust pred '$usertick'(Tick) => flt_list2(Tick).
:- impl_defined('$usertick'/1).
:- export('$userclockfreq'/1).
:- trust pred '$userclockfreq'(Freq) => flt(Freq).
:- impl_defined('$userclockfreq'/1).
:- export('$systemtime'/1).
:- trust pred '$systemtime'(Time) => flt_list2(Time).
:- impl_defined('$systemtime'/1).
:- export('$systemtick'/1).
:- trust pred '$systemtick'(Tick) => flt_list2(Tick).
:- impl_defined('$systemtick'/1).
:- export('$systemclockfreq'/1).
:- trust pred '$systemclockfreq'(Freq) => flt(Freq).
:- impl_defined('$systemclockfreq'/1).
:- export('$walltime'/1).
:- trust pred '$walltime'(Time) => flt_list2(Time).
:- impl_defined('$walltime'/1).
:- export('$walltick'/1).
:- trust pred '$walltick'(Tick) => flt_list2(Tick).
:- impl_defined('$walltick'/1).
:- export('$wallclockfreq'/1).
:- trust pred '$wallclockfreq'(Freq) => flt(Freq).
:- impl_defined('$wallclockfreq'/1).
:- endif.

:- export('$termheap_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$termheap_usage'/1, [impnat=cbool(termheap_usage)]).
:- else.
:- trust pred '$termheap_usage'(Usage) => int_list2(Usage).
:- impl_defined('$termheap_usage'/1).
:- endif.
:- export('$envstack_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$envstack_usage'/1, [impnat=cbool(envstack_usage)]).
:- else.
:- trust pred '$envstack_usage'(Usage) => int_list2(Usage).
:- impl_defined('$envstack_usage'/1).
:- endif.
:- export('$trail_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$trail_usage'/1, [impnat=cbool(trail_usage)]).
:- else.
:- trust pred '$trail_usage'(Usage) => int_list2(Usage).
:- impl_defined('$trail_usage'/1).
:- endif.
:- export('$choice_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$choice_usage'/1, [impnat=cbool(choice_usage)]).
:- else.
:- trust pred '$choice_usage'(Usage) => int_list2(Usage).
:- impl_defined('$choice_usage'/1).
:- endif.
:- export('$stack_shift_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$stack_shift_usage'/1, [impnat=cbool(stack_shift_usage)]).
:- else.
:- trust pred '$stack_shift_usage'(Usage) => int_list3(Usage).
:- impl_defined('$stack_shift_usage'/1).
:- endif.

:- export('$program_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$program_usage'/1, [impnat=cbool(program_usage)]).
:- else.
:- trust pred '$program_usage'(Usage) => int_list2(Usage).
:- impl_defined('$program_usage'/1).
:- endif.
:- export('$internal_symbol_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$internal_symbol_usage'/1, [impnat=cbool(internal_symbol_usage)]).
:- else.
:- trust pred '$internal_symbol_usage'(Usage) => int_list2(Usage).
:- impl_defined('$internal_symbol_usage'/1).
:- endif.
:- export('$total_usage'/1).
:- if(defined(optim_comp)).
:- '$props'('$total_usage'/1, [impnat=cbool(total_usage)]).
:- else.
:- trust pred '$total_usage'(Usage) => int_list2(Usage).
:- impl_defined('$total_usage'/1).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Low level global variables").

:- include('.'(global_variables)).

% ---------------------------------------------------------------------------
:- doc(section, "Internal builtin errors").

% NOTE: error/5 is called from basiccontrol.c:wam, used to decode the
%   C exception and throw an error
% TODO: too complicated? partially recode in C?

:- if(defined(optim_comp)).
:- export(error/5). % TODO:[oc-merge] internal entry, not an export
:- else.
:- entry error/5.
:- endif.

error(Type, _, _, _, Error_Term) :-
    in_range(foreign, Type, _), !,
    throw(Error_Term).
error(Type, PredName, PredArity, Arg, Culprit) :-
%        display('In Error'(Type, Culprit)), nl,
    error_term(Type, Culprit, Error_Term),
%        display(error_term_is(Error_Term)), nl,
    where_term(PredName, PredArity, Arg, Where_Error),
    throw(error(Error_Term, Where_Error)).

in_range(Type, Code, WhichWithinType):-
    range_per_error(Range),
    error_start(Type, Section),
    Start is Section * Range,
    Code >= Start,
    Code < Start + Range,
    WhichWithinType is Code - Start.

error_term(1, _, instantiation_error) :- !.
error_term(2, Culprit, uninstantiation_error(Culprit)) :- !.
error_term(Code, _, system_error) :- in_range(system, Code, _), !.
error_term(Code, _, syntax_error) :- in_range(syntax, Code, _), !.
error_term(N, _, resource_error(Res)) :- 
    in_range(res, N, Code), !, 
    resource_code(Code, Res).
error_term(Code, _, user_error) :- in_range(user,   Code, _), !.
error_term(N, _Culprit, evaluation_error(Type)) :-
    in_range(eval, N, Code), !,
    evaluation_code(Code, Type).
error_term(N, _Culprit, representation_error(Type)) :-
    in_range(repres, N, Code), !,
    representation_code(Code, Type).
error_term(N, Culprit, type_error(Type, Culprit)) :-
    in_range(type, N, Code), !,
    type_code(Code, Type).
error_term(N, Culprit, domain_error(Type, Culprit)) :-
    in_range(dom, N, Code), !,
    domain_code(Code, Type).
error_term(N, Culprit, existence_error(Type, Culprit)) :-
    in_range(exist, N, Code), !,
    existence_code(Code, Type).
error_term(N, Culprit, permission_error(Permission, Object, Culprit)) :-
    in_range(perm, N, Code), !,
    get_obj_perm(Code,Obj,Per),
    permission_type_code(Per, Permission),
    permission_object_code(Obj, Object).

%% Check error type and return get Code for every class of error.  This should
%% be made more modularly (i.e., with an C interface - but is it worth?)

 %% is_evaluation_error(N,Code) :-     N>120, N<126, Code is N-121.
 %% 
 %% is_representation_error(N,Code) :- N>114, N<121, Code is N-115.
 %% 
 %% is_type_error(N,Code) :-           N>1, N<15, Code is N-2.
 %% 
 %% is_domain_error(N,Code) :-         N>14, N<32, Code is N-15.
 %% 
 %% is_existence_error(N,Code) :-      N>31, N<35, Code is N-32.
 %% 
 %% is_permission_error(N,Code) :-     N>34, N<115, Code is N-35.

get_obj_perm(Code, Obj, Perm) :-
    Obj is Code mod 10,
    Perm is Code // 10.

 %% culprit_stream([], S) :- !, current_input(S).
 %% culprit_stream(S,S).

% NOTE: Keep in sync with defs in ciao/eng.h

range_per_error(100).

error_start(inst,   0).
error_start(type,   1).
error_start(dom,    2).
error_start(exist,  3).
error_start(perm,   4).
error_start(repres, 5).
error_start(eval,   6).
error_start(res,    7).
error_start(syntax, 8).
error_start(system, 9).
error_start(foreign, 10).
error_start(user,   11).

type_code(0, atom).
type_code(1, atomic).
type_code(2, byte).
type_code(3, character).
type_code(4, compound).
type_code(5, evaluable).
type_code(6, in_byte).
type_code(7, integer).
type_code(8, list).
type_code(9, number).
type_code(10, predicate_indicator).
% type_code(11, variable).
type_code(12, callable).

domain_code(0, character_code_list).
domain_code(1, source_sink).
domain_code(2, stream).
domain_code(3, io_mode).
domain_code(4, non_empty_list).
domain_code(5, not_less_than_zero).
domain_code(6, operator_priority).
domain_code(7, prolog_flag).
domain_code(8, read_option).
domain_code(9, flag_value).
domain_code(10, close_option).
domain_code(11, stream_option).
domain_code(12, stream_or_alias).
domain_code(13, stream_position).
domain_code(14, stream_property).
domain_code(15, write_option).
domain_code(16, operator_specifier).

existence_code(0, procedure).
existence_code(1, source_sink).
existence_code(2, stream).

permission_type_code(0, access).
permission_type_code(1, create).
permission_type_code(2, input).
permission_type_code(3, modify).
permission_type_code(4, open).
permission_type_code(5, output).
permission_type_code(6, reposition).

permission_object_code(0, binary_stream).
permission_object_code(1, source_sink).
permission_object_code(2, stream).
permission_object_code(3, text_stream).
permission_object_code(4, flag).
permission_object_code(5, operator).
permission_object_code(6, past_end_of_stream).
permission_object_code(7, private_procedure).
permission_object_code(8, static_procedure).

representation_code(0, character_code_list).
representation_code(1, in_character_code).
representation_code(2, max_arity).
representation_code(3, character).
representation_code(4, max_integer).
representation_code(5, min_integer).
representation_code(6, character_code).
representation_code(7, nan_or_inf_to_integer).
representation_code(8, max_atom_length).

evaluation_code(0, float_overflow).
evaluation_code(1, int_overflow).
evaluation_code(2, undefined).
evaluation_code(3, underflow).
evaluation_code(4, zero_divisor).

resource_code(0, undefined).
resource_code(1, heap).

:- multifile('$internal_error_where_term'/4).

where_term(PredName, PredArity, Arg, WhereError):-
    '$internal_error_where_term'(PredName, PredArity, Arg, WhereError), !.
where_term(PredName, PredArity, 0, PredName/PredArity) :- !.
where_term(PredName, PredArity, Arg, PredName/PredArity-Arg).

% ---------------------------------------------------------------------------
:- doc(section, "Internal for streams").

:- if(defined(optim_comp)).
:- else.
:- use_module(engine(stream_basic), [atm_or_int/1, stream/1]). % TODO: circular? move types somewhere else?
:- endif.

:- if(defined(optim_comp)).
:- else.
:- export('$open'/3).
:- trust pred '$open'(File,Mode,Stream) : (atm_or_int(File), atm(Mode)) => stream(Stream).
:- impl_defined('$open'/3).
:- endif.

:- export('$prompt'/2).
:- if(defined(optim_comp)).
:- '$props'('$prompt'/2, [impnat=cbool(prompt)]).
:- else.
:- trust pred '$prompt'(Old,New) : atm(New) => atm(Old).
:- trust pred '$prompt'(-Old,-New) : (Old == New) => (atm(Old), atm(New)). 
:- impl_defined('$prompt'/2).
:- endif.

:- export('$force_interactive'/0).
:- if(defined(optim_comp)).
:- '$props'('$force_interactive'/0, [impnat=cbool(prolog_force_interactive)]).
:- else.
:- impl_defined('$force_interactive'/0).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Internal for system.pl").

% TODO:[oc-merge] in system.c, move here? keep there?

:- export('$unix_argv'/1).
:- if(defined(optim_comp)).
:- '$props'('$unix_argv'/1, [impnat=cbool(prolog_unix_argv)]).
:- else.
:- trust pred '$unix_argv'(Argv) : list(atm,Arg).
:- impl_defined('$unix_argv'/1).
:- endif.

:- if(defined(optim_comp)).
:- export('$unix_shift_arg'/0).
:- '$props'('$unix_shift_arg'/0, [impnat=cbool(prolog_unix_shift_arg)]).
:- else.
% TODO:[oc-merge] backport
:- endif.

:- export('$find_file'/8).
:- if(defined(optim_comp)).
:- '$props'('$find_file'/8, [impnat=cbool(prolog_find_file)]).
:- else.
:- trust pred '$find_file'(LibDir, Path, Opt, Suffix, Found, AbsFile, AbsBase, AbsDir)
    : (atm(LibDir), atm(Path), atm(Opt), atm(Suffix))
    => (true_fail(Found), atm(AbsFile), atm(AbsBase), atm(AbsDir)).
:- impl_defined('$find_file'/8).

:- regtype true_fail/1.
true_fail(true).
true_fail(fail).
:- endif.

% TODO: split in two versions? (concat cwd may be optional)
% TODO: not exported, only used in bundlereg_load.pl?
:- if(defined(optim_comp)).
:- '$props'('$expand_file_name'/3, [impnat=cbool(prolog_expand_file_name)]).
:- else.
:- trust pred '$expand_file_name'(Path,Abs,Path2) : atm(Path) => atm(Path2).
:- impl_defined('$expand_file_name'/3).
:- endif.

% TODO: duplicated: same as path_is_absolute/1
:- export('$path_is_absolute'/1).
:- if(defined(optim_comp)).
:- '$props'('$path_is_absolute'/1, [impnat=cbool(prolog_path_is_absolute)]).
:- else.
:- trust pred '$path_is_absolute'(Path) : atm(Path).
:- impl_defined('$path_is_absolute'/1).
:- endif.

:- export('$exec'/9).
:- if(defined(optim_comp)).
:- '$props'('$exec'/9, [impnat=cbool(prolog_exec)]).
:- else.
:- trust pred '$exec'/9. % (see system.c for details)
:- impl_defined('$exec'/9).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Internal for format.pl").

:- if(defined(optim_comp)).
:- else.
 :- export('$format_print_float'/3).
:- trust pred '$format_print_float'(Format,Arg,Prec) : (int(Format), flt(Arg), int(Prec)).
:- impl_defined('$format_print_float'/3).

:- export('$format_print_integer'/3).
:- trust pred '$format_print_integer'(Format,Arg,Prec) : (int(Format), flt(Arg), int(Prec)).
:- trust pred '$format_print_integer'(Format,Arg,Prec) : (int(Format), int(Arg), int(Prec)).
:- impl_defined('$format_print_integer'/3). 
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Module initialization (both static and dynamic)").
% (as part the 'module' object operations)

:- if(defined(optim_comp)).
:- use_module(engine(rt_exp), [do_initialization/1]).
:- else.
:- export(initialized/1).
:- data initialized/1.

:- export(initialize_module/1).
initialize_module(M) :- current_fact(initialized(M)), !.
initialize_module(M) :- asserta_fact(initialized(M)),
    do_initialize_module(M).

do_initialize_module(M) :-
    '$u'(M, N),
    initialize_module(N),
    fail.
do_initialize_module(M) :-
    '$initialization'(M),
    fail.
do_initialize_module(_).

% warp internal predicate, as requested by Jose Manuel % TODO: needed now?
:- export(initialization/1).
initialization(M) :- '$initialization'(M). 
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Runtime module expansions").
% (as part of the 'module' object operations)

:- if(defined(optim_comp)).
% TODO:[oc-merge] this is kept in rt_exp.pl
:- else.
:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(engine(messages_basic), [message/2]).

% Called from engine(mexpand)
uses_runtime_module_expansion.
ciaopp_expansion :- fail.

:- export(goal_trans/3).
:- data goal_trans/3.

:- include(library(compiler/mexpand)).
mexpand_meta_args(M, P, Primitive) :-
    '$meta_args'(M, P),
    ( '$primitive_meta_predicate'(P, M) ->
        Primitive = true
    ; Primitive = fail
    ).
mexpand_imports(M, IM, F, N, EM) :-
    '$imports'(M, IM, F, N, EM).
mexpand_defines(M, F, N) :-
    '$defines'(M, F, N).
mexpand_defines(M, F, N) :-
    functor(Meta, F, N),
    '$meta_args'(M, Meta),
    meta_inc_args(Meta, N, N1),
    '$defines'(M, F, N1).
mexpand_multifile(M, F, N) :-
    '$multifile'(M, F, N).

redefining(_,_,_). % Avoid imported_needs_qual warnings

module_warning(not_defined(F, N, M)) :- !,
    ( '$unknown'(fail,fail) -> true
    ; message(warning, ['Predicate ',~~(F/N),' undefined in module ',M])
    ).
module_warning(not_imported(F, N, M, QM)) :- !,
    message(error, ['Bad module qualification of ',~~(F/N),
                    ', module ',M,
                    ' does not import the predicate from module ',QM]).
module_warning(bad_pred_abs(PA)) :- !,
    message(error, ['Bad predicate abstraction ',~~(PA),
                      ' : head functor should be ''''']).
module_warning(big_pred_abs(PA, N)) :- !,
    message(error, ['Predicate abstraction ',~~(PA),
                      ' has too many arguments: should be ',N]).
module_warning(short_pred_abs(PA, N)) :- !,
    message(error, ['Predicate abstraction ',~~(PA),
                      ' has too few arguments: should be ',N]).

:- export(term_to_meta/2).
:- pred term_to_meta/2 # "Transforms a normal term to a meta-term.".

term_to_meta(X, '$:'(X)).

% These two backwards compatibility
:- export(last_module_exp/5).
last_module_exp(T, Type, M, QM, NT) :-
    rt_module_exp(T, Type, M, QM, true, NT).

:- export(mid_module_exp/5).
mid_module_exp(T, Type, M, QM, NT) :-
    rt_module_exp(T, Type, M, QM, fail, NT).


rt_module_exp(V, _Type, _M, _QM, _Pr, V) :- var(V), !.
rt_module_exp(T, _Type, _M, _QM,  Pr, NT) :-
    T = '$:'(Tx), !, % already expanded
    ( Pr = true -> NT = Tx ; NT = T).
rt_module_exp(QM:T, Type, M,_QM, Pr, NT) :- !,
    ( var(QM) ->
        meta_expansion_type(Type, T, M, _, Pr, NT, no, no)
    ; rt_module_exp(T, Type, M, QM, Pr, NT)
    ).
rt_module_exp(T, Type, M, QM, Pr, NT) :-
    do_module_exp(QM, T, M, Pr, Type, NT), !.
rt_module_exp(T,_Type,_M,_QM,_Pr, T).

do_module_exp(QM, T, M, Primitive, Type, NT) :-
    nonvar(QM),
    functor(QM, XM, 1), !,
%        accessible_in(M, XM, mod_exp, 5),
    arg(1, QM, XQM),
    atom_concat(XM, ':mod_exp', PX), % XM:mod_exp/5 makes the expansion
    functor(GEXP, PX, 5),
    '$predicate_property'(GEXP, _, _),
    arg(1,GEXP,Type),
    arg(2,GEXP,T),
    arg(3,GEXP,M),
    arg(4,GEXP,XQM),
    arg(5,GEXP,XT),
    '$meta_call'(GEXP),
    term_to_meta_or_primitive(Primitive, XT, NT).
do_module_exp(QM, T, M, Primitive, Type, NT) :-
    meta_expansion_type(Type, T, M, QM, Primitive, NT, no, no).

meta_expansion_type(Type, X, M, QM, Primitive, NX, NG, NG_) :-
    meta_expansion_type_(Type, X, M, QM, compile, Primitive, NX, NG, NG_).

% TODO: move together with runtime_control:module_split/3?
% TODO: inefficient, write in C or adopt a hash-table approach like in optim_comp
:- export(module_concat/3).
module_concat(user(_), X0, X) :- !,
    module_concat(user, X0, X).
module_concat(Module, X0, X) :-
    X0 =.. [F0|Args],
    atom(F0), !,
    atom_concat(Module, ':', Mc),
    atom_concat(Mc, F0, F),
    X =.. [F|Args].
module_concat(_, X, X). % If a number, do not change to complain later
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Dynamic module changes").
% (as part of the 'module' object operations)

:- if(defined(optim_comp)).
% TODO:[oc-merge] this is kept in rt_exp.pl
:- else.
:- export('$define_predicate'/2).
:- trust pred '$define_predicate'(Pred,Mode) : (predname(Pred), pred_mode(Mode)).
:- impl_defined('$define_predicate'/2).

:- regtype pred_mode/1.
pred_mode(consult).
pred_mode(interpreted).
pred_mode(profiled).
pred_mode(unprofiled).

:- export('$set_property'/2).
:- trust pred '$set_property'(Head,Prop) : (cgoal(Head), pred_property(Prop)).
:- impl_defined('$set_property'/2).

:- regtype pred_property/1.
pred_property(multifile).
pred_property(dynamic).
pred_property(concurrent).
pred_property(wait).

:- export('$current_predicate'/2).
:- trust pred '$current_predicate'(V,Pred) : cgoal(Pred). %jcf% No info about V.
:- impl_defined('$current_predicate'/2).

:- export('$predicate_property'/3).
:- trust pred '$predicate_property'(Head,Entry,Bits) => cgoal * int * int.
:- impl_defined('$predicate_property'/3).

:- export('$module_is_static'/1).
:- trust pred '$module_is_static'(M) => atm(M).
:- impl_defined('$module_is_static'/1).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Source path resolver").

%! ## Bundle registry

:- if(defined(optim_comp)).
% TODO:[oc-merge] port to OC
:- else.
% Database
:- include(library(bundle/bundlereg_db)).
% Loading code
:- include(library(bundle/bundlereg_load)).

:- export(ciao_root/1).
% The CIAOROOT directory
:- impl_defined([ciao_root/1]).
:- endif.

%! ## Alias paths

:- if(defined(optim_comp)).
% TODO:[oc-merge] this is in compiler_oc/store.pl, split?; note that loader in optim-comp is a user program
:- else.
% TODO:[JF]: New absolute file name library. I need it to fix some pending issues
% of the foreign interface and future problems with the compilation to C

% A product is any output of a compilation 
% A source is the input of a compilation

:- trust success library_directory(X) => gnd(X).
:- multifile library_directory/1.
:- dynamic(library_directory/1).

:- trust success file_search_path(X,Y) => (gnd(X), gnd(Y)).
:- multifile file_search_path/2.
:- dynamic(file_search_path/2).

file_search_path(library, Lib) :- library_directory(Lib).
file_search_path(Alias, Path) :- '$bundle_alias_path'(Alias, _Bundle, Path).
file_search_path(.,.).
:- endif.

%! ## Path initialization

:- if(defined(optim_comp)).
% TODO:[oc-merge] 'store:setup_paths'
:- else.
setup_paths :-
    % Setup default path aliases
    ciao_root(CiaoRoot),
    path_concat(CiaoRoot, 'core/lib', LibPath),
    assertz_fact(library_directory(LibPath)),
    path_concat(CiaoRoot, 'core/library', LibraryPath),
    assertz_fact(library_directory(LibraryPath)),
    path_concat(CiaoRoot, 'core/engine', Engine),
    assertz_fact(file_search_path(engine, Engine)),
    % Setup path for bundles (using CIAOPATH if available)
    get_ciaopath,
    % Load bundleregs (if available)
    reload_bundleregs,
    % Fill use_cache_dir
    ( c_get_env('CIAOCCACHE', '0') -> % TODO: build/engine option instead?
        true
    ; fill_cache_dir
    ).

:- use_module(engine(system_info), [ciao_c_headers_dir/1]).

fill_cache_dir :- % cachedir relative to the workspace
    ( % Detect if we are using build/ or build-boot/
      ciao_root(Root),
      ciao_c_headers_dir(HDir),
      atom_concat(Root, RelHDir, HDir),
      atom_concat('/build-boot/', _, RelHDir) ->
        RelCacheDir = 'build-boot/cache'
    ; RelCacheDir = 'build/cache'
    ),
    ( % (failure-driven loop)
      ciao_wksp(Wksp, WkspBase),
        atom_concat(WkspBase, '/', Prefix),
        path_concat(Wksp, RelCacheDir, CacheDir),
        assertz_fact(use_cache_dir(CacheDir, Prefix)),
        fail
    ; true
    ).
:- endif.

%! ## Source (and compiled) file resolution

:- if(defined(optim_comp)).
:- else.
:- use_module(engine(system_info), [get_os/1, get_arch/1, get_a_ext/1, get_so_ext/1]).

%JF: filename for some type of files
:- export(po_filename/2).
po_filename(Base, Name) :- product_filename(prolog_object, Base, Name).
:- export(wam_filename/2).
wam_filename(Base, Name) :- product_filename(prolog_wam, Base, Name).
:- export(itf_filename/2).
itf_filename(Base, Name) :- product_filename(prolog_itf, Base, Name).
:- export(asr_filename/2).
asr_filename(Base, Name) :- product_filename(prolog_assertion, Base, Name).
:- export(ast_filename/2). % (like .asr, for CiaoPP)
ast_filename(Base, Name) :- product_filename(prolog_assertion2, Base, Name).
:- export(a_filename/2).
a_filename(Base, Name) :- product_filename(gluecode_a, Base, Name).
:- export(so_filename/2).
so_filename(Base, Name) :- product_filename(gluecode_so, Base, Name).
%
:- export(testout_filename/2). % (for unittests)
testout_filename(Base, Name) :- product_filename(prolog_testout, Base, Name).

% JF: Name of a file
:- export(product_filename/3).
product_filename(Type, Base0, Name) :-
%       ( Type = prolog_object -> display(po_filename(Base0)), nl ; true ),
    filetype(Type, Ext, ArchDep),
    ( ArchDep = noarch ->
        Suffix = Ext
    ; get_os_arch_suffix(OsArchSuffix),
      glue_suffix(Type, GlueSuffix),
      atom_concat(OsArchSuffix, GlueSuffix, Suffix0),
      atom_concat(Suffix0, Ext, Suffix)
    ),
    translate_base(Base0, Base),
    atom_concat(Base, Suffix, Name),
%       ( Type = prolog_object -> display(po_filename_2(Name)), nl ; true ).
    true.

:- data use_cache_dir/2.

% %TO DEACTIVATE
% TODO: translate to C to avoid atom polution
% TODO: detect the bundle of each base to create shorter and relocatable names
%translate_base(Base, Base) :- display(user_error, trbase(Base)), nl(user_error), fail.
translate_base(Base0, Base2) :-
    use_cache_dir(CacheDir, Prefix),
    atom_concat(Prefix, Base, Base0),
    !,
    atom_codes(Base, Codes),
    translate_base_2(Codes, Codes1),
    atom_codes(Base1, Codes1),
    path_concat(CacheDir, Base1, Base2).
%       display(user_error, Base), nl(user_error),
%       display(user_error, Base2), nl(user_error).
translate_base(Base, Base).

translate_base_2("."||Xs0, ".."||Xs) :- !, % (escape .)
    translate_base_2(Xs0, Xs).
translate_base_2("/"||Xs0, "."||Xs) :- !,
    translate_base_2(Xs0, Xs).
translate_base_2([X|Xs0], [X|Xs]) :- !,
    translate_base_2(Xs0, Xs).
translate_base_2([], []).

% TODO: Rewrite in a way that does not need find_pl_filename (when CIAOCCACHE is enabled)
%:- export(find_so_filename/2).
find_so_filename(File, Abs) :-
    get_os_arch_suffix(OsArchSuffix),
    get_so_ext(SOExt),
    absolute_file_name_(File, OsArchSuffix, SOExt, '.', Abs, Base, _),
    Abs \== Base,  % Has .so extension
    !.
find_so_filename(File, Abs) :-
    find_pl_filename(File, _PlName, Base, _Dir),
    so_filename(Base, Abs).

get_os_arch_suffix(OsArchSuffix) :-
    get_os(Os),
    get_arch(Arch),
    atom_concat(Os, Arch, OsArch),
    atom_concat('_', OsArch, OsArchSuffix).

% TODO: Rewrite in a way that does not need find_pl_filename (when CIAOCCACHE is enabled)
%:- export(find_po_filename/2).
find_po_filename(File, Abs) :-
    opt_suff(Opt),
    absolute_file_name_(File, Opt, '.po', '.', Abs, Base, _),
    Abs \== Base, % Has .po extension
    !.
find_po_filename(File, Abs) :-
    find_pl_filename(File, _PlName, Base, _Dir),
    po_filename(Base, Abs).

:- export(find_pl_filename/4).
find_pl_filename(File, PlName, Base, Dir) :-
    % TODO: if file is a path, ok... but if file is a module desc,
    % this is not correct (= in ciao 1.9)
    atom(File), !,
    opt_suff(Opt),
    ( '$find_file'('.', File, Opt, '.pl', true, PlName, Base, Dir) ->
        true
    ; '$find_file'('.', File, Opt, '', true, PlName, Base, Dir)
    ).
find_pl_filename(File, PlName, Base, Dir) :- 
    opt_suff(Opt),
    absolute_file_name_(File, Opt, '.pl', '.', PlName, Base, Dir).

% :- export(find_c_filename/4).
% find_c_filename(File, CName, Base, Dir) :- 
%       absolute_file_name_(File, [], '.c', '.', CName, Base, Dir).

%:- pred absolute_file_name(+sourcename,+atm,+atm,+atm,-atm,-atm,-atm).

% TODO: define a data types for module spec, slash paths, etc.

% (called from stream_basic:absolute_file_name/7)
:- export('$absolute_file_name_checked'/7).
'$absolute_file_name_checked'(Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir) :-
    absolute_file_name_(Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir).

absolute_file_name_(Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir) :-
    % Test Spec to be an alias (e.g., library(a/b/c)).
    nonvar(Spec),
    functor(Spec, Alias, 1),
    arg(1,Spec,Name0),
    slash_to_path(Name0, Name),
    atom(Name), !,
    ( file_search_path(Alias, Dir),
      atom(Dir),
      '$find_file'(Dir, Name, Opt, Suffix, true, AbsFile, AbsBase, AbsDir) ->
        true
    ; file_not_found_error(Spec)
    ).
absolute_file_name_(Name, Opt, Suffix, CurrDir, AbsFile, AbsBase, AbsDir) :-
    atom(Name), !,
    '$find_file'(CurrDir, Name, Opt, Suffix, _, AbsFile, AbsBase, AbsDir).
absolute_file_name_(X, _, _, _, _, _, _) :-
    throw(error(domain_error(source_sink, X), absolute_file_name/7-1)).

file_not_found_error(Spec) :-
    ( '$ferror_flag'(on, on) ->
        throw(error(existence_error(source_sink,Spec), absolute_file_name/7-1))
    ; fail
    ).

% Get (relative) pathname from term notation (e.g., a/b/c -> 'a/b/c').
% (see pathnames:pathname/1)
slash_to_path((SubName/Name), Flat) :- !,
    slash_to_path(SubName, SubFlat),
    atom_concat(SubFlat, '/', SubFlat1),
    atom_concat(SubFlat1, Name, Flat).
slash_to_path(Spec, Flat) :-
    atom(Spec),
    !,
    Flat = Spec.
slash_to_path(Spec, _Flat) :-
    % TODO: define a right type for Spec (module_spec does not exist) (perhaps sourcename/1)
    throw(error(domain_error(module_spec, Spec), slash_to_path/2-1)).

% TODO: engine(internals) should not import lib/ or library/ modules
%
%   This works because both predicates are impl_defined already in the
%   engine.
%:- use_module(library(system), [file_exists/2, file_properties/6]).
:- import(system, [file_exists/2, file_properties/6]).
:- import(system, [c_get_env/2]).
:- use_module(engine(runtime_control), [prolog_flag/3, set_prolog_flag/2]).

is_dir(Path) :-
    prolog_flag(fileerrors, OldFE, off),
    file_properties(Path, directory, [], [], [], []),
    set_prolog_flag(fileerrors, OldFE).

% JF: Information about file types involved in compilation
% - THIS IS NOT A MIME-like LIST: 
% JF: will be moved to a different module when possible...
:- export(filetype/3).
filetype(prolog_object,     '.po',  noarch).
filetype(prolog_wam,        '.wam', noarch).
filetype(prolog_itf,        '.itf', noarch).
filetype(prolog_assertion,  '.asr', noarch).
filetype(prolog_assertion2, '.ast', noarch).
filetype(prolog_testout,    '.testout', noarch).
filetype(c_object,          '.o',   arch).
filetype(gluecode_c,        '.c',   arch).
filetype(gluecode_o,        '.o',   arch).
filetype(gluecode_a,        Ext,    arch) :- get_a_ext(Ext).
filetype(gluecode_so,       Ext,     arch) :- get_so_ext(Ext).

% MCL: not all arch-dep files need to have the '_glue' marker
glue_suffix(c_object,    '').
glue_suffix(gluecode_c,  '_glue').
glue_suffix(gluecode_o,  '_glue').
glue_suffix(gluecode_a,  '').
glue_suffix(gluecode_so, '').

:- data opt_suff/1.

:- export(opt_suff/1).
opt_suff('_opt').

% TODO: move preds here and reexport/wrap from pathnames
% (copy of pathnames that can be used before modules are initialized)
:- include(library(pathnames_boot)).
% :- use_module(library(pathnames), [path_concat/3]).

:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Dynamic module loading").

:- if(defined(optim_comp)).
% Force dependency % TODO: polish, some executables may not require them!
:- use_module(engine(modload)).
:- else.

:- use_module(engine(stream_basic), [close/1]).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(runtime_control), [prolog_flag/3, set_prolog_flag/2]).

:- export(load_lib/2).
:- pred load_lib(Module,File) : atm * atm.

load_lib(Module,_File) :-
    '$current_module'(Module), !.
load_lib(Module, File) :- % loads both .so and .po - JFMC
    prolog_flag(fileerrors, OldFE, off),
    ( find_pl_filename(File, _, Base, _),
      so_filename(Base, SoName),
      file_exists(SoName, 0),
        dynlink(SoName, Module),
%            assertz_fact(current_module(Module)),
        fail
    ; true
    ),
    ( find_po_filename(File, PoName),
        poload(PoName),
        ldlibs(Module),
        fail
    ; true
    ),
    set_prolog_flag(fileerrors, OldFE),
    check_module_loaded(Module, File).

ldlibs(X) :- '$ldlibs'(X).

% (for lazy load)

:- entry load_lib_lazy/2.
load_lib_lazy(Module,_File) :-
    '$current_module'(Module), !.
load_lib_lazy(Module, File) :- % loads both .so and .po - JFMC
    del_stumps(Module),
    prolog_flag(fileerrors, OldFE, off),
    ( find_po_filename(File, PoName),
        poload(PoName),
        fail
    ; true
    ),
    ( find_so_filename(File, SoName),
        dynlink(SoName, Module),
%            assertz_fact(current_module(Module)),
        fail
    ; true
    ),
    set_prolog_flag(fileerrors, OldFE),
    retractall_fact(initialized(Module)),
    initialize_module(Module),
    check_module_loaded(Module, File).

check_module_loaded(Module,_File) :- '$current_module'(Module), !.
check_module_loaded(_Module,File) :-
    message(error,['library ',File,' not found, exiting...']),
    halt(1).

:- trust pred stump(A,B) => (atm(A), cgoal(B)).
:- multifile stump/2.
:- data stump/2.

del_stumps(Module) :-
    retract_fact(stump(Module, Pred)),
    '$abolish'(Pred),
    fail.
del_stumps(_).

% Low-level loading of objects (QL)
poload(AbsName) :-
    '$push_qlinfo',
    '$open'(AbsName, r, Stream),            % Gives errors
    '$qread'(Stream, Version),
    poversion(Version), !,
    repeat,
      '$qread'(Stream, Goal),
      (   Goal= -1
      ;   'SYSCALL'(Goal), fail
      ), !,
    '$pop_qlinfo',
    close(Stream).
poload(AbsName) :-
    message(error, ['could not load ', AbsName,
                    ' (missing, corrupt, or wrong .po version)']),
    halt(1).

:- export(load_so/2).
load_so(Module, File) :-
    find_so_filename(File, SoName),
    dynlink(SoName, Module).

:- export(load_po/1).
load_po(File) :-
    find_po_filename(File, PoName),
    poload(PoName).

:- export(poversion/1).
poversion(version(67)).

:- endif.

% --------------------------------------------------------------------------
:- doc(section, "Dynamic loading of native code (.so)").

:- if(defined(optim_comp)).
:- else.
:- export(dynlink/2).
:- trust pred dynlink(File,Module) : atm * atm.
:- impl_defined(dynlink/2).

:- export(dynunlink/1).
:- trust pred dynunlink(File) : atm.
:- impl_defined(dynunlink/1).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Signal handlers").

:- initialization(fail).
:- on_abort(fail).

:- if(defined(optim_comp)).
:- export(control_c_handler/0). % TODO:[oc-merge] internal entry, not an export
:- else.
:- entry control_c_handler/0.
:- endif.
control_c_handler :- send_signal(control_c).

% ---------------------------------------------------------------------------
:- doc(section, "Boot the machine").

:- use_module(engine(messages_basic), [message/2]).

:- if(defined(optim_comp)).
:- export(boot/0). % TODO:[oc-merge] internal entry, not an export
:- else.
:- entry boot/0.
:- endif.
boot :-
    init,
    run_main_entry.

% TODO: recode in C?
% High level part of the initialization
:- data init_ok/0.
init :-
    init_hooks, !,
    asserta_fact(init_ok).
init :-
    message(error, '{Internal initialization failed}'),
    halt(1).

:- if(defined(optim_comp)).
:- export(reboot/0). % TODO:[oc-merge] internal entry, not an export
:- else.
:- entry reboot/0.
:- endif.
:- if(defined(optim_comp)).
reboot :-
    init_ok, % stop if initialization did not succeed
    % note: global vars are not reinitialized since they are backtrackable
    abort_hooks, % TODO: reset debugger state?
    !.
:- else.
reboot :-
    init_ok, % stop if initialization did not succeed
    % note: global vars are not reinitialized since they are backtrackable
    initialize_debugger_state,
    abort_hooks,
    !.
:- endif.

:- if(defined(optim_comp)).
init_hooks :-
    ( '$main_module'(MainModule),
      rt_exp:do_initialization(MainModule),
      fail
    ; true
    ).
:- else.
% TODO: backport 'loader' from optim_comp (it will allow smaller executables)
init_hooks :-
    setup_paths,
    ( '$load_libs' ; true ), % load dyn linked libs (see exemaker.pl) % TODO: use loader.pl instead
    initialize_debugger_state,
    init_hooks_.

init_hooks_ :-
    main_module(M),
    initialize_module(M),
    fail.
init_hooks_.

:- use_module(engine(hiord_rt), ['SYSCALL'/1]).
% hack: since '$debugger_state' loads a global variable with a reference
% to a heap term, once executed you should not fail...
initialize_debugger_state :-
    '$current_module'(debugger), !,
    'SYSCALL'('debugger:initialize_debugger_state').
initialize_debugger_state.
:- endif.

:- if(defined(optim_comp)).
:- import(user, [aborting/0]).
:- else.
:- use_module(user, [aborting/0]).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1]).
:- endif.
abort_hooks :-
    '$on_abort'(_), % module's abort hook
    fail.
:- if(defined(optim_comp)).
abort_hooks :-
    % user's abort hook
    aborting.
:- else.
abort_hooks :-
    % user's abort hook
    ( '$predicate_property'('user:aborting',_,_) ->
        '$nodebug_call'(aborting)
    ; % Exit with error code 1 if no user:aborting/0 is defined
      % (for standalone executables)
      halt(1)
    ).
:- endif.

:- if(defined(optim_comp)).
:- import(internal_init, ['$main_module'/1]).
:- import(internal_init, ['$main_entry'/1]).
:- else.
:- trust pred main_module(M) => atm(M).
:- impl_defined(main_module/1).
:- use_module(user, [main/0, main/1, '$main'/1]).
:- use_module(engine(hiord_rt), ['$nodebug_call'/1]).
'$main_entry'(_Args) :-
    '$predicate_property'('user:main',_,_), !,
    '$nodebug_call'(main).
'$main_entry'(Args) :-
    '$predicate_property'('user:main'(_),_,_), !,
    '$nodebug_call'(main(Args)).
'$main_entry'(Args) :-
    '$predicate_property'('user:$main'(_),_,_), !, % TODO: use multifile instead?
    '$nodebug_call'('$main'(Args)).
'$main_entry'(_) :-
    message(error,'Predicates user:main/0 and user:main/1 undefined, exiting...'), % TODO: only for user modules! (it is a bug otherwise)
    halt(1).
:- endif.

% Run the main entry
run_main_entry :-
    ( '$unix_argv'(Args), '$main_entry'(Args) ->
        true
    ; message(error, '{Program ended with failure}'),
      halt(2)
    ).

