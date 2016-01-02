:- module(internals, [], [assertions, basicmodes, nortchecks, regtypes]).

:- use_module(user, [main/0, main/1, aborting/0]).

:- doc(title,"Engine Internal Predicates").  

:- doc(module,"

This library lists a set of internal predicates (written in C) used by
the system code. They should not be used in user code. The file itself
provides handles for the module system into the internal definitions.

").

:- use_module(engine(hiord_rt), ['SYSCALL'/1, '$nodebug_call'/1, '$meta_call'/1]).

% ---------------------------------------------------------------------------
:- doc(section, "Internal Debugging").

:- export('$show_nodes'/2).
:- trust pred '$show_nodes'/2.   %jcf% Not used in Prolog code.
:- impl_defined('$show_nodes'/2).

:- export('$show_all_nodes'/0).
:- trust pred '$show_all_nodes'/0.   %%jcf% Not used in Prolog code.
:- impl_defined('$show_all_nodes'/0).

:- export('$start_node'/1).
:- trust pred '$start_node'/1. %jcf% Not used in Prolog code.
:- impl_defined('$start_node'/1).

% ---------------------------------------------------------------------------
:- doc(section, "Engine version").

:- export('$bootversion'/0).
:- impl_defined('$bootversion'/0).

:- export('$ciao_version'/6).
:- trust pred '$ciao_version'(Version, Patch,
	                      CommitBranch, CommitId, CommitDate, CommitDesc) =>
	(atm(Version), atm(Patch),
	 atm(CommitBranch), atm(CommitId), atm(CommitDate), atm(CommitDesc)).
:- impl_defined('$ciao_version'/6).

% ---------------------------------------------------------------------------
:- doc(section, "Stream support").

:- export('$open'/3).
:- trust pred '$open'(File,Mode,Stream) : (atm_or_int(File), atm(Mode)) => stream(Stream).
:- impl_defined('$open'/3).

:- export('$prompt'/2).
:- trust pred '$prompt'(Old,New) : atm(New) => atm(Old).
:- trust pred '$prompt'(-Old,-New) : (Old == New) => (atm(Old), atm(New)). 
:- impl_defined('$prompt'/2).

:- export('$force_interactive'/0).
:- impl_defined('$force_interactive'/0).

% ---------------------------------------------------------------------------
:- doc(section, "Dynamic predicates").

:- export('$purge'/1).
:- trust pred '$purge'/1. %jcf% Not used in Prolog code.
:- impl_defined('$purge'/1).

:- export('$erase'/1).
:- trust pred '$erase'(+Ptr) : int(Ptr).
:- impl_defined('$erase'/1).

:- export('$ptr_ref'/2).
:- trust pred '$ptr_ref'(Ptr,Ref) : int(Ptr) => ref(Ref).
:- trust pred '$ptr_ref'(Ptr,Ref) : ref(Ref) => int(Ptr).
:- impl_defined('$ptr_ref'/2).

:- regtype ref/1.
ref('$ref'(A,B)):- int(A), int(B).

:- export('$inserta'/2).
:- trust pred '$inserta'(Root,Ptr) : (int(Root), int(Ptr)).
:- impl_defined('$inserta'/2).

:- export('$insertz'/2).
:- trust pred '$insertz'(Root,Ptr) : (int(Root), int(Ptr)).
:- impl_defined('$insertz'/2).

:- export('$make_bytecode_object'/4).
:- trust pred '$make_bytecode_object'(Size,Counters,Tokens,Obj) 
	: (int(Size), int(Counters), list(Tokens)) => int(Obj).
:- impl_defined('$make_bytecode_object'/4).

:- export('$abolish'/1).
:- trust pred '$abolish'(Head) : callable(Head).
:- impl_defined('$abolish'/1).

:- export('$compile_term'/2).
:- trust pred '$compile_term'(LClause,Ptr) : list_clause(LClause) => int(Ptr).
:- impl_defined('$compile_term'/2).

:- regtype list_clause/1.
list_clause([Head|Body]):- callable(Head), body(Body).

:- regtype body/1.
body(X) :- 
	callable(X).
body((X,Xs)) :-
	callable(X),
	body(Xs).

:- export('$instance'/3).
:- trust pred '$instance'(Head,Body,Ptr) : int(Ptr) => (callable(Head), body(Body)).
:- impl_defined('$instance'/3).

:- export('$define_predicate'/2).
:- trust pred '$define_predicate'(Pred,Mode) : (predname(Pred), pred_mode(Mode)).
:- impl_defined('$define_predicate'/2).

:- regtype pred_mode/1.
pred_mode(consult).
pred_mode(interpreted).
pred_mode(profiled).
pred_mode(unprofiled).

:- export('$erase_clause'/1).
:- trust pred '$erase_clause'/1. %jcf% Not used in Prolog code.
:- impl_defined('$erase_clause'/1).

:- export('$clause_number'/2).
:- trust pred '$clause_number'(Pred,Number). %jcf% Not used in Prolog code.
:- impl_defined('$clause_number'/2).

:- export('$compiled_clause'/4).
:- trust pred '$compiled_clause'(Pred,Obj,Mode,Data) 
	: (predname(Pred), int(Obj), pred_mode(Mode)). %jcf% No info about Data.
:- impl_defined('$compiled_clause'/4).

:- export('$empty_gcdef_bin'/0).
:- impl_defined('$empty_gcdef_bin'/0).

:- export('$set_property'/2).
:- trust pred '$set_property'(Head,Prop) : (callable(Head), pred_property(Prop)).
:- impl_defined('$set_property'/2).

:- regtype pred_property/1.
pred_property(multifile).
pred_property(dynamic).
pred_property(concurrent).
pred_property(wait).

% ---------------------------------------------------------------------------

:- export('$ddt'/1).
:- trust pred '$ddt'(T) : int(T). %jcf% Check that int is the right type!. Not used in Prolog code.
:- impl_defined('$ddt'/1).

% ---------------------------------------------------------------------------
:- doc(section, "QL bytecode loader").

:- export('$qread'/2).
:- trust pred '$qread'(Stream, Term) : stream(Stream) => int(Term). %jcf%Check that int is the right type!.
:- impl_defined('$qread'/2).

:- export('$push_qlinfo'/0).
:- impl_defined('$push_qlinfo'/0).

:- export('$pop_qlinfo'/0).
:- impl_defined('$pop_qlinfo'/0).

% ---------------------------------------------------------------------------

:- export('$frozen'/2).
:- trust pred '$frozen'/2. %jcf% Not used in Prolog code.
:- impl_defined('$frozen'/2).

:- export('$defrost'/2).
:- trust pred '$defrost'/2. %jcf% Not used in Prolog code.
:- impl_defined('$defrost'/2).

% ---------------------------------------------------------------------------

:- export('$setarg'/4).
:- trust pred '$setarg'(I, +Term, +Newarg, Mode) : (int(I), setarg_mode(Mode)).
:- impl_defined('$setarg'/4).

:- regtype setarg_mode/1.
setarg_mode(on).
setarg_mode(off).
setarg_mode(true).

% ---------------------------------------------------------------------------

:- export('$undo_goal'/1). 
:- trust pred '$undo_goal'(Goal) : callable(Goal).
:- impl_defined('$undo_goal'/1). 

% ---------------------------------------------------------------------------

:- export('$exit'/1).
:- trust pred '$exit'(A) => int(A).
:- impl_defined('$exit'/1).

% ---------------------------------------------------------------------------

:- export('$unknown'/2).
:- trust pred '$unknown'(Old,+New) : unknown_level(New) => unknown_level(Old).
:- trust pred '$unknown'(-Old,-New) : (Old == New) => (unknown_level(Old), unknown_level(New)). 
:- impl_defined('$unknown'/2).

:- regtype unknown_level/1.
unknown_level(error).
unknown_level(fail).
unknown_level(warning).

% ---------------------------------------------------------------------------

:- export('$compiling'/2).
:- trust pred '$compiling'(Old,+New) : compiling_mode(New) => compiling_mode(Old).
:- trust pred '$compiling'(-Old,-New) : (Old == New) => (compiling_mode(Old), compiling_mode(New)). 
:- impl_defined('$compiling'/2).

:- regtype compiling_mode/1.
compiling_mode(profiled).   %jcf% Not used
compiling_mode(unprofiled).

% ---------------------------------------------------------------------------

:- export('$ferror_flag'/2).
:- trust pred '$ferror_flag'(Old,+New) : on_off(New) => on_off(Old).
:- trust pred '$ferror_flag'(-Old,-New) : (Old == New) => (on_off(Old), on_off(New)). 
:- impl_defined('$ferror_flag'/2).

:- regtype on_off/1.
on_off(on).
on_off(off).

% ---------------------------------------------------------------------------

:- export('$quiet_flag'/2).
:- trust pred '$quiet_flag'(Old,+New) : quiet_mode(New) => quiet_mode(Old).
:- trust pred '$quiet_flag'(-Old,-New) : (Old == New) => (quiet_mode(Old), quiet_mode(New)). 
:- impl_defined('$quiet_flag'/2).

:- regtype quiet_mode/1.
quiet_mode(on).
quiet_mode(error).
quiet_mode(warning).
quiet_mode(debug).
quiet_mode(off).

% ---------------------------------------------------------------------------

% :- export('$prolog_radix'/2).
% :- impl_defined('$prolog_radix'/2).

% ---------------------------------------------------------------------------

:- export('$constraint_list'/2).
:- trust pred '$constraint_list'/2. %jcf% Not used in Prolog code.
:- impl_defined('$constraint_list'/2).

% ---------------------------------------------------------------------------

:- export('$eq'/2).
:- trust pred '$eq'(Term,Term). %jcf% Not used in Prolog code.
:- impl_defined('$eq'/2).

% ---------------------------------------------------------------------------

:- export('$large_data'/3).
:- trust pred '$large_data'/3. %jcf% Not used in Prolog code.
:- impl_defined('$large_data'/3).

% ---------------------------------------------------------------------------

:- export('$interpreted_clause'/2).
:- trust pred '$interpreted_clause'(Pred,LClause) 
	: (predname(Pred), list_clause(LClause)). %jcf% Check that these are the right types!
:- impl_defined('$interpreted_clause'/2).

% ---------------------------------------------------------------------------
/* system.pl */
	
:- export('$unix_popen'/3).
:- trust pred '$unix_popen'/3. %jcf% Not used in Prolog code.
:- impl_defined('$unix_popen'/3).

:- export('$exec'/9).
:- trust pred '$exec'/9. % (see os_utils.c for details)
:- impl_defined('$exec'/9).

:- export('$unix_argv'/1).
:- trust pred '$unix_argv'(Argv) : list(Arg,atm).
:- impl_defined('$unix_argv'/1).

:- export('$find_file'/8).
:- trust pred '$find_file'(LibDir, Path, Opt, Suffix, Found, AbsFile, AbsBase, AbsDir)
	: (atm(LibDir), atm(Path), atm(Opt), atm(Suffix))
        => (true_fail(Found), atm(AbsFile), atm(AbsBase), atm(AbsDir)).
:- impl_defined('$find_file'/8).
% $find_file(+LibDir, +Path, +Opt, +Suffix, ?Found, -AbsPath, -AbsBase, -AbsDir)
%
%   string LibDir       a library in which to search for Path
%   string Path	        a path, may be absolute or relative. If LibDir
%   	                is specified then Path must be relative to LibDir.
%   string Opt          an optional suffix to Path, must precede Suffix, is
%                       included in AbsBase
%   string Suffix       an optional suffix to Path, not included in AbsBase
%   atom   Found        true or fail
%   string AbsPath      the absolute pathname of Path
%   string AbsBase      the absolute pathname of Path, without Suffix
%   string AbsDir       the absolute pathname of the directory of Path
%
% Description: Try to find in LibDir, in this order:
%   Path+Opt+Suffix
%   Path+Suffix
%   Path
%   Path/Path+Opt+Suffix
%   Path/Path+Suffix
%   Path/Path
% if any found, unify Found with true, and return in AbsPath, AbsBase and
% AbsDir the appropriate values, else unify Found with false, and return in
% AbsPath, AbsBase and AbsDir the values corresponding to the last option
% (no Opt nor Suffix).

:- regtype true_fail/1.
true_fail(true).
true_fail(fail).

% TODO: split in two versions? (concat cwd may be optional)
:- trust pred '$expand_file_name'(Path,Abs,Path2) : atm(Path) => atm(Path2).
:- impl_defined('$expand_file_name'/3).

% ---------------------------------------------------------------------------
 /* format.pl */

 :- export('$format_print_float'/3).
:- trust pred '$format_print_float'(Format,Arg,Prec) : (int(Format), flt(Arg), int(Prec)).
:- impl_defined('$format_print_float'/3).

:- export('$format_print_integer'/3).
:- trust pred '$format_print_integer'(Format,Arg,Prec) : (int(Format), flt(Arg), int(Prec)).
:- trust pred '$format_print_integer'(Format,Arg,Prec) : (int(Format), int(Arg), int(Prec)).
:- impl_defined('$format_print_integer'/3). 

:- export('$erase_atom'/1).
:- trust pred '$erase_atom'/1. %jcf% Not used in Prolog code.
:- impl_defined('$erase_atom'/1).

:- export('$runtime'/1).
:- trust pred '$runtime'(Time) => flt_list2(Time).
:- impl_defined('$runtime'/1).

:- regtype flt_list2/1.
flt_list2([F1,F2]) :- flt(F1), flt(F2).

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

:- export('$termheap_usage'/1).
:- trust pred '$termheap_usage'(Usage) => int_list2(Usage).
:- impl_defined('$termheap_usage'/1).

:- regtype int_list2/1.
int_list2([I1,I2]) :- int(I1), int(I2).

:- export('$envstack_usage'/1).
:- trust pred '$envstack_usage'(Usage) => int_list2(Usage).
:- impl_defined('$envstack_usage'/1).

:- export('$trail_usage'/1).
:- trust pred '$trail_usage'(Usage) => int_list2(Usage).
:- impl_defined('$trail_usage'/1).

:- export('$choice_usage'/1).
:- trust pred '$choice_usage'(Usage) => int_list2(Usage).
:- impl_defined('$choice_usage'/1).

:- export('$stack_shift_usage'/1).
:- trust pred '$stack_shift_usage'(Usage) => int_list3(Usage).
:- impl_defined('$stack_shift_usage'/1).

:- regtype int_list3/1.
int_list3([I1,I2,I3]) :- int(I1), int(I2), int(I3).

:- export('$program_usage'/1).
:- trust pred '$program_usage'(Usage) => int_list2(Usage).
:- impl_defined('$program_usage'/1).

:- export('$internal_symbol_usage'/1).
:- trust pred '$internal_symbol_usage'(Usage) => int_list2(Usage).
:- impl_defined('$internal_symbol_usage'/1).

:- export('$total_usage'/1).
:- trust pred '$total_usage'(Usage) => int_list2(Usage).
:- impl_defined('$total_usage'/1).

:- export('$gc_mode'/2).
:- trust pred '$gc_mode'(Old,+New) : on_off(New) => on_off(Old).
:- trust pred '$gc_mode'(-Old,-New) : (Old == New) => (on_off(Old), on_off(New)). 
:- impl_defined('$gc_mode'/2).

:- export('$gc_trace'/2).
:- trust pred '$gc_trace'(Old,+New) : gc_trace_modes(New) => gc_trace_modes(Old).
:- trust pred '$gc_trace'(-Old,-New) : (Old == New) => (gc_trace_modes(Old), gc_trace_modes(New)). 
:- impl_defined('$gc_trace'/2).

:- regtype gc_trace_modes/1.
gc_trace_modes(on).
gc_trace_modes(off).
gc_trace_modes(terse).
gc_trace_modes(verbose).

:- export('$gc_margin'/2).
:- trust pred '$gc_margin'(Old,+New) : int(New) => int(Old).
:- trust pred '$gc_margin'(-Old,-New) : (Old == New) => (int(Old), int(New)). 
:- impl_defined('$gc_margin'/2).

:- export('$gc_usage'/1).
:- trust pred '$gc_usage'(Usage) => gc_list3(Usage).
:- impl_defined('$gc_usage'/1).

:- regtype gc_list3/1.
gc_list3([F1,I2,I3]) :- flt(F1), int(I2), int(I3).

:- export('$current_predicate'/2).
:- trust pred '$current_predicate'(V,Pred) : callable(Pred). %jcf% No info about V.
:- impl_defined('$current_predicate'/2).

:- export('$predicate_property'/3).
:- trust pred '$predicate_property'(Head,Entry,Bits) => callable * int * int.
:- impl_defined('$predicate_property'/3).

:- export('$module_is_static'/1).
:- trust pred '$module_is_static'(M) => atm(M).
:- impl_defined('$module_is_static'/1).

:- export('$current_clauses'/2).
:- trust pred '$current_clauses'(Head,Root) : callable(Head) => int(Root).
:- impl_defined('$current_clauses'/2).

:- export('$first_instance'/2).
:- trust pred '$first_instance'(Root,Ptr) : int(Root) => int(Ptr).
:- impl_defined('$first_instance'/2).

:- export('$current_instance'/5).
:- trust pred '$current_instance'(Head, Body, Root, Ptr, Blocking) 
	: (callable(Head), body(Body), int(Root), blocking_mode(Blocking)) => int(Ptr).
:- impl_defined('$current_instance'/5).

:- regtype blocking_mode/1.
blocking_mode(block).
blocking_mode(no_block).

:- export('$emulated_clause_counters'/4).
:- trust pred '$emulated_clause_counters'/4. %jcf% Not used in Prolog code.
:- impl_defined('$emulated_clause_counters'/4).

:- export('$counter_values'/3).
:- trust pred '$counter_values'/3. %jcf% Not used in Prolog code.
:- impl_defined('$counter_values'/3).

:- export('$close_predicate'/1).
:- trust pred '$close_predicate'(Root) : int(Root).
:- impl_defined('$close_predicate'/1).

:- export('$open_predicate'/1).
:- trust pred '$open_predicate'(Root) : int(Root).
:- impl_defined('$open_predicate'/1).

:- export('$unlock_predicate'/1).
:- trust pred '$unlock_predicate'(Root) : int(Root).
:- impl_defined('$unlock_predicate'/1).

:- export('$reset_counters'/2).
:- trust pred '$reset_counters'/2. %jcf% Not used in Prolog code.
:- impl_defined('$reset_counters'/2).

:- trust pred main_module(M) => atm(M).
:- impl_defined(main_module/1).

% --------------------------------------------------------------------------
:- doc(section, "Dynamic loading of native code (.so)").

:- export(dynlink/2).
:- trust pred dynlink(File,Module) : atm * atm.
:- impl_defined(dynlink/2).

:- export(dynunlink/1).
:- trust pred dynunlink(File) : atm.
:- impl_defined(dynunlink/1).

% --------------------------------------------------------------------------

:- export('$atom_mode'/2). /* write.pl */
:- trust pred '$atom_mode'(Atom, Context) : atm(Atom) => int(Context).
:- impl_defined('$atom_mode'/2). 

% ---------------------------------------------------------------------------
:- doc(section, "Startgoal support code").

% Call with continuations (for startgoal.c)

% Called from within the emulator, as possible boot goal for a wam
:- entry call_with_cont/1.
call_with_cont([](Goal, OnSuccess, _OnFailure)):-
        'SYSCALL'(Goal),
        'SYSCALL'(OnSuccess).
call_with_cont([](_Goal_, _OnSuccess, OnFailure)):-
        'SYSCALL'(OnFailure).

% --------------------------------------------------------------------------
:- doc(section, "Ciao boot").
% (Initialization, module loading, and call to main/0 or main/1)

% :- export('$eng_call'/6).  % Concurrency hook
% :- primitive_meta_predicate('$eng_call'(goal, ?, ?, ?, ?, ?)).
% :- impl_defined('$eng_call'/6).

:- data all_loaded/0.

% :- use_module(engine(debugger_support), ['$debugger_mode'/0, '$debugger_state'/2]).

:- entry boot/0.
boot:-
        setup_paths,
        ( '$load_libs' ; true ),
	initialize_debugger_state,
	initialize_global_vars,
	initialize,
        asserta_fact(all_loaded),
        gomain.
boot:-
        message(error,'Predicates user:main/0 and user:main/1 undefined, exiting...'),
        halt(1).

gomain :-
        '$predicate_property'('user:main',_,_), !,
	( '$nodebug_call'(main) -> true ; global_failure ).
gomain :-
        '$predicate_property'('user:main'(_),_,_), !,
        current_prolog_flag(argv, Args),
        ( '$nodebug_call'(main(Args)) -> true ; global_failure ).

global_failure :-
        message(error, '{Program ended with failure}'),
        halt(2).

:- entry reboot/0.
reboot :-
        all_loaded,
	initialize_debugger_state,
	initialize_global_vars,
	reinitialize,
        ( '$predicate_property'('user:aborting',_,_) ->
	    '$nodebug_call'(aborting)
	; % Exit with error code 1 if no user:aborting/0 is defined
	  % (for standalone executables)
	  halt(1)
	),
	!.

initialize :-
	initialize_debugger_state,
	initialize_global_vars,
	initialize_2.

initialize_2 :-
	main_module(M),
        initialize_module(M),
	fail.
initialize_2.

% hack: since '$debugger_state' loads a global variable with a reference
% to a heap term, once executed you should not fail...
initialize_debugger_state :-
	'$current_module'(debugger), !,
	'SYSCALL'('debugger:initialize_debugger_state').
initialize_debugger_state.

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

reinitialize:-
	'$on_abort'(_),
	fail.
reinitialize.

% warp internal predicate, as requested by Jose Manuel
:- export(initialization/1).
initialization(M) :- '$initialization'(M). 

% ---------------------------------------------------------------------------
:- doc(section, "Low level global variables").

% Currently, the global variables are reserved statically:
%
%   [optim_comp only]
%   1 - debugger
%   2 - action__compile
%   4 - action__split
%   3 - Errs object during analysis
%   5 - unused, see compiler/dynload.pl
%   6 - absmach
%
%   [rest of Ciao]
%   10 - CHR package (chr/hrpolog.pl)
%   11 - global_vars module
%
% TODO: move to a different file and share
%       (core/engine/internals.pl and core_OC/engine/internals.pl)

% non mutable version
:- export('$global_vars_get'/2).
'$global_vars_get'(I, X) :-
        '$global_vars_get_root'(R),
        arg(I, R, X).

initialize_global_vars :-
	F = '$glb'(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
	           _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), % 32 global vars
	'$global_vars_set_root'(F).

:- impl_defined('$global_vars_get_root'/1).
:- impl_defined('$global_vars_set_root'/1).

% ---------------------------------------------------------------------------
:- doc(section, "Support for runtime module expansions").

:- initialization(fail).
:- on_abort(fail).

:- entry control_c_handler/0.
control_c_handler :- send_signal(control_c).

%% Module expansion

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

% ---------------------------------------------------------------------------
:- doc(section, "Dynamic loading of libraries").

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

:- trust pred stump(A,B) => (atm(A), callable(B)).
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

% ---------------------------------------------------------------------------
:- doc(section, "Bundle registry").

% Database
:- include(library(bundle/bundlereg_db)).
% Loading code
:- include(library(bundle/bundlereg_load)).

% ---------------------------------------------------------------------------
:- doc(section, "Filesystem abstraction for source names").

% JF: New absolute file name library. I need it to fix some pending issues
% of the foreign interface and future problems with the compilation to C

% A product is any output of a compilation 
% A source is the input of a compilation

% ------ paths ------ %

:- trust success library_directory(X) => gnd(X).
:- multifile library_directory/1.
:- dynamic library_directory/1.

:- trust success file_search_path(X,Y) => (gnd(X), gnd(Y)).
:- multifile file_search_path/2.
:- dynamic file_search_path/2.

file_search_path(library, Lib) :- library_directory(Lib).
file_search_path(Alias, Path) :- '$bundle_alias_path'(Alias, _Bundle, Path).
file_search_path(.,.).

setup_paths :-
	% Setup default alias paths
        ciao_lib_dir(Path),
        atom_concat(Path,'/lib',LibPath),
        assertz_fact(library_directory(LibPath)),
        atom_concat(Path,'/library',LibraryPath),
        assertz_fact(library_directory(LibraryPath)),
        atom_concat(Path, '/engine', Engine),
        assertz_fact(file_search_path(engine, Engine)),
	% Load bundleregs (if available)
	reload_bundleregs,
	% Setup for CIAOCACHEDIR
	( c_get_env('CIAOCACHEDIR', CacheDir) ->
	    assertz_fact(use_cache_dir(CacheDir))
	; true
	).

%JF: filename for some type of files
:- export(po_filename/2).
po_filename(Base, Name) :-
	product_filename(prolog_object, Base, Name).
:- export(wam_filename/2).
wam_filename(Base, Name) :-
	product_filename(prolog_wam, Base, Name).
:- export(itf_filename/2).
itf_filename(Base, Name) :-
	product_filename(prolog_itf, Base, Name).
:- export(asr_filename/2).
asr_filename(Base, Name) :-
	product_filename(prolog_assertion, Base, Name).
:- export(ast_filename/2). % (like .asr, for CiaoPP)
ast_filename(Base, Name) :-
	product_filename(prolog_assertion2, Base, Name).
:- export(a_filename/2).
a_filename(Base, Name) :-
	product_filename(gluecode_a, Base, Name).
:- export(so_filename/2).
so_filename(Base, Name) :-
	product_filename(gluecode_so, Base, Name).
%
:- export(testout_filename/2). % (for unittests)
testout_filename(Base, Name) :-
	product_filename(prolog_testout, Base, Name).

% JF: Name of a file
:- export(product_filename/3).
product_filename(Type, Base0, Name) :-
%	( Type = prolog_object -> display(po_filename(Base0)), nl ; true ),
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
%	( Type = prolog_object -> display(po_filename_2(Name)), nl ; true ).
	true.

:- data use_cache_dir/1.

% %TO DEACTIVATE
% TODO: translate to C to avoid atom polution
% TODO: detect the bundle of each base to create shorter and relocatable names
%translate_base(Base, Base) :- display(user_error, trbase(Base)), nl(user_error), fail.
translate_base(Base, Base2) :-
	use_cache_dir(CacheDir),
	!,
	atom_codes(Base, Codes),
	translate_base_2(Codes, Codes1),
	atom_codes(Base1, Codes1),
	path_concat(CacheDir, Base1, Base2).
%	display(user_error, Base), nl(user_error),
%	display(user_error, Base2), nl(user_error).
translate_base(Base, Base).

translate_base_2("."||Xs0, ".."||Xs) :- !, % (escape .)
	translate_base_2(Xs0, Xs).
translate_base_2("/"||Xs0, "."||Xs) :- !,
	translate_base_2(Xs0, Xs).
translate_base_2([X|Xs0], [X|Xs]) :- !,
	translate_base_2(Xs0, Xs).
translate_base_2([], []).

% TODO: Rewrite in a way that does not need find_pl_filename (when CIAOCACHEDIR is enabled)
%:- export(find_so_filename/2).
find_so_filename(File, Abs) :-
        get_os_arch_suffix(OsArchSuffix),
	get_so_ext(SOExt),
        my_absolute_file_name(new, File, OsArchSuffix, SOExt, '.', Abs, Base, _),
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

% TODO: Rewrite in a way that does not need find_pl_filename (when CIAOCACHEDIR is enabled)
%:- export(find_po_filename/2).
find_po_filename(File, Abs) :-
	opt_suff(Opt),
        my_absolute_file_name(new, File, Opt, '.po', '.', Abs, Base, _),
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
	( my_find_file(new, '.', File, Opt, '.pl', true, PlName, Base, Dir) ->
	    true
	; my_find_file(new, '.', File, Opt, '', true, PlName, Base, Dir)
	).
find_pl_filename(File, PlName, Base, Dir) :- 
        opt_suff(Opt),
	my_absolute_file_name(new, File, Opt, '.pl', '.', PlName, Base, Dir).

% :- export(find_c_filename/4).
% find_c_filename(File, CName, Base, Dir) :- 
% 	my_absolute_file_name(new, File, [], '.c', '.', CName, Base, Dir).

%:- true pred absolute_file_name(+sourcename,+atm,+atm,+atm,-atm,-atm,-atm).

% TODO: define a data types for module spec, slash paths, etc.

% (called from streams_basic:absolute_file_name/7)
:- export('$absolute_file_name_checked'/7).
'$absolute_file_name_checked'(Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir) :-
	my_absolute_file_name(old, Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir).

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

% ImplVer=old to use the old implementation, new for the new implementation
my_absolute_file_name(ImplVer, Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir) :-
        % Test Spec to be an alias (e.g., library(Module) or similar).
        nonvar(Spec),
        functor(Spec, Alias, 1),
        arg(1,Spec,Name0),
	slash_to_path(Name0, Name),
        atom(Name), !,
        ( file_search_path(Alias, Dir),
          atom(Dir),
          my_find_file(ImplVer, Dir, Name, Opt, Suffix, true, AbsFile, AbsBase, AbsDir) ->
	    true
        ; file_not_found_error(Spec)
        ).
my_absolute_file_name(ImplVer, Name, Opt, Suffix, CurrDir, AbsFile, AbsBase, AbsDir) :-
        atom(Name), !,
        my_find_file(ImplVer, CurrDir, Name, Opt, Suffix, _, AbsFile, AbsBase, AbsDir).
my_absolute_file_name(_, X, _, _, _, _, _, _) :-
        throw(error(domain_error(source_sink, X), absolute_file_name/7-1)).

file_not_found_error(Spec) :-
	( '$ferror_flag'(on, on) ->
	    throw(error(existence_error(source_sink,Spec), absolute_file_name/7-1))
	; fail
	).

% TOpt is 0 if Path does not exists or is a directory,
% else, it is the modification time

%% modif_time0_nodir(Path, Time) :-
%% 	( my_modif_time0(Path, Time),
%% 	  \+ Time = 0,
%% 	  \+ is_dir(Path) ->
%% 	    true
%% 	; Time = 0
%% 	).

my_modif_time0(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], T, [], []), !
        ; T = 0
        ),
        set_prolog_flag(fileerrors, OldFE),
        Time = T.

% TODO: engine(internals) should not import lib/ or library/ modules
%
%   This works because both predicates are impl_defined already in the
%   engine.
%:- use_module(library(system), [file_exists/2, file_properties/6]).
:- import(system, [file_exists/2, file_properties/6]).
:- import(system, [c_get_env/2]).

is_dir(Path) :-
        prolog_flag(fileerrors, OldFE, off),
        file_properties(Path, directory, [], [], [], []),
        set_prolog_flag(fileerrors, OldFE).

my_find_file(old, LibDir0, Path0, Opt, Suffix, Exists, AbsFile, AbsBase, AbsDir) :-
	'$find_file'(LibDir0, Path0, Opt, Suffix, Exists, AbsFile, AbsBase, AbsDir).
my_find_file(new, LibDir0, Path0, Opt, Suffix, Exists, AbsFile, AbsBase, AbsDir) :-
	% TODO: differences between 'old' and 'new' version?
	( atom_concat(Path1, Suffix, Path0) -> % remove the extension
	    true
	; Path1 = Path0
	),
	'$expand_file_name'(LibDir0, true, LibDir0e),
	'$expand_file_name'(Path1, false, Path1e),
	( LibDir0e = '.' -> Path = Path1e
	; path_concat(LibDir0e, Path1e, Path)
	),
	( my_find_file_exists(Path, Opt, Suffix, AbsFile) ->
	    Exists = true,
	    atom_concat(AbsBase, Suffix, AbsFile) % without extension
	; my_find_file_no_exists(Path, AbsFile) ->
	    Exists = false,
	    AbsBase = AbsFile
	; fail
	),
%	display(user_error, mff(LibDir0, Path0, Exists, AbsFile)), nl(user_error),
	path_split(AbsFile, AbsDir, _). % without last filename

% 1) newer non-directory of {path+opt+suffix, path+suffix}
% 2) path, if path does not exists
% 3) path, if a non-directory
% 4) recursive call with duplicated file name in path, if path is a directory

my_find_file_exists(Path, Opt, Suffix, AbsFile) :-
	( my_find_file_3(Path, Opt, Suffix, AbsFile) ->
	    true
	; is_dir(Path) ->
	    duplicate_dir_name(Path, DupPath), % search inside
	    my_find_file_3(DupPath, Opt, Suffix, AbsFile)
	; fail
	).

my_find_file_no_exists(Path, AbsFile) :-
	( is_dir(Path) ->
	    duplicate_dir_name(Path, DupPath), % search inside
	    \+ file_exists(DupPath, 0),
	    AbsFile = DupPath
	; \+ file_exists(Path, 0),
	  AbsFile = Path
	).

% newer non-directory of {path+opt+suffix, path+suffix}
my_find_file_3(Path, Opt, Suffix, AbsFile) :-
	( \+ Opt = '', atom_concat(Path, Opt, PathOpt),
	  atom_concat(PathOpt, Suffix, PathOptSuffix) ->
	    my_modif_time0(PathOptSuffix, TOpt)
        ; TOpt = 0
	),
	( /*\+ Suffix = '',*/ atom_concat(Path, Suffix, PathSuffix) ->
	    my_modif_time0(PathSuffix, TPri)
        ; TPri = 0
	),
	( TPri > TOpt -> % path+suffix exists, path+opt+suffix older|absent
	    AbsFile = PathSuffix
	; TOpt > 0 -> % newer path+opt+suffix exists
	    AbsFile = PathOptSuffix
	; fail
	).

duplicate_dir_name(Path, Path2) :-
	path_split(Path, _, Name),
	path_concat(Path, Name, Path2).

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
filetype(gluecode_c,        '.c',   arch).
filetype(gluecode_o,        '.o',   arch).
filetype(gluecode_a,        Ext,    arch) :- get_a_ext(Ext).
filetype(gluecode_so,       Ext,     arch) :- get_so_ext(Ext).

% MCL: not all arch-dep files need to have the '_glue' marker
glue_suffix(gluecode_c,        '_glue').
glue_suffix(gluecode_o,        '_glue').
glue_suffix(gluecode_a,        '').
glue_suffix(gluecode_so,       '').

:- data opt_suff/1.

:- export(opt_suff/1).
opt_suff('_opt').

% ---------------------------------------------------------------------------
% [duplicated from library(pathnames), currently boot loader cannot include
% lib/ or library/ modules]

% :- use_module(library(pathnames), [path_concat/3, path_split/3]).

% TODO: backport 'loader' from optim_comp (it will allow smaller executables)

path_concat('', B, R) :- !, R = B.
path_concat(_A, B, R) :- '$path_is_absolute'(B), !,
	R = B.
path_concat(A, B, R) :-
	( atom_concat(_, '/', A) ->
	    A0 = A
        ; atom_concat(A, '/', A0) % add '/' if needed
	),
	atom_concat(A0, B, R).

% TODO: duplicated: same as path_is_absolute/1
:- export('$path_is_absolute'/1).
:- trust pred '$path_is_absolute'(Path) : atm(Path).
:- impl_defined('$path_is_absolute'/1).

path_split(Path, Dir, Base) :-
	atom_codes(Path, PathS),
	path_split_(PathS, DirS, BaseS),
	atom_codes(Dir, DirS),
	atom_codes(Base, BaseS).

path_split_(Path, Dir, Base) :-
	reverse(Path, R),
	( append(BaseR, "/"||DirR, R) ->
	    anysep(DirR, DirR2), % Strip all trailing /
	    ( DirR2 = "" -> % (Dir is root, preserve all /)
	        Dir = "/"||DirR
	    ; reverse(DirR2, Dir)
	    ),
	    reverse(BaseR, Base)
	; Dir = "",
	  Base = Path
	).

% Zero or more '/'
anysep("/"||Xs, Ys) :- !, anysep(Xs, Ys).
anysep(Xs, Xs).

% [duplicated from library(lists)]

append([], L, L).
append([E|Es], L, [E|R]) :- append(Es, L, R).

reverse(Xs,Ys):- reverse_(Xs,[],Ys).

reverse_([], L, L).
reverse_([E|Es],L,R) :- reverse_(Es,[E|L],R).

% ---------------------------------------------------------------------------
:- doc(section, "Attributed variables").

% (entry information)
:- trust pred verify_attribute(A,B).
:- trust pred combine_attributes(A,B).
:- entry uvc/2.
:- entry ucc/2.
:- entry pending_unifications/1.
:- include(attributed_variables).

% ---------------------------------------------------------------------------
:- doc(section, "Internal builtin errors").

% Called from within the emulator
:- entry error/5.

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

error_term( 1, _, instantiation_error) :- !.
error_term( 2, Culprit, uninstantiation_error(Culprit)) :- !.
error_term(Code, _, system_error) :-   in_range(system, Code, _), !.
error_term(Code, _, syntax_error) :-   in_range(syntax, Code, _), !.
error_term(N, _, resource_error(Res)) :- 
	in_range(res, N, Code), !, 
	resource_code(Code, Res).
error_term(Code, _, user_error) :-     in_range(user,   Code, _), !.
error_term(N, _Culprit, evaluation_error(Type)) :-
        in_range(eval, N, Code), !,
        evaluation_code(Code, Type).
error_term(N, _Culprit, representation_error(Type)) :-
        in_range(repres, N, Code), !,
        representation_code(Code, Type).
error_term(N, Culprit, type_error(Type, Culprit)) :-
        in_range(type, N, Code),
        type_code(Code, Type).
error_term(N, Culprit, domain_error(Type, Culprit)) :-
        in_range(dom, N, Code),
        domain_code(Code, Type).
error_term(N, Culprit, existence_error(Type, Culprit)) :-
        in_range(exist, N, Code),
        existence_code(Code, Type).
error_term(N, Culprit, permission_error(Permission, Object, Culprit)) :-
        in_range(perm, N, Code),
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

%% This is the Prolog counterpart of the definitions in support.h.  Please 
%% have a look there!

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

:- export('$undo_heap_overflow_excep'/0).
:- trust pred '$undo_heap_overflow_excep'/0.
:- impl_defined('$undo_heap_overflow_excep'/0). 

:- export('$heap_limit'/1).
:- trust pred '$heap_limit'/1.
:- impl_defined('$heap_limit'/1). 

% ---------------------------------------------------------------------------

