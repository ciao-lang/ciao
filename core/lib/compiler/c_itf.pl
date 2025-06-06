:- module(_, [], [assertions, nortchecks, hiord, define_flag, datafacts]).

:- doc(title, "Compiler frontend").
:- doc(author, "The Ciao Development Team").

:- doc(module, "This module implements the Ciao compiler front-end.

   The main entry point for the front-end is the
   @pred{process_files_from/7} predicate, which process (Ciao) source
   files and all their dependencies. Source file processing is
   incremental and dependecy extraction automatic from source
   declarations. For each processed file, a user-provided back-end (as
   higher-order arguments) is invoked.

   Based on incremental file processing, this module implements
   source-to-bytecode compilation (using @lib{pl2wam} and @lib{wamql}
   as back-end) and basic support for dynamic module loading
   (@pred{use_mod/3}).").

% itf sections: allow much faster dependecy checkings
:- compilation_fact(itf_sections). % (comment out to disable)

% ---------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(hiord_rt), [call/1]).

:- use_module(library(compiler/translation)).
:- use_module(library(compiler/pl2wam)).
:- use_module(library(compiler/srcdbg), [srcdbg_expand/6]).
:- use_module(library(compiler/global_module_options)).
:- use_module(library(compiler/file_buffer)).
:- use_module(library(fastrw)).
:- use_module(library(varnames/complete_dict)).
:- use_module(engine(runtime_control), [current_prolog_flag/2,
     set_prolog_flag/2, prolog_flag/3, push_prolog_flag/2,
     pop_prolog_flag/1]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(terms_io), [term_write/1]).
:- use_module(engine(messages_basic), [
    message/2, message_lns/4, message_type_visible/1]).
:- use_module(engine(runtime_control), [current_module/1]).
:- use_module(engine(hiord_rt), [this_module/1]).
:- use_module(engine(internals), [
    filetype/3,
    po_filename/2,
    wam_filename/2,
    itf_filename/2,
    so_filename/2,
    product_filename/3,
    find_pl_filename/4,
    opt_suff/1,
    %
    '$open'/3, initialize_module/1, initialized/1,
    '$set_currmod'/1,
    '$define_predicate'/2, '$set_property'/2, '$compiled_clause'/4,
    '$compile_term'/2, '$current_clauses'/2, '$insertz'/2, '$abolish'/1,
    '$current_instance'/5, '$erase'/1, '$predicate_property'/3,
    '$module_is_static'/1,
    '$unlock_predicate'/1, dynlink/2, dynunlink/1,
    poversion/1, '$qread'/2, '$push_qlinfo'/0, '$pop_qlinfo'/0,
    % Used by mexpand
    module_concat/3, term_to_meta/2]).
:- use_module(engine(internals), [ciao_root/1]).
:- use_module(library(system), [
    modif_time0/2, modif_time/2, now/1, fmode/2, chmod/2,
    working_directory/2, file_exists/1]).
:- use_module(library(dynamic/dynamic_rt),    [wellformed_body/3]).
:- use_module(library(pathnames), [path_basename/2, path_split/3, path_concat/3, path_is_relative/1]).
:- use_module(library(strings),    [whitespace0/2]).
:- use_module(library(stream_utils),    [get_line/1]).
:- use_module(library(ctrlcclean), [ctrlcclean/0]).
:- use_module(library(terms),      [copy_args/3, atom_concat/2]).
:- use_module(library(hiordlib), [maplist/2, maplist/3]).
:- use_module(library(lists)).
:- use_module(library(read)).
:- use_module(library(operators)).
:- use_module(library(compiler/build_foreign_interface)). % JFMC
:- use_module(library(assertions/assrt_lib), [normalize_assertion/9,
    assertion_body/7, assertion_read/9, comps_to_goal/3]).

% ---------------------------------------------------------------------------
:- doc(section, "The current compiler version").

% The compiler version can be used in conditional code to test
% incremental changes in the compiler source without intermediate
% compiler promotions:
%
%   :- if('$with_compiler_version'(VPlus1)).
%     << code using the new feature >>
%   :- else.
%     << previous code >>
%   :- endif.

:- export(compiler_version/1).
compiler_version(109). % TODO: keep synchronized with optim_comp

:- export(compiler_name/1).
compiler_name(c_itf).

% Interface version number. Increment to enforce invalidation of
% previously compiled files. This is useful when incremental
% compilation cannot deal with deep changes in the libraries (e.g.,
% default imports) or the compiler (format, runtime information, etc.)

itf_version(6).

% ---------------------------------------------------------------------------
% (Register static compilation modules that are part of bootstrap ciaoc)

%:- use_module(library(compiler/basic_compilation_modules)).
:- include(library(compiler/basic_compilation_modules)).

% ---------------------------------------------------------------------------
:- doc(section, "Compiler flags and settings").

define_flag(verbose_compilation,    [on,  off], off).
define_flag(itf_format,             [f,   r],   f). % f=fast{read,write}, r=prolog terms.
define_flag(read_assertions,        [yes, no],  yes).

% runtime checks related flags:
define_flag(runtime_checks,          [yes, no],                no).
define_flag(rtchecks_level,          [inner, exports],         inner).
define_flag(rtchecks_trust,          [yes, no],                yes).
define_flag(rtchecks_entry,          [yes, no],                yes).
define_flag(rtchecks_exit,           [yes, no],                yes).
define_flag(rtchecks_test,           [yes, no],                no).
define_flag(rtchecks_asrloc,         [yes, no],                yes).
define_flag(rtchecks_predloc,        [yes, no],                yes).
define_flag(rtchecks_callloc,        [no, literal, predicate], predicate).
define_flag(rtchecks_namefmt,        [short, long],            long).

% Keep asertions after reading
% TODO: This is a temporary hack for assertions/assrt_lib. It needs better integration.
% (e.g. assrt_lib distinguishes main files and components because it
%  was designed for documentation purposes, does not normalize
%  assertions in the same way for them, and only write .asr for
%  compoments)
define_flag(keep_assertions,        [yes, no],  no).

:- export(opt_suffix/2).
opt_suffix(Old, New) :- Old == New, !,
    current_fact(opt_suff(Old)).
opt_suffix(Old, New) :-
    retract_fact(opt_suff(Old)), 
    asserta_fact(opt_suff(New)).

% ---------------------------------------------------------------------------
:- doc(section, "Data coming from/going to itf file").

% Needed by dependent files

:- export(defines_module/2).
:- pred defines_module(Base, Module)
    # "The source @var{Base}.pl defines module @var{Module}.".
:- data defines_module/2.

:- pred direct_export(Base, F, A, DefType, Meta)
    # "The source @var{Base}.pl directly exports predicate @var{F}/@var{A},
       defined as @var{DefType} (static, implicit, dynamic, data or
       concurrent) and with meta_predicate declaration @var{Meta}
       (which can be 0 if it has not).".
:- data direct_export/5.

:- export(def_multifile/4).
:- pred def_multifile(Base, F, A, DynType)
    # "The source @var{Base}.pl defines multifile predicate
       @var{F}/@var{A}, defined as @var{DynType} (static, dynamic, data
       or concurrent).".
:- data def_multifile/4.

:- export(decl/2).
:- pred decl(Base, Decl)
    # "The source @var{Base}.pl contains the declaration @var{Decl}
       as an itf-exported new_declaration.".
:- data decl/2.

:- export(exports/5).
:- pred exports(Base, F, A, DefType, Meta)
    # "The source @var{Base}.pl exports predicate @var{F}/@var{A},
       defined as @var{DefType} (static, implicit, dynamic, data or
       concurrent) and with meta_predicate declaration @var{Meta}
       (which can be 0 if it has not).".

exports(Base, F, A, DefType, Meta) :-
    direct_export(Base, F, A, DefType, Meta).
exports(Base, F, A, DefType, Meta) :-
    reexports_pred(Base, File, F, A),
    imports_pred(Base, File, F, A, DefType, Meta, _EndFile).

% Data to follow dependencies

:- export(uses/2).
:- pred uses(Base, File)
    # "The source @var{Base}.pl imports from file @var{File}.".
:- data uses/2.

:- export(adds/2).
:- pred adds(Base, File)
    # "The source @var{Base}.pl does @decl{ensure_loaded/1} of file
       @var{File}.".
:- data adds/2.

:- pred reexports_from(Base, File)
    # "The source @var{Base}.pl reexports from file @var{File}.".
:- data reexports_from/2.

:- export(uses_file/2).
:- pred uses_file(Base, File)
    # "The source @var{Base}.pl uses file @var{File} explicitly through
        @decl{use_module/1} or @decl{use_module/2} or implicity.".

uses_file(Base, File) :- uses(Base, File).

% Data for dependency check

:- export(imports_pred/7).
% NOTE: ImpFile can be 'user'
:- pred imports_pred(Base, ImpFile, F, A, DefType, Meta, EndFile)
    # "The source @var{Base}.pl imports from file @var{ImpFile}
       predicate @var{F}/@var{A}.  Predicate is defined as
       @var{DefType} and has meta_predicate declaration @var{Meta}
       (possibly 0).  @var{EndFile} is '.' if the predicate resides
       in @var{ImpFile}, otherwise it is the file in which the
       predicate resides (due to reexportations).  Stored in itf
       file for dependency check.".
:- data imports_pred/7.

:- export(imports_all/2).
:- pred imports_all(Base, ImpFile)
    # "The source @var{Base}.pl imports all predicates of @var{ImpFile}.".
:- data imports_all/2.

:- pred reexports(Base, File, F, A)
    # "The source @var{Base}.pl reexports predicate @var{F}/@var{A}
      from file @var{File}.".
:- data reexports/4.

:- pred reexports_all(Base, File)
    # "The source @var{Base}.pl reexports all predicates of @var{File}.".
:- data reexports_all/2.

:- export(includes/2).
:- pred includes(Base, File)
    # "The source @var{Base}.pl includes file @var{File}.  Stored in
       itf file for dependency check.".
:- data includes/2.

:- export(loads/2).
:- pred loads(Base, File)
    # "The source @var{Base}.pl does load_compilation_module of file
       @var{File}.  Stored in itf file for dependency check.".
:- data loads/2.

% Delete itf data for Base, considering opt_suff
% TODO: nicer way?
delete_itf_data_opt(Base) :-
    opt_suff(Opt),
    ( Opt = '' -> % No opt
        true
    ; atom_concat(BaseNoOpt,Opt,Base) -> % Base contains Opt
        delete_itf_data(BaseNoOpt)
    ; atom_concat(Base,Opt,BaseOpt), % Try base with Opt
      delete_itf_data(BaseOpt)
    ),
    delete_itf_data(Base).

% Delete itf data for Base
delete_itf_data(Base) :-
    retractall_fact(defines_module(Base,_)),
    retractall_fact(direct_export(Base,_,_,_,_)),
    retractall_fact(def_multifile(Base,_,_,_)),
    retractall_fact(uses(Base,_)),
    retractall_fact(adds(Base,_)),
    retractall_fact(includes(Base,_)),
    retractall_fact(loads(Base,_)),
    retractall_fact(reexports_from(Base, _)),
    retractall_fact(imports_all(Base,_)),
    retractall_fact(imports_pred(Base,_,_,_,_,_,_)),
    retractall_fact(reexports_all(Base, _)),
    retractall_fact(reexports(Base, _, _, _)),
    retractall_fact(decl(Base,_)).

do_use_module(UsedFile, Imports, Base, Ln0, Ln1) :-
    nonvar(UsedFile),
    ( UsedFile = user -> true
    ; get_base_name(UsedFile, UsedBase, _, _),
      ( UsedBase = Base ->  Ignore = true
      ; current_fact(uses(Base, UsedFile)) -> true
      ; assertz_fact(uses(Base, UsedFile))
      )
    ), !,
    ( nonvar(Ignore) -> true
    ; store_imports(Imports, UsedFile, Base, Ln0, Ln1)
    ).
do_use_module(UsedFile,_Imports,_Base, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_use_module(UsedFile)).

store_imports(all, user,_Base, Ln0, Ln1) :- !,
    compiler_error(Ln0, Ln1, all_user).
store_imports(all, File, Base,_Ln0,_Ln1) :- !,
    assertz_fact(imports_all(Base, File)).
store_imports(Imports, File, Base, Ln0, Ln1) :-
    store_import_list(Imports, File, Base, Ln0, Ln1).

store_import_list([I|Is], File, Base, Ln0, Ln1) :- !,
    store_import(I, File, Base, Ln0, Ln1),
    store_import_list(Is, File, Base, Ln0, Ln1).
store_import_list([], _, _, _, _) :- !.
store_import_list(Bad, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_import_list(Bad)).

store_import(F/A, File, Base, _, _) :-
    atom(F), integer(A), !,
    assertz_fact(imports_expl(Base, File, F, A)).
store_import(Bad, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_import_spec(Bad)).

do_reexport(UsedFile, Preds, Base, Ln0, Ln1) :-
    nonvar(UsedFile),
    get_base_name(UsedFile, _, _, _), !,
    ( current_fact(uses(Base, UsedFile)) -> true
    ; assertz_fact(uses(Base, UsedFile))
    ),
    ( current_fact(reexports_from(Base, UsedFile)) -> true
    ; assertz_fact(reexports_from(Base, UsedFile))
    ),
    store_reexports(Preds, UsedFile, Base, Ln0, Ln1).
do_reexport(UsedFile,_Preds,_Base, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_file(UsedFile)).

store_reexports(all, File, Base,_Ln0,_Ln1) :- !,
    assertz_fact(reexports_all(Base, File)).
store_reexports(Preds, File, Base, Ln0, Ln1) :-
    store_reexport_list(Preds, File, Base, Ln0, Ln1).

store_reexport_list([P|Ps], File, Base, Ln0, Ln1) :- !,
    store_reexport(P, File, Base, Ln0, Ln1),
    store_reexport_list(Ps, File, Base, Ln0, Ln1).
store_reexport_list([], _, _, _, _) :- !.
store_reexport_list(Bad, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_import_list(Bad)).

store_reexport(F/A, File, Base, _, _) :-
    atom(F), integer(A), !,
    assertz_fact(reexports(Base, File, F, A)).
store_reexport(Bad, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_import_spec(Bad)).

% ---------------------------------------------------------------------------
:- doc(section, "Other data coming from read_record_file/4").

% Deleted after file compiled

:- export(clause_of/7).
:- pred clause_of(Base, Head, Body, VarNames, Source, Line0, Line1)
    # "We have read from @var{Base}.pl (or included files) the
       clause @var{Head} :- @var{Body}, which has variable names
       @var{VarNames}, and is located in source @var{Source} (that
       changes if clauses are from an included file) between lines
       @var{Line0} and @var{Line1}.  In the special case that
       @var{Head} is a number, @var{Body} is the body of a
       declaration.".
:- data clause_of/7.

:- export(package/2).
:- pred package(Base, Package).
:- data package/2.

:- export(imports_nocheck/4).
:- pred imports_nocheck(Base, Module, F, A)
    # "The source @var{Base}.pl imports predicate @var{F}/@var{A}
       from module @var{Module} using @decl{import/2}.".
:- data imports_nocheck/4.

:- export(defines_pred/3).
:- pred defines_pred(Base, F, A).
:- data defines_pred/3.

:- pred impl_defines(Base, F, A).
:- data impl_defines/3.

:- export(meta_pred/4).
:- pred meta_pred(Base, F, A, Meta).
:- data meta_pred/4.

:- export(dyn_decl/4).
:- pred dyn_decl(Base, F, A, Decl). % Does not contain multifile preds.
:- data dyn_decl/4.

delete_file_data(Base) :-
    retractall_fact(clause_of(Base,_,_,_,_,_,_)),
    retractall_fact(package(Base,_)),
    retractall_fact(imports_nocheck(Base,_,_,_)),
    retractall_fact(defines_pred(Base,_,_)),
    retractall_fact(impl_defines(Base,_,_)),
    retractall_fact(meta_pred(Base,_,_,_)),
    retractall_fact(dyn_decl(Base,_,_,_)).

do_import(Module, Imports, Base, Ln0, Ln1) :-
    atom(Module), !,
    store_import_nocheck_list(Imports, Module, Base, Ln0, Ln1).
do_import(Module, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_import(Module)).

store_import_nocheck_list([I|Is], Module, Base, Ln0, Ln1) :- !,
    store_import_nocheck(I, Module, Base, Ln0, Ln1),
    store_import_nocheck_list(Is, Module, Base, Ln0, Ln1).
store_import_nocheck_list([], _, _, _, _) :- !.
store_import_nocheck_list(Bad, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_import_list(Bad)).

store_import_nocheck(F/A, Module, Base, _, _) :-
    atom(F), integer(A), !,
    assertz_fact(imports_nocheck(Base, Module, F, A)).
store_import_nocheck(Bad, _, _, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_import_spec(Bad)).

% ---------------------------------------------------------------------------
:- doc(section, "Data used by the assertion library").

:- export(defines/5).
:- pred defines(Base, F, A, DefType, Meta)
    # "The source @var{Base}.pl defines predicate @var{F}/@var{A},
       defined as @var{DefType} (static, implicit, dynamic, data or
       concurrent) and with meta_predicate declaration @var{Meta}
       (which can be 0 if it has not).  Generated by calling
       @pred{comp_defines/1}.".
:- data defines/5.

delete_assrtlib_data :-
    retractall_fact(defines(_,_,_,_,_)).

% ---------------------------------------------------------------------------
:- doc(section, "Interface to itf data (for assrt_lib, ciaopp)").

:- export(comp_defines/1).
:- pred comp_defines(Base)
    # "Can be used in the @tt{TreatP} phase of the compilation
      process to generate facts of @pred{defines/5} for source
      @var{Base}.pl".

comp_defines(Base) :-
    defines_pred(Base, F, A),
      def_type(Base, F, A, DefType),
      (meta_pred(Base, F, A, Meta) -> true ; Meta = 0),
      assertz_fact(defines(Base,F,A,DefType,Meta)),
      fail.
comp_defines(_).

% TODO: Why are those predicates needed? (some names are wrong)

:- export(restore_defines/5).
:- pred restore_defines(Base, F, A, DefType, Meta)
# "Given the same arguments obtained from @pred{defines/5}, it
  generates (assert in c_itf DB) all necessary data to offer itf
  functionality.".

restore_defines(Base, F, A, DefType, Meta) :-
    assertz_fact(defines_pred(Base, F, A)),
    assertz_fact(defines(Base, F, A)),
    restore_def_type(DefType, Base, F, A),
    ( 
        Meta = 0 
    ->
        true
    ;
        assertz_fact(meta_pred(Base, F, A, Meta))
    ).

restore_def_type(implicit, Base, F, A) :-
    !,
    assertz_fact(impl_defines(Base, F, A)).
restore_def_type(static, _Base, _F, _A) :-
    !.
restore_def_type(DefType, Base, F, A) :-
    assertz_fact(dyn_decl(Base, F, A, DefType)).

:- export(restore_imports/5).
:- pred restore_imports(M, IM, F, A, EndMod)
# "Given the same arguments obtained from @pred{imports/5}, it
  restores c_itf imports DB.".

restore_imports(M, IM, F, A, EndMod) :-
    assertz_fact(imports(M, IM, F, A, EndMod)).

:- export(restore_multifile/4).
:- pred restore_multifile(M, IM, F, Def)
# "Given the same arguments obtained from @pred{multifile/4}, it
  restores c_itf multifile DB.".

restore_multifile(M, F, A, Def) :-
    assertz_fact(multifile(M, F, A, Def)).

% ---------------------------------------------------------------------------
:- doc(section, "Data for mexpand (module expansion)").
% Data used by engine(mexpand) and the .po compiler

:- export(imports/5).
:- pred imports(Mod, Mod2, F, A, EndMod)
    # "Module @var{Mod} imports from module @var{Mod2} predicate
       @var{F}/@var{A}, which resides in module @var{EndMod}.
       @var{EndMod} can be different from @var{Mod2} due to
       reexportation.".
:- data imports/5.

:- export(meta_args/2).
:- pred meta_args(Mod, Meta)
    # "Module @var{Mod} has meta_predicate declaration @var{Meta}.".
:- data meta_args/2.

:- export(multifile/3).
:- pred multifile(Mod, F, A, DynType)
    # "Module @var{Mod} defines multifile predicate @var{F}/@var{A}
       defined as @var{DynType}.".
:- data multifile/4.

:- export(defines/3).
:- pred defines(Mod, F, A)
    # "Module @var{Mod} defines predicate @var{F}/@var{A}.".
:- data defines/3.

:- pred redefining(Module, F, A).
:- data redefining/3.

delete_module_data(M) :-
    % JFMC & EMM
    ( current_prolog_flag(keep_assertions, yes) ->
        true
    ; retractall_fact(assertion_read(_, M, _, _, _, _, _, _, _))
    ),
    %
    retractall_fact(defines(M,_,_)),
    retractall_fact(multifile(M,_,_,_)),
    retractall_fact(meta_args(M,_)),
    retractall_fact(imports(M,_,_,_,_)),
    retractall_fact(redefining(M, _, _)).

% ---------------------------------------------------------------------------
:- doc(section, "Temporary data for read_record_file").
% (Deleted after reading file)

:- pred new_decl(Base, Pred, In_itf)
    # "The source @var{Base}.pl has defined the declaration @var{Pred}
       with a new_declaration directive, @var{In_itf} is 'on' if the
       declaration is to be included in the .itf file, 'off' otherwise.".
:- data new_decl/3.

:- pred undo_decl(Base, Goal, UndoGoal)
    # "@var{Goal} which have been done while reading @var{Base}.pl is
       undone with @var{UndoGoal}.".
:- meta_predicate undo_decl(_, goal, goal).
:- data undo_decl/3.

:- export(discontiguous/3).
:- pred discontiguous(F, A, Base).
:- data discontiguous/3.

:- pred reading_pred(F, A, Base).
:- data reading_pred/3.

:- pred pred_read(F, A, Base).
:- data pred_read/3.

clean_read_record_data(Base) :-
    undo_decls(Base),
    retractall_fact(discontiguous(_, _, Base)),
    retractall_fact(reading_pred(_, _, Base)),
    retractall_fact(pred_read(_, _, Base)),
    retractall_fact(new_decl(Base, _, _)),
    retractall_fact(undo_decl(Base, _, _)).

% ---------------------------------------------------------------------------

% Deleted while generating itf file

:- pred imports_expl(Base, ImpFile, F, A). % Translates to imports_pred
:- data imports_expl/4.

:- pred expansion_check(Base, Pred)
    # "@var{Pred} will be executed before file @var{Base}.pl is compiled
       to make additional checks, with @var{Base} as its first argument.".
:- data expansion_check/2.

% These are deleted when computing imports_pred/7
delete_aux_data(Base) :-
    retractall_fact(imports_expl(Base, _, _, _)),
    retractall_fact(expansion_check(Base, _)).

% ---------------------------------------------------------------------------
:- doc(section, "Predicates for code expanders").

:- export(add_module_check/1).
:- meta_predicate add_module_check(pred(1)).
:- pred add_module_check(Pred)
    # "Used by code expanders (loaded with
       @decl{load_compilation_module/1} declarations) to provide
       additional checks to be executed before the current file is
       compiled.  The predicate is called with the base name of the file
       processed as the first argument, and all solutions will be found.
       A fact @pred{module_error/0} can be asserted if a condition
       which should stop the compilation is found.".

add_module_check(Pred) :-
    current_fact(reading_from(Base)), !,
    asserta_fact(expansion_check(Base, Pred)).

:- export(ensure_imported/4).
ensure_imported(Base, Module, F, A) :-
    imports_pred(Base, File, F, A, _, _, _),
    file_defines_module(File, Module), !.
ensure_imported(_, Module, F, A) :-
    error_in_lns(_,_,error,
                 ['this module should import ',~~(F/A),' from ',Module]),
    asserta_fact(module_error).

% ---------------------------------------------------------------------------
:- doc(section, "Default operators").

:- export(define_ops/0).
define_ops :-
    op(1150,  fx, [% (public),  % compatibility
                   % (mode),    % compatibility
                   % (dynamic), % In dynamic.pl package
                   % (concurrent), (data), % In datafacts.pl package
                   (multifile),
                   (meta_predicate),
                   (discontiguous)]).

:- initialization(define_ops).

% ---------------------------------------------------------------------------
:- doc(section, "Process source files and dependencies incrementally").

:- pred process_too(Mode, Base).
:- data process_too/2. % From use_module/ensure_loaded

:- export(processed/2).
:- pred processed(Base, Mode).
:- data processed/2.

:- pred status(Base, Status).
:- data status/2.

:- pred already_have_itf(Base, ItfLevel)
    # "The itf file of source @var{Base}.pl was already read or generated
       in this compilation.".
:- data already_have_itf/2.

:- pred time_of_itf_data(Base, Time, ItfLevel)
    # "The itf file of source @var{Base}.pl was read at time @var{Time}.".
:- data time_of_itf_data/3.

delete_time_of_itf_data(Base) :-
    retractall_fact(time_of_itf_data(Base, _, _)).

delete_process_file_data :-
    retractall_fact(status(_,_)),
    retractall_fact(process_too(_,_)),
    retractall_fact(processed(_,_)),
    retractall_fact(already_have_itf(_,_)).

% TODO: generalize for compilation contexts? rename Mode to something else
:- data in_mode/1.
in_mode(in).

new_in_mode(NIn) :-
    in_mode(In), !,
    atom_concat(In, $, NIn),
    asserta_fact(in_mode(NIn)).

del_in_mode(In) :-
    retract_fact(in_mode(In)).

:- export(process_files_from/7).
% process_files_from(File, Mode{in,in$,...,po,...}, Type{module,any},
%                    TreatP, StopP, SkipP, RedoP)
:- meta_predicate process_files_from(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_files_from(Files, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
    cleanup_c_itf_data,
    process_files_from_all(Files, Mode, Type, TreatP, StopP, SkipP, RedoP).

:- meta_predicate process_files_from_all(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_files_from_all([],_Mode,_Type,_TreatP,_StopP,_SkipP,_RedoP) :- !.
process_files_from_all([F|Fs], Mode, Type, TreatP, StopP, SkipP, RedoP) :- !,
    process_files_from_(F, Mode, Type, TreatP, StopP, SkipP, RedoP),
    process_files_from_all(Fs, Mode, Type, TreatP, StopP, SkipP, RedoP).
process_files_from_all(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
    process_files_from_(File, Mode, Type, TreatP, StopP, SkipP, RedoP).

:- export(process_file/7).
:- meta_predicate process_file(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_file(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
    cleanup_c_itf_data,
    get_base_name(File, Base, Pl, Dir),
    process_file_(Base, Pl, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP).

:- meta_predicate process_files_from_(+, +, +, pred(1), pred(1), pred(1), pred(1)).

process_files_from_(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
    get_base_name(File, Base, Pl, Dir),
    process_file_(Base, Pl, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP),
    process_remaining_files(Mode, TreatP, StopP, SkipP, RedoP).

:- meta_predicate process_remaining_files(+, pred(1), pred(1), pred(1), pred(1)).

process_remaining_files(Mode, TreatP, StopP, SkipP, RedoP) :-
    retract_fact(process_too(Mode, Base)), % Coming from ensure_loaded
    \+ current_fact(processed(Base, Mode)), !,
      file_data(Base, Pl, Dir),
      process_file_(Base, Pl, Dir, Mode, any, TreatP, StopP, SkipP, RedoP),
      process_remaining_files(Mode, TreatP, StopP, SkipP, RedoP).
process_remaining_files(_, _, _, _, _).

:- meta_predicate process_file_(+, +, +, +, +, pred(1), pred(1), pred(1), pred(1)).

process_file_(Base, PlName, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
    check_loop(Base, Check),
    ( retract_fact(status(Base, Status)) -> true % already viewed
    ; Check = static -> 
        % TODO: this may not work for all TreatP
        Status = static_mod
    ; new_file_status(1, Base, PlName, Dir, Type, Status)
    ),
    get_treat_action(Base, Status, Mode, StopP, SkipP, RedoP, Action),
    do_treat_action(Action, Base, PlName, Dir, Type, TreatP, NewStatus),
    asserta_fact(status(Base, NewStatus)),
    asserta_fact(processed(Base, Mode)).

check_loop(Base, Check) :-
    reading_from(Base),
    !,
    ( % TODO: make sure that this is a correct backport from optim_comp
      % TODO: at least warn if a changes in a compilation_module are being ignored
      % This prevents COMPILER LOOP error if the module is statically linked -- EMM
      static_base(Base) ->
        Check = static
%           , message(note, ['Using static library ', Base,
%                          '.pl instead of the one in sources.'])
    ; Check = loop,
      findall(M, (reading_from(B), defines_module(B, M)), Ms0),
      reverse(Ms0, Ms),
      message(error, ['COMPILER LOOP: ', Base,
              '.pl uses for compilation a file which ',
              'depends on it: ', Ms, ' -- aborting']),
      throw(compiler_loop)
    ).
check_loop(_, ok).

new_file_status(ItfLevel, Base, PlName, Dir, Type, Status) :-
    itf_filename(Base, ItfName),
    modif_time0(ItfName, ItfTime),
    modif_time0(PlName, PlTime),
    ( ItfTime >= PlTime,
      read_itf(ItfLevel, ItfName, ItfTime, Base, Dir, Type) ->
        Status = itf_read(ItfName,ItfTime)
    ; read_record_file(PlName, Base, Dir, Type),
      assertz_fact(already_have_itf(Base,1)),
      Status = file_read(ItfName)
    ).

:- export(old_file_extension/2).
% TODO: JF: extension is not enough... 
% TODO: improve name
old_file_extension(Base, Ext) :-
    ( filetype(Type, Ext, _) ->
        product_filename(Type, Base, ExtName)
    ; throw(unknown_filetype(Ext))
    ),
    itf_filename(Base, ItfName),
    modif_time0(ExtName, ExtTime),
    modif_time0(ItfName, ItfTime),
    ExtTime < ItfTime.

read_itf(ItfLevel, ItfName, ItfTime, Base, Dir, Type) :-
    ( current_fact(time_of_itf_data(Base, ItfDataTime, ItfLevel0)),
      ItfDataTime >= ItfTime ->
        ( ItfLevel0 < ItfLevel -> % needs reload
            do_read_itf(ItfLevel, ItfName, Base, Dir, prev)
        ; % (no reload, just refresh base names)
          base_names_in_itf(ItfName, Base, Dir)
        )
    ; do_read_itf(ItfLevel, ItfName, Base, Dir, now)
    ),
    assertz_fact(already_have_itf(Base,ItfLevel)),
    defines_module(Base, M),
    ( M = user(_), Type == module ->
        warning_module_missing(_, _)
    ; true
    ),
    end_doing.

% Obtain treat action for process file
:- meta_predicate get_treat_action(+, +, +, pred(1), pred(1), pred(1), ?).
get_treat_action(Base, Status, Mode, StopP, SkipP, RedoP, Action) :-
    ( StopP(Base) ->
        do_not_treat_action(Status, Action)
    ; handle_related_files(Base, Mode),
      ( SkipP(Base) ->
          do_not_treat_action(Status, Action)
      ; ( Status = file_read(ItfName) ->
            Action = action_treat_file(noread, gen_itf, ItfName)
        ; ( Status = file_noclauses(ItfName)
          ; Status = itf_read(ItfName,ItfTime),
            changed_dependencies(Base, ItfTime)
          ) ->
            Action = action_treat_file(read_record, gen_itf, ItfName)
        ; RedoP(Base) -> % Do not regenerate .itf
            Action = action_treat_file(read_record, nogen, ItfName)
        ; Action = none
        )
      )
    ).

do_not_treat_action(Status, Action) :-
    ( Status = file_read(ItfName) ->
        Action = noclauses(ItfName)
    ; Action = old_status(Status)
    ).

handle_related_files(Base, Mode):-
    uses_file(Base, File),
      base_name(File, BFile),
      \+ current_fact(processed(BFile, Mode)),
        get_file_deps(BFile, module),
        asserta_fact(process_too(Mode, BFile)),
        follow_reexports(BFile,[BFile]),
    fail.
handle_related_files(Base, Mode):-
    adds(Base, File),
      base_name(File, BFile),
      \+ current_fact(processed(BFile, Mode)),
        get_file_deps(BFile, any), % NOTE: needed for compile_ldlibs/3
        asserta_fact(process_too(Mode, BFile)),
    fail.
handle_related_files(_, _).

follow_reexports(Base,Covered) :-
    reexports_from(Base, File), base_name(File, BFile),
      check_reexportation_loop(BFile, Covered),
      get_file_deps(BFile, module),
      follow_reexports(BFile,[BFile|Covered]),
    fail.
follow_reexports(_, _).

check_reexportation_loop(BFile, Covered) :-
    ( member(BFile, Covered) ->
        reverse([BFile|Covered], Loop),
        message(error,  ['Reexportation loop: ',
                         Loop,' -- aborting']),
        throw(compiler_loop)
    ; true
    ).

% (ensure that ItfLevel=1 info is loaded)
itf_handle_related_files(Base):-
    uses_file(Base, File),
      base_name(File, BFile),
        get_file_itf(1, BFile, module),
        itf_follow_reexports(BFile),
    fail.
itf_handle_related_files(Base):-
    adds(Base, File),
      base_name(File, BFile),
        get_file_itf(1, BFile, any),
    fail.
itf_handle_related_files(_).

% (assumes no loops)
itf_follow_reexports(Base) :-
    reexports_from(Base, File), base_name(File, BFile),
      get_file_itf(1, BFile, module),
      itf_follow_reexports(BFile),
    fail.
itf_follow_reexports(_).

:- if(defined(itf_sections)).
get_file_deps(Base, Type) :-
    get_file_itf(0, Base, Type).
:- else.
get_file_deps(Base, Type) :-
    get_file_itf(1, Base, Type). % load full itf
:- endif.

get_file_itf(ItfLevel, Base, _Type) :-
    already_have_itf(Base, ItfLevel0), !,
    ( ItfLevel0 < ItfLevel -> % needs reload
        file_data(Base, _PlName, Dir),
        itf_filename(Base, ItfName),
        retractall_fact(already_have_itf(Base,_)),
        do_read_itf(ItfLevel, ItfName, Base, Dir, prev),
        assertz_fact(already_have_itf(Base,ItfLevel))
    ; true
    ).
get_file_itf(ItfLevel, Base, Type) :-
    file_data(Base, PlName, Dir),
    new_file_status(ItfLevel, Base, PlName, Dir, Type, Status),
    asserta_fact(status(Base, Status)).

:- meta_predicate do_treat_action(+, +, +, +, +, pred(1), ?).
do_treat_action(Action, Base, PlName, Dir, Type, TreatP, NewStatus) :-
    ( Action = old_status(Status) ->
        NewStatus = Status
    ; Action = none ->
        NewStatus = itf_ok
    ; Action = noclauses(ItfName) ->
        delete_aux_data(Base),
        delete_file_data(Base),
        NewStatus = file_noclauses(ItfName)
    ; Action = action_treat_file(ReadRecord, GenerateItf, ItfName) ->
        itf_handle_related_files(Base),
        ( ReadRecord = noread -> % read_record_file/4 was already done
            true
        ; read_record_file(PlName, Base, Dir, Type), % TODO: Type was 'any'
          assertz_fact(already_have_itf(Base,1))
        ),
        ( check_itf_data(Base, PlName), % fails if incorrect imports/exports
          gen_itf(GenerateItf, Base, PlName, ItfName) ->
            treat_file(Base, TreatP)
        ; true % (note: errors state elsewhere)
        ),
        delete_file_data(Base),
        NewStatus = itf_ok
    ; fail
    ).

gen_itf(nogen, _Base, _PlName, _ItfName).
gen_itf(gen_itf, Base, PlName, ItfName) :-
    ( fmode(PlName, Mode), % TODO: rename Mode in process_file (this is a file mode!)
      generate_itf(ItfName, Mode, Base) ->
        true
    ; fail
    ).

:- meta_predicate treat_file(+, pred(1)).
treat_file(Base, TreatP) :-
    defines_module(Base, M),
    generate_module_data(Base, M),
    ( TreatP(Base) -> true
    ; message(warning, ['Treatment of ',Base,'(.pl) failed (goal: ', ~~(TreatP), ')'])
    ),
    delete_module_data(M).

% ---------------------------------------------------------------------------
:- doc(section, "Cleanup c_itf_data").

:- export(cleanup_c_itf_data/0).
cleanup_c_itf_data :-
    delete_aux_data(_),
    delete_file_data(_),
    %
    clean_read_record_data(_),
    delete_module_data(_),
    delete_assrtlib_data,
    delete_base_data,
    delete_process_file_data,
    delete_read_record_data.

% (FS mapping)
delete_base_data :-
    retractall_fact(base_name_(_,_,_)),
    retractall_fact(file_data(_,_,_)).

:- export(cleanup_itf_cache/0).
cleanup_itf_cache :-
    delete_time_of_itf_data(_),
    delete_itf_data(_).

% ---------------------------------------------------------------------------
:- doc(section, "Mapping between files and module names").

:- export(base_name/2).
base_name(File, Base) :- var(File), !, base_name_(_, File, Base).
base_name(File, Base) :-
    file_key(File, Key),
    base_name_(Key, File, Base).

% (possibly non-unique key to speedup lookups)
% recursively visit last argument until we reach an atom (e.g., library(a/b/c)) -> c)
file_key(File, K) :- atom(File), !, K = File.
file_key(File, K) :-
    functor(File, _, A),
    arg(A, File, File0),
    file_key(File0, K).

:- data base_name_/3.

:- export(file_data/3).
:- data file_data/3.

get_base_name(File, Base, PlName, Dir) :-
    base_name(File, Base), !,
    file_data(Base, PlName, Dir).
get_base_name(File, Base, PlName, Dir) :-
    compute_base_name(File, Base, PlName, Dir),
    file_key(File, Key),
    asserta_fact(base_name_(Key, File, Base)),
    ( current_fact(file_data(Base, _, _)) ->
        true
    ;
        asserta_fact(file_data(Base, PlName, Dir))
    ).

:- export(compute_base_name/4).
compute_base_name(File, Base, PlName, Dir) :-
    prolog_flag(fileerrors, OldFE, off), % (fail on file errors)
    ( functor(File, _, N), N =< 1,
      find_pl_filename(File, PlName, Base, Dir),
      file_exists(PlName) ->
        OK = yes
    ; OK = no
    ),
    set_prolog_flag(fileerrors, OldFE),
    ( OK = yes -> true
    ; throw(error(existence_error(source_sink,File),
                  absolute_file_name/7-1))
    ).

file_defines_module(user, user) :- !.
file_defines_module(File, Module) :-
    base_name(File, BFile),
    defines_module(BFile, Module).

:- export(module_from_base/2).
module_from_base(B, M) :-
    opt_suff(Opt),
    (atom_concat(BB,Opt,B), ! ; BB = B),
    path_basename(BB, M).

% ---------------------------------------------------------------------------
:- doc(section, "Read source and generate itf (fill read_record_file data)").

:- export(exports_pred/3).
:- pred exports_pred(Base, F, A). % Translates to direct_export/5
:- data exports_pred/3.

:- pred multifile_pred(Base, F, A). % Translates to def_multifile/4
:- data multifile_pred/3.

:- pred reading_from(Base).
:- data reading_from/1.

:- export(module_error/0).
:- data module_error/0.

:- export(module_error/1).
:- data module_error/1.

:- data syntax_error_in/1.

% (temporary for read_record_file)
delete_read_record_data :-
    retractall_fact(reading_from(_)),
    retractall_fact(exports_pred(_,_,_)),
    retractall_fact(multifile_pred(_,_,_)),
    retractall_fact(module_error),
    retractall_fact(module_error(_)),
    retractall_fact(syntax_error_in(_)).

read_record_file(PlName, Base, Dir, Type) :-
    delete_time_of_itf_data(Base),
    delete_itf_data_opt(Base),
    working_directory(OldDir, Dir),
    now_doing(['Reading ',PlName]),
    asserta_fact(reading_from(Base)),
    read_record_file_(PlName, Base, Type),
    retract_fact(reading_from(Base)),
    gen_exports(Base),
    gen_def_multifile(Base),
    end_doing,
    clean_read_record_data(Base),
    working_directory(_, OldDir).

read_record_file_(PlName, Base, Type) :-
    '$open'(PlName, r, Stream),
    skip_shell_lines(Stream),
    ( read_sentence(Stream, Base, Sentence) -> true ; fail ), % (once)
    expand_module_decl(Sentence, Base, Type, module(Module, Exports, Packages), Rest),
    (record_module_decl(Sentence, Module, Base, PlName) -> true ; true),
    (Sentence = sentence(_, _, _, Ln0, Ln1) -> true ; Ln0 = 1, Ln1 = 1),
    check_define_module(Base, Module, Ln0, Ln1),
    ( Module=user(_), Type==module ->
        warning_module_missing(Ln0, Ln1)
    ; true
    ),
    ( nonvar(Packages), member(Package, Packages), no_prelude(Package) -> 
        true
    ; do_use_package(Base, Module, Ln0, Ln1, prelude)
    ),
    assert_export_list(Exports, Base, Ln0, Ln1),
    %
    glbmod_collect_packages(Module, Packages, Packages2),
    process_packages(Base, Module, Ln0, Ln1, Packages2),
    %
    ( member(Sentence2, Rest) ; read_sentence(Stream, Base, Sentence2) ),
    ( Sentence2 = end_of_file(Ln0b, Ln1b) ->
        process_end_of_file(Base, PlName, Module, Ln0b, Ln1b)
    ; process_sentence(Sentence2, Base, PlName, Module),
      fail
    ),
    !,
    %log_translations(Base, Module, term),
    %log_translations(Base, Module, sentence),
    assert_dyn_decl(Base, '$current_module', 1, dynamic, 1, 1),
    assert_dyn_decl(Base, '$meta_args', 2, dynamic, 1, 1),
    assert_dyn_decl(Base, '$imports', 5, dynamic, 1, 1),
    close(Stream).

% Packages that prevent the inclusion of the prelude
no_prelude(pure).
no_prelude(noprelude).

record_module_decl(sentence((:- module(Module, Exports)), VNs, _, Ln0, Ln1), Module, Base, Pl) :-
    assertz_fact(clause_of(Base, 1, module(_, Exports), VNs, Pl, Ln0, Ln1)).
record_module_decl(sentence((:- module(Module, Exports, Packages)), VNs, _, Ln0, Ln1), Module, Base, Pl) :-
    assertz_fact(clause_of(Base, 1, module(_, Exports, Packages), VNs, Pl, Ln0, Ln1)).

process_packages(Base, Module, Ln0, Ln1, Packages) :-
    maplist(do_use_package(Base, Module, Ln0, Ln1), Packages).

process_sentence(Sentence, Base, Pl, Module) :-
    Sentence = sentence(RawData, VNs, Sings, Ln0, Ln1),
    asserta_fact(location(Pl, Ln0, Ln1), Ref),
    ( expand_term_to_list(RawData, Module, VNs, DataList) ->
        process_expanded_data_list(DataList, Base, Module, VNs, Sings,
          Pl, Ln0, Ln1)
    ; true
    ),
    erase(Ref).

process_end_of_file(Base, Pl, Module, Ln0, Ln1) :-
    asserta_fact(location(Pl, Ln0, Ln1), Ref),
    ( expand_term_to_list(end_of_file, Module, [], Data0),
      append(DataList, [end_of_file], Data0) ->
        process_expanded_data_list(DataList, Base, Module, [], [],
          Pl, Ln0, Ln1)
    ; true
    ),
    erase(Ref).

process_expanded_data_list(Data0, Base, M, VNs, Sings, Pl, Ln0, Ln1) :-
    ( member(Data, Data0),
      process_expanded_data(Data, Base, M, VNs, Sings, Pl, Ln0, Ln1),
      fail
    ; true
    ).

:- export(expand_module_decl/5).
expand_module_decl(Sentence, Base, _Type, Decl2, Rest) :-
    ( Sentence = sentence((:- Decl), _, _, _, _),
      normalize_module_decl(Decl, Base, Decl2) ->
        Rest = []
    ; Sentence = sentence((:- Decl), _, _, _, _),
      Decl = use_package(Packages) ->
        user_module_decl(Base, Packages, Decl2),
        Rest = []
    ; default_package(DefaultPackage),
      user_module_decl(Base, DefaultPackage, Decl2),
      Rest = [Sentence]
    ).

normalize_module_decl(Decl, _, Decl) :- Decl = module(_, _, _), !.
normalize_module_decl(module(Module, Exports), _,
        module(Module, Exports, Package)) :- !,
    default_package(Package).
% Unknown firts declaration may include package
normalize_module_decl(PackageDecl, Base, module(Module, Exports, Package)) :-
    \+ is_known_decl(PackageDecl, Base),
    functor(PackageDecl, Package0, _),
    catch(get_base_name(library(Package0), _, _, _),_,fail), !,
    ( arg(1, PackageDecl, Module) -> true ; true ),
    ( arg(2, PackageDecl, Exports) ->
        true
    ; Exports = []
    ),
    ( arg(3, PackageDecl, MorePackages) ->
        Package = [Package0|MorePackages]
    ; Package = Package0
    ).

% Module is user(_) if has not module declaration
user_module_decl(Base, Packages, module(user(Base), [], Packages)).

expand_term_to_list(Data0, M, VNs, Data) :-
    do_expand_term(Data0, M, VNs, Data1),
    expand_list_tail(Data1, Data).

expand_list_tail(Data1, Data) :-
    ( var(Data1) ->
        Data = []
    ; Data1 = [X|Data3] ->
        Data = [X|Data2],
        expand_list_tail(Data3, Data2)
    ; Data1 = [] ->
        Data = Data1
    ; Data = [Data1]
    ).

:- use_module(library(compiler/frontend_condcomp), [
    condcomp_sentence/3,
    condcomp_split_doccomment/3,
    add_condcomp_fact/2
]).

do_expand_term(Data0, M, VNs, Data) :-
    % Update conditional compilation state and filter sentence
    % TODO: merge with compiler_oc
    ( condcomp_split_doccomment(Data0, Data0c, Data0d) ->
        % treat the doccomment, then directive (see condcomp_split_doccomment/3)
        maybe_expand_term(Data0c, M, VNs, Data),
        maybe_expand_term(Data0d, M, VNs, _) % (output must be [] in this case)
    ; maybe_expand_term(Data0, M, VNs, Data)
    ).

maybe_expand_term(Data0, M, VNs, Data) :-
    ( condcomp_sentence(Data0, Data1, M), Data1 = [] -> Data = []
    ; expand_term(Data0, M, VNs, Data)
    ).

% ---------------------------------------------------------------------------
:- doc(section, "'export' declaration").

assert_export_list(All, Base, _Ln0,_Ln1) :-
    var(All), !,
    assertz_fact(exports_pred(Base, all, all)).
assert_export_list([Exp|Exports], Base, Ln0, Ln1) :- !,
    assert_export(Exp, Base, Ln0, Ln1),
    assert_export_list(Exports, Base, Ln0, Ln1).
assert_export_list([],_Base,_Ln0,_Ln1) :- !.
assert_export_list(Exp, Base, Ln0, Ln1) :-
    assert_export(Exp, Base, Ln0, Ln1).

assert_export(F/A, Base,_Ln0,_Ln1) :-
    atom(F), integer(A), !,
    assertz_fact(exports_pred(Base, F, A)).
assert_export(Spec,_Base, Ln0, Ln1) :-
    compiler_error(Ln0, Ln1, bad_export(Spec)).

% ---------------------------------------------------------------------------
:- doc(section, "Support for declaration contexts").

undo_decls(Base) :-
    current_fact(undo_decl(Base, _, UndoGoal)),
      call(UndoGoal),
    fail.
undo_decls(_).

redo_decls(Base) :-
    findall(Goal, undo_decl(Base,Goal,_), Gs),
    call_list_rev(Gs).

call_list_rev([]).
call_list_rev([G|Gs]) :-
    call_list_rev(Gs),
    call(G).

% ---------------------------------------------------------------------------
:- doc(section, "Treat a sentence").

:- export(read_sentence/3).
read_sentence(Stream, Base, Sentence) :-
    repeat,
    Opts = [ variable_names(VarNames),
             singletons(Singletons),
             lines(Ln0, Ln1) ],
    catch(read_term(Stream, Data, Opts),
          error(syntax_error([L0,L1,Msg,ErrorLoc]), _),
          handle_syntax_error(Base,L0,L1,Msg,ErrorLoc)),
    % !,
    ( Data = end_of_file ->
        Sentence = end_of_file(Ln0, Ln1)
    ; Sentence = sentence(Data, VarNames, Singletons, Ln0, Ln1)
    ).

handle_syntax_error(Base,L0,L1,Msg,ErrorLoc) :-
    assertz_fact(syntax_error_in(Base)),
    error_in_lns(L0, L1, error, ['syntax error: ',[](Msg),'\n'| ErrorLoc]),
    fail.

% ---------------------------------------------------------------------------
:- doc(section, "Default package").

% WARNING: Use only a single package, in the form pathalias(packagefile)
:- export(default_package/1).
:- data default_package/1.
default_package(library(default)).

% ---------------------------------------------------------------------------
:- doc(section, "Check module name").

check_define_module(Base, M, Ln0, Ln1) :-
    module_from_base(Base, SM),
    ( SM = M -> % Allow vars in module declarations
        check_other_defines(Base, M, Ln0, Ln1)
    ; M = user(_) ->
        true
    ; compiler_error(Ln0, Ln1, bad_module(Base, M))
    ),
    assertz_fact(defines_module(Base, M)).

check_other_defines(Base, M, Ln0, Ln1) :-
    defines_module(OtherFile, M), !,
    compiler_error(Ln0, Ln1, module_redefined(OtherFile, Base, M)).
check_other_defines(_, _, _, _).

% ---------------------------------------------------------------------------
:- doc(section, "Treat sentence after expansion (term, sentence)").

% :- pred read_assertion/6 + not_fails.
read_assertion(Assr, M, S, LB, LE, Dict) :-
    ( normalize_assertion(M, Assr, Pred, Status, T, B, S, LB, LE) ->
        assertz_fact(assertion_read(Pred, M, Status, T, B, Dict, S, LB, LE))
    ; true % was not an assertion
    ).

% note: this predicate handles code before module expansion
process_expanded_data((?- Goal), _, _, _, _, _, _, _) :- !,
    call(Goal), !. % TODO: Deprecate (or make it optional with a flag)
process_expanded_data((:- Decl), Base, M, VNs,_Sings, Pl, Ln0, Ln1) :- !,
    process_decl(Decl, Base, M, VNs, Ln0, Ln1),
    ( current_prolog_flag(read_assertions, yes), atom(M) ->
        read_assertion(Decl, M, Pl, Ln0, Ln1, VNs)
    ; true
    ),
    assertz_fact(clause_of(Base, 1, Decl, VNs, Pl, Ln0, Ln1)).
process_expanded_data((H :- B), Base, _M, VNs, Sings, Pl, Ln0, Ln1) :- !,
    nonvar(H),
    callable(H),
    functor(H, F, A),
    ( atom(F) -> true
    ; error_in_lns(Ln0, Ln1, error, ['illegal clause']), fail
    ),
    ( wellformed_body(B, +, B1) -> true
    ; error_in_lns(Ln0, Ln1, error, ['malformed body in ',''(F/A)]), fail
    ),
    defined_in_source(Base, F, A),
    clause_check(F, A, Base, Ln0, Ln1),
    singleton_check(Sings, F, A, Ln0, Ln1),
    assertz_fact(clause_of(Base, H, B1, VNs, Pl, Ln0, Ln1)),
    ( dyn_decl(Base, F, A, dynamic) ->
        dynamic_handling(Base, F, A, H, B1, VNs, Pl, Ln0, Ln1)
    ; true
    ).
process_expanded_data(C, _, _, _, _, _, Ln0, Ln1) :- 
    construct(C), !,
    functor(C, F, A),
    error_in_lns(Ln0, Ln1, error, ['attempt to redefine ',''(F/A)]).
process_expanded_data(F, Base, M, VNs, Sings, Pl, Ln0, Ln1) :-
    process_expanded_data((F:-true), Base, M, VNs, Sings, Pl, Ln0, Ln1).

defined_in_source(Base, F, A) :-
    multifile_pred(Base, F, A), !.
defined_in_source(Base, F, A) :-
    defines_pred(Base, F, A), !.
defined_in_source(Base, F, A) :-
    assertz_fact(defines_pred(Base,F,A)).

% ---------------------------------------------------------------------------
:- doc(section, "Support for dynamic_clauses package").

% TODO:[JF] it was added for all dynamic multifile predicates even if the
%   module is not using dynamic_handling; show error/warning instead?

dynamic_handling(Base, F, A, H, B, VNs, Pl, Ln0, Ln1) :-
    new_decl(Base, dynamic_handling, _), !,
    % Add clause with '-\3\clause' suffix
    functor(H, F, A),
    atom_concat(F, '-\3\clause', F1),
    functor(H1, F1, A),
    copy_args(A, H, H1),
    %
    ( multifile_pred(Base, F, A) -> add_multifile_pred(Base, F1, A) ; true ),
    assert_dyn_decl(Base, F1, A, data, Ln0, Ln1), % TODO:[JF] actually 'dynamic'
    %
    assertz_fact(clause_of(Base, '\3\rawbody'(H1), B, VNs, Pl, Ln0, Ln1)).
dynamic_handling(_Base,_F,_A,_H,_B,_VNs,_Pl,_Ln0,_Ln1).

% ---------------------------------------------------------------------------
:- doc(section, "Treat declarations").

% NOTE: this predicate handles code before module expansion
construct(true).
construct((_ , _)).
construct((_ ; _)).
construct((_ -> _)).
construct((\+ _)).
construct(if(_, _, _)).
construct((_ ^ _)).

:- discontiguous(decl__treatDom/1).
:- discontiguous(decl__treat/6).

is_known_decl(D, _) :- decl__treatDom(D), !.
is_known_decl(D, Base) :- new_decl(Base, D, _), !.

process_decl(D, Base, M, VNs, Ln0, Ln1) :- decl__treatDom(D), !,
    ( decl__treat(D, Base, M, VNs, Ln0, Ln1) -> true
    ; error_in_lns(Ln0, Ln1, error, ['declaration processing failed ', ~~(D)])
    ).
process_decl(D, Base,_M,_VNs,_Ln0,_Ln1) :-
    % User-defined declarations
    new_decl(Base, D, ITF), !,
    ( ITF = on -> assertz_fact(decl(Base, D)) ; true).
process_decl(D, _Base,_M,_VNs, Ln0, Ln1) :-
    error_in_lns(Ln0, Ln1, error, ['unknown declaration ',~~(D)]).

warning_failed_decl(Ln0, Ln1, Decl) :-
    error_in_lns(Ln0, Ln1, warning, [Decl,' - declaration failed']).

bad_spec_error(Decl, Ln0, Ln1, Spec) :-
    compiler_error(Ln0, Ln1, badly_formed(Decl,Spec)).

% ---------------------------------------------------------------------------
:- doc(section, "Language definitions for the front-end").

:- include(library(compiler/frontend_core)).
:- include(library(compiler/clause_check)).

% ---------------------------------------------------------------------------
:- doc(section, "Generate exports and multifile data").

gen_exports(Base) :-
    exports_pred(Base, all, all), !,
    retractall_fact(exports_pred(Base, _, _)),
    ( defines_pred(Base, F, A),
        gen_export(Base, F, A),
      fail
    ; true
    ).
gen_exports(Base) :-
    retract_fact(exports_pred(Base, F, A)),
      ( multifile_pred(Base, F, A) ->
          error_in_lns(_,_,warning,
                       ['no need to export multifile predicate ',~~(F/A)])
      ; gen_export(Base, F, A)
      ),
    fail.
gen_exports(_Base).

gen_export(Base, F, A) :-
    def_type(Base, F, A, DefType),
    (meta_pred(Base, F, A, Meta) -> true ; Meta = 0),
    assertz_fact(direct_export(Base,F,A,DefType,Meta)).

def_type(Base, F, A, DefType) :-
    ( dyn_decl(Base, F, A, DefType) -> true
    ; impl_defines(Base, F, A) -> DefType = implicit
    ; DefType = static
    ).

gen_def_multifile(Base) :-
    retract_fact(multifile_pred(Base, F, A)),
      ( retract_fact(dyn_decl(Base,F,A,DynType)) -> true
      ; DynType = static
      ),
      assertz_fact(def_multifile(Base,F,A,DynType)),
    fail.
gen_def_multifile(_).

% ---------------------------------------------------------------------------
:- doc(section, "Check and complete module interface data").

% fails if incorrect imports/exports
check_itf_data(Base, PlName) :-
    now_doing(['Checking interface data of ',PlName]),
    gen_imports(Base),
    check_exports(Base),
    check_multifile(Base),
    do_expansion_checks(Base),
    \+ current_fact(module_error),
    \+ current_fact(syntax_error_in(Base)),
    end_doing.
check_itf_data(Base, _) :-
    retractall_fact(module_error),
    assertz_fact(module_error(Base)),
    end_doing,
    message(error0, ['{Compilation aborted}']),
    signal_compilation_error,
    fail.

:- doc(subsection, "Generate imports data").

gen_imports(Base) :-
    ( imports_all(Base, File) ; reexports_all(Base, File) ),
    base_name(File, BFile),
    ( direct_export(BFile, F, A, DefType, Meta),
      EndFile = '.'
    ; indirect_export(BFile, F, A, DefType, Meta, EndFile0),
      relocate_endfile(BFile, File, EndFile0, EndFile)
    ),
    assertz_fact(imports_pred(Base, File, F, A, DefType, Meta, EndFile)),
    fail.
gen_imports(Base) :-
    ( retract_fact(imports_expl(Base, File, F, A))
    ; reexports(Base, File, F, A)
    ),
    base_name_or_user(File, BFile),
    ( exports_thru(BFile, F, A, DefType, Meta, EndFile) ->
        relocate_endfile(BFile, File, EndFile, EndFile2),
        assertz_fact(imports_pred(Base, File, F, A, DefType, Meta, EndFile2))
    ; defines_module(BFile, IM),
      interface_error(not_exported(IM,F/A))
    ),
    fail.
gen_imports(_).

base_name_or_user(user, Base) :- !, Base = user.
base_name_or_user(File, Base) :- base_name(File, Base).

:- doc(subsection, "Generate exports data").

exports_thru(user,_F,_A, DefType, Meta, EndFile) :- !,
    DefType = static, Meta = 0, EndFile = '.'.
exports_thru(BFile, F, A, DefType, Meta, '.') :-
    direct_export(BFile, F, A, DefType, Meta).
exports_thru(BFile, F, A, DefType, Meta, EndFile) :-
    indirect_export(BFile, F, A, DefType, Meta, EndFile).

indirect_export(BFile, F, A, DefType, Meta, EndFile) :-
    reexports_pred(BFile, MFile, F, A),
    base_name(MFile, BMFile),
    ( direct_export(BMFile, F, A, DefType, Meta), EndFile = MFile
    ; indirect_export(BMFile, F, A, DefType, Meta, EndFile0),
      relocate_endfile(BMFile, MFile, EndFile0, EndFile)
    ),
    \+ direct_export(BFile, F, A, _, _).

reexports_pred(Base, File, F, A) :-
    reexports(Base, File, F, A).
reexports_pred(Base, File,_F,_A) :-
    reexports_all(Base, File).

check_exports(Base) :-
    direct_export(Base, F, A, _, Meta),
      meta_inc_args(Meta, A, A1),
      ( defines_pred(Base, F, A1) -> true
      ; error_in_lns(_,_,warning, ['exported predicate ',~~(F/A),
                     ' is not defined in this module'])
      ),
    fail.
check_exports(_).

check_multifile(Base) :-
    def_multifile(Base, F, A, T),
    def_multifile(Base1, F, A, T1),
    T \== T1,
    defines_module(Base1, M1),
    interface_error(incompatible_multifile(F,A,T,T1,M1)),
    fail.
check_multifile(_).

interface_error(Error) :-
    compiler_error(_, _, Error),
    asserta_fact(module_error).

:- doc(subsection, "Other interface data checks").

do_expansion_checks(Base) :-
    retract_fact(expansion_check(Base, Pred)),
      Pred(Base),
    fail.
do_expansion_checks(_).

:- doc(subsection, "Relocate for reexports").

% TODO: EndFile relocation do not support alias paths in File, fix?

% Relocate EndFile0 (relative to File) as EndFile (relative to the
% current context). Also update base name database for EndFile
% (do_get_base_name/1).
%
% This assumes base_name(File, BFile) (a file and its absolute path
% name without extension). Example:
%
%  File:      dir1/a.pl (relative to the current context)
%  EndFile0:  dir2/c.pl (relative to File)
%  EndFile:   dir1/dir2/c.pl (now relative to the current context)

relocate_endfile(_BFile, _File, EndFile0, EndFile) :- EndFile0 = '.', !,
    EndFile = EndFile0.
relocate_endfile(BFile, File, EndFile0, EndFile) :-
    atom(File), atom(EndFile0), path_is_relative(EndFile0),
    !,
    path_split(File, Dir, _),
    path_concat(Dir, EndFile0, EndFile),
    ( ( path_is_relative(EndFile) -> % (change dir if needed)
          get_base_dir(File, BFile, BFileDir),
          working_directory(OldDir, BFileDir),
          ( true ; working_directory(_, OldDir), fail ) % (restore dir on backtracking)
      ; true
      ),
      do_get_base_name(EndFile),
      fail
    ; true
    ).
relocate_endfile(_, _, EndFile, EndFile).

% Get BFileDir such that BFileDir+File=BFile
get_base_dir(File, BFile, BFileDir) :-
    atom_concat(BFileDir0, File, BFile), % must be relative to File so that we find EndFile
    atom_concat(BFileDir, '/', BFileDir0).

% ---------------------------------------------------------------------------
:- doc(section, "Generate itf file").
% TODO: move to a separate file (not dealing with incremental compilation)

generate_itf(ItfName, Mode, Base) :-
    file_buffer_begin(ItfName, Buffer, Stream),
    current_output(CO),
    set_output(Stream),
    itf_version(V),
    current_prolog_flag(itf_format, Format),
    term_write(v(V,Format)),
    write_itf_data_of(Format, Base),
    set_output(CO),
    ( file_buffer_commit(Buffer) ->
        chmod(ItfName, Mode)
    ; ( current_prolog_flag(verbose_compilation,off) -> true
      ; message(warning, ['cannot create ',ItfName])
      )
    ),
    now(Now),
    assertz_fact(time_of_itf_data(Base,Now,1)).

write_itf_data_of(Format, Base) :-
    itf_data(ITF, Base, _, _, Fact),
      current_fact(Fact),
        do_write(Format, ITF),
    fail.
write_itf_data_of(_, _).

do_write(f,Term) :- fast_write(Term).
do_write(r,Term) :- term_write(Term).

% ---------------------------------------------------------------------------
:- doc(section, "Read itf file").

% (just get base names in stored itf data)
base_names_in_itf(ItfName, Base, Dir) :-
    working_directory(OldDir, Dir),
    ( true ; working_directory(_, OldDir), fail ),
    %
    now_doing(['Checking data of ',ItfName]),
    ( uses(Base, File)
    ; adds(Base,File)
    ; includes(Base,File)
    ; loads(Base,File)
    ; imports_pred(Base, _, _, _, _, _, File) % TODO: check if needed with ItfLevel=0
    ),
    do_get_base_name(File),
    fail.
base_names_in_itf(_,_,_).

% ItfLevel=0 read only dependencies
% ItfLevel=1 full itf read 

do_read_itf(ItfLevel, ItfName, Base, Dir, UpdTime) :-
    working_directory(OldDir, Dir),
    ( true ; working_directory(_, OldDir), fail ),
    do_read_itf_(ItfLevel, ItfName, Base), !,
    working_directory(_, OldDir),
    % Set or update time
    ( current_fact(time_of_itf_data(Base, PrevTime, _)) -> true ; true ),
    retractall_fact(time_of_itf_data(Base, _, _)),
    ( nonvar(PrevTime), UpdTime = prev -> Time = PrevTime
    ; now(Time)
    ),
    assertz_fact(time_of_itf_data(Base,Time,ItfLevel)).
    
do_read_itf_(ItfLevel, ItfName, Base) :-
    delete_itf_data_opt(Base),
    '$open'(ItfName, r, Stream),
    current_input(CI),
    set_input(Stream),
    ( itf_version(V),
      read(v(V,Format)), !
    ; set_input(CI),
      close(Stream),
      fail
    ),
    now_doing(['Reading ',ItfName]),
    read_itf_data_of(ItfLevel,Format,Base),
    set_input(CI),
    close(Stream).

read_itf_data_of(ItfLevel,Format,Base) :-
    repeat,
    do_read(Format,ITF),
    ( ITF = end_of_file, !
    ; itf_data(ITF, Base, File, DataLevel, Fact),
      ( DataLevel > ItfLevel ->
          ! % (stop here)
      ; do_get_base_name(File),
        assertz_fact(Fact),
        fail
      )
    ).

do_read(f,Term) :- fast_read(Term), ! ; Term = end_of_file.
do_read(r,Term) :- read(Term).

% Catch file errors now
do_get_base_name('.') :- !.
do_get_base_name(user) :- !.
do_get_base_name(File) :- get_base_name(File, _, _, _).

% ---------------------------------------------------------------------------
:- doc(section, "Format of itf files").

% We keep itf data split in two sections to speedup dependency
% checking operations.

:- meta_predicate itf_data(?, ?, ?, ?, fact).

% (dependencies section)
itf_data(m(M),             Base, user, 0, defines_module(Base,M)).
itf_data(u(File),          Base, File, 0, uses(Base,File)).
itf_data(e(File),          Base, File, 0, adds(Base,File)).
itf_data(n(File),          Base, File, 0, includes(Base,File)).
itf_data(l(File),          Base, File, 0, loads(Base,File)).
%itf_data(h(File),          Base, user, 0, reexports_from(Base,File)). % TODO: OLD
itf_data(h(File),          Base, File, 0, reexports_from(Base,File)).
itf_data(m(F,A,Def),       Base, user, 0, def_multifile(Base,F,A,Def)). % TODO: this should not be in deps but generate_multifile_data/2 from compute_load_action/4 needs it
% (interface section)
itf_data(e(F,A,Def,Meta),  Base, user, 1, direct_export(Base,F,A,Def,Meta)).
%itf_data(m(F,A,Def),       Base, user, 1, def_multifile(Base,F,A,Def)).
% The following five has File in uses/2
itf_data(i(File,F,A,Df,Mt,EF),Base, EF, 1, imports_pred(Base,File,F,A,Df,Mt,EF)). % TODO: check if EF appears in reexports_from
itf_data(i(File),          Base, user, 1, imports_all(Base,File)).
itf_data(r(File,F,A),      Base, user, 1, reexports(Base,File,F,A)).
itf_data(r(File),          Base, user, 1, reexports_all(Base,File)).
itf_data(d(Decl),          Base, user, 1, decl(Base,Decl)).

% ---------------------------------------------------------------------------
:- doc(section, "Fill data for mexpand (and treat_file/2)").

generate_module_data(Base, M) :-
    defines_pred(Base, F, A),
      assertz_fact(defines(M, F, A)),
    fail.
generate_module_data(Base, M) :-
    def_multifile(Base, F, A, Def),
      assertz_fact(multifile(M, F, A, Def)),
    fail.
generate_module_data(Base, M) :-
    meta_pred(Base, _, _, Meta),
      assertz_fact(meta_args(M, Meta)),
    fail.
generate_module_data(Base, M) :-
    imports_pred(Base, File, F, A, _, Meta, EndFile),
      file_defines_module(File, IM),
      ( EndFile = '.' -> EM = IM ; file_defines_module(EndFile, EM) ),
      assertz_fact(imports(M, IM, F, A, EM)),
      Meta \== 0,
      assertz_fact(meta_args(EM, Meta)),
    fail.
generate_module_data(Base, M) :-
    imports_nocheck(Base, IM, F, A),
      assertz_fact(imports(M, IM, F, A, IM)),
    fail.
generate_module_data(_, _).

% ---------------------------------------------------------------------------
:- doc(section, "Detect changes in dependencies").

:- if(\+ defined(itf_sections)).
% Fine-grained dependency changes
%
% Deftype not checked because is not exact in builtin modules
changed_dependencies(Base, _) :-
    imports_pred(Base, File, F, A,_DefType, Meta, EndFile),
    base_name(File, BFile),
    \+ exports_thru(BFile, F, A,_DefType2, Meta, EndFile).
changed_dependencies(Base, _) :-
    ( imports_all(Base, File); reexports_all(Base, File) ),
    base_name(File, BFile),
    ( direct_export(BFile, F, A, _, _)
    ; indirect_export(BFile, F, A, _, _, _) ),
    \+ imports_pred(Base, File, F, A, _, _, _).
:- else.
changed_dependencies(Base, ItfTime) :-
    % Assume that if any of the imported files have changed our
    % dependencies have potentially changed too (which is faster than
    % the fine-grained method although it may raise some false
    % positives)
    ( uses(Base, File)
    ; adds(Base, File)
    ),
    file_data(File, PlName, _),
    modif_time(PlName, PlTime),
    PlTime > ItfTime.
:- endif.
changed_dependencies(Base, ItfTime) :-
    includes(Base, File),
    base_name(File, BFile),
    file_data(BFile, PlName, _),
    modif_time(PlName, PlTime),
    PlTime > ItfTime.
changed_dependencies(Base, ItfTime) :-
    loads(Base, File),
    base_name(File, Base2),
    ( file_data(Base2, PlName2, _),
      modif_time(PlName2, PlTime2),
      PlTime2 > ItfTime
    ; itf_filename(Base2, ItfName),
      modif_time0(ItfName, ItfTime2),
      ItfTime2 > ItfTime
    ).

% ---------------------------------------------------------------------------

:- meta_predicate sequence_contains(+, pred(1), -, -).

sequence_contains(V, BadP, _, _) :- var(V), !,
    BadP(V), fail.
sequence_contains([], _, _, _) :- !, fail.
sequence_contains([S|Ss], BadP, F, A) :- !,
    ( sequence_contains(S, BadP, F, A)
    ; sequence_contains(Ss, BadP, F, A)
    ).
sequence_contains((S,Ss), BadP, F, A) :- !,
    ( sequence_contains(S, BadP, F, A)
    ; sequence_contains(Ss, BadP, F, A)
    ).
sequence_contains(F/A, _, F, A) :-
    atom(F), integer(A), !.
sequence_contains(S, BadP, _, _) :-
    BadP(S), fail.

% ---------------------------------------------------------------------------
:- doc(section, "Support for ciao-shell scripts").

skip_shell_lines(Stream) :-
    peek_code(Stream, 0'#), !,
    current_input(OldIn),
    set_input(Stream),
    skip_code(10),
    get_line(Line),
    skip_lines_until_blank(Line),
    set_input(OldIn).
skip_shell_lines(_).

skip_lines_until_blank(Line) :-
    whitespace0(Line, []), !.
skip_lines_until_blank(_) :-
    get_line(Line),
    skip_lines_until_blank(Line).

% ---------------------------------------------------------------------------
:- doc(section, "Handler for compilation errors").

signal_compilation_error :- 
    send_signal(compilation_error, _). % (ignore if there is no handler)

:- export(handle_exc/1).
handle_exc(cannot_create(File)) :- !,
    message(error, ['Unable to create ',File,' - aborting...']), 
    signal_compilation_error.
handle_exc(error(Error, _)) :-
    handle_file_error(Error), !, signal_compilation_error.
handle_exc(unintercepted_signal(control_c)) :- !,
    ctrlcclean, signal_compilation_error.
handle_exc(compiler_loop) :- !, 
    signal_compilation_error, 
    fail.
handle_exc(Error) :- throw(Error).

handle_file_error(existence_error(source_sink,File)) :-
    error_in_lns(_,_,error, ['File ',File,' not found - aborting...']).
handle_file_error(permission_error(open,source_sink,File)) :-
    message(error, ['Cannot open ',File,' - aborting...']).

warning_module_missing(L0, L1) :-
    error_in_lns(L0, L1, warning,
                 ['Source used as module without module declaration']).

compiler_error(L0, L1, Error) :-
    compiler_error_data(Error, Message),
    error_in_lns(L0, L1, error, Message).

compiler_error_data(module_missing,
    ['Source used as module without module declaration']).
compiler_error_data(bad_module(_Base, M),
    ['Bad module ',M,' in module declaration']).
compiler_error_data(bad_package(_Base, M),
    ['Bad package ',M,' in package declaration']).
compiler_error_data(badly_formed(Decl, Spec),
    ['Bad predicate indicator ',~~(Spec),' in ',Decl,' directive']).
compiler_error_data(nonstarting(F,A),
    ['Declaration ',~~(F/A),' not starting file']).
compiler_error_data(nonstarting_package,
    ['Declaration package/1 not starting file, probably a package is being included as a raw file']).
compiler_error_data(bad_package_file(F),
    ['Bad package file ',~~(F)]).
compiler_error_data(bad_export(Spec),
    ['Bad predicate indicator ',~~(Spec),' in export']).
compiler_error_data(bad_use_module(ModuleFile),
    ['Bad/unreadable file ',ModuleFile,' in use_module declaration']).
compiler_error_data(bad_file(File),
    ['Bad/unreadable file ',File,' in declaration']).
compiler_error_data(all_user,
    ['Attempt to import all user predicates']).
compiler_error_data(bad_import_list(Bad),
    ['Bad import/reexport list ',~~(Bad)]).
compiler_error_data(bad_import_spec(Spec),
    ['Bad predicate indicator ',~~(Spec),' in import/reexport']).
compiler_error_data(bad_import(Module),
    ['Bad module ',Module,' in import declaration']).
compiler_error_data(bad_meta_predicate(Spec),
    ['Bad meta_predicate specification ',~~(Spec)]).
compiler_error_data(not_exported(IM,F/A),
    ['imported predicate ',~~(F/A),' not exported by ',IM]).
compiler_error_data(incompatible_multifile(F,A,T,T1,M1),
    ['multifile predicate ',~~(F/A),' is defined ',T,
     ' while in module ',M1,' is defined ',T1]).
compiler_error_data(incompatible_decl(F,A,Decl,Decl2),
    ['predicate ',~~(F/A),' is being defined ',Decl,
                   ' but it was already defined ',Decl2]).
compiler_error_data(module_redefined(OtherFile, _SourceFile, M),
    ['Module ',M,' already defined in source ',OtherFile]).

% ---------------------------------------------------------------------------
:- doc(section, "Compilation to .po").

:- export(false/1).
false(_) :- fail.

make_po1(File) :-
    process_file(File, po, any, make_po_file, c_itf:false, c_itf:false,
                 po_older_than_itf),
    base_name(File, Base),       
    make_gluecode(Base).

make_wam1(File) :-
    process_file(File, wam, any, make_wam_file, c_itf:false, c_itf:false,
                 wam_older_than_itf).

:- export(make_object1/2).
make_object1(po,  File) :-
    make_po1(File).
make_object1(wam, File) :-
    make_wam1(File).

po_older_than_itf(Base) :-
    po_filename(Base, PoName),
    file_older_than_itf(Base, PoName).

wam_older_than_itf(Base) :-
    wam_filename(Base, WamName),
    file_older_than_itf(Base, WamName).

file_older_than_itf(Base, File) :-
    modif_time0(File, FileTime),
    itf_filename(Base, ItfName),
    modif_time0(ItfName, ItfTime),
    FileTime < ItfTime.

:- export(make_po_file/1).
make_po_file(Base) :-
    make_po_file_1(Base, po_filename,  ql(unprofiled), 'Compiling ').

make_wam_file(Base) :-
    make_po_file_1(Base, wam_filename, wam, 'Assembling ').

:- meta_predicate make_po_file_1(+, pred(2), +, +).

make_po_file_1(Base, FilePred, Mode, Message) :-
    file_data(Base, Source, _Dir),
    defines_module(Base, Module),
    FilePred(Base, PoName),
    now_doing([Message,Source]),
    make_po_file_2(PoName, Mode, Base, Module, Source),
    end_doing.

make_po_file_2(PoName, Mode, Base, Module, Source) :-
    file_buffer_begin(PoName, Buffer, Stream),
    flatten_mod_name(Module, FlatModule), % TODO: use flat name in more places?
    reset_counter(FlatModule),
    set_compiler_mode_out(Mode, Stream),
    compiler_pass(Source, Base, Module, Mode, OK),
    retractall_fact(incore_mode_of(_, _)), % TODO: why?
    ( OK = yes ->
        ( file_buffer_commit(Buffer) ->
            fmode(Source, FMode),
            chmod(PoName, FMode)
        ; ( file_exists(PoName) ->
              message(warning, ['Unable to update ',PoName,
                                ' - using existing file'])
          ; throw(cannot_create(PoName))
          )
        )
    ; file_buffer_erase(Buffer) % TODO: keep previous version instead?
    ).

% ---------------------------------------------------------------------------
:- doc(section, "Expand and check assertions").

check_assertions_syntax(M) :-
    current_fact(assertion_read(_P, M, _S, T, B, Dict, S, LB, LE)),
    ( T \== modedef, T \== test, T \== texec -> % Skip tests assertions, not processed here
        asserta_fact(location(S, LB, LE), LRef),
        assrt_module_expansion(M, T, B, Dict, _Def, _H, _Props), % TODO: save? (e.g., for ciaopp/lpdoc)
        erase(LRef)
    ; true
    ),
    fail.
check_assertions_syntax(_).

warn_not_defined(decl, no) :- !.
warn_not_defined(_,    yes).

check_not_defined(no, yes, F, N, M) :-
    !,
    module_warning(not_defined(F, N, M)).
check_not_defined(_, _, _, _, _).

assrt_module_expansion(M, Type, Body, Dict, Defined, H, Props) :-
    assertion_body(PD, Co, Ca, Su, Cp, _Cm, Body),
    functor(PD, F, N),
    assr_head_expansion(PD, M, F, N, MH, H, Defined),
    warn_not_defined(Type, WND),
    check_not_defined(Defined, WND, F, N, M),
%       Trick to avoid warnings about module qualification:
    ( Defined == no -> G = true
    ; MH == multifile -> G = PD
    ; G = MH:PD
    ),
%       Trick to avoid duplicated warnings about undefined predicates:
    comps_to_goal(Cp, Cp1, G),
%       This makes the syntax checking:
    maplist(([M,Dict] -> ''(V1,V2) :-
              maplist(expand_subbody(M, Dict),V1,V2)),
            [Co, Ca, Su, [Cp1]], Props).

% Head expansion for assertions
assr_head_expansion(A, M, F, N, MQ, NA, Defined) :-
    ( mexpand_multifile(M, F, N) ->
        MA = multifile, MQ = MA, Defined = yes
    ; mexpand_defines(M, F, N) ->
          MA = M, MQ = M, Defined = yes
    ; mexpand_imports(M, IM, F, N, EM) ->
        MA = EM, MQ = IM, Defined = yes
    ; MA = M, MQ = M, Defined = no
    ),
    module_concat(MA, A, NA).

expand_subbody(M, Dict, C, EC) :-
    expand_head_body(in_assertion_body, C, M, Dict, asr, _, EC).
%       asbody_to_conj(CO, EC).

% ---------------------------------------------------------------------------
:- doc(section, "Module compilation pass (after source is read)").

compiler_pass(Source, Base, Module, Mode, OK) :-
    del_compiler_pass_data,
    asserta_fact(compiling_src(Source)),
    flatten_mod_name(Module, FlatModule), % TODO: use flat name in more places?
    compile_set_currmod(Mode, FlatModule),
    compile_multifile_decls(Base, Module, Mode),
    compile_dyn_decls(Base, Module, Mode),
    activate_translation(Base, Module, add_clause_trans),
    activate_translation(Base, Module, add_goal_trans),
    %log_translations(Base, Module, clause),
    %log_translations(Base, Module, goal),
    compile_goal_decl(initialization, Base, Module, Mode),
    compile_goal_decl(on_abort, Base, Module, Mode),
    compile_clauses(Base, Module, Mode),
    ( current_prolog_flag(read_assertions, yes) ->  
      check_assertions_syntax(Module)
    ; true
    ),
    (current_fact(mexpand_error) -> OK = no ; OK = yes),
    ( OK = yes ->
        compile_ldlibs(Base, Module, Mode),
        compile_dependences(Base, Module, Mode),
        include_module_data(Mode, Module)
    ; 
        message(error, ['Aborted module compilation']), 
        signal_compilation_error
    ),
    end_goal_trans(Module),
    del_clause_trans(Module),
    del_goal_trans(Module),
    end_brace_if_needed,
    compile_unset_currmod(Mode),
    cleanup_compilation_data.

% ---------------------------------------------------------------------------
:- doc(section, "Translations for compiler_pass").

:- export(activate_translation/3).
activate_translation(Base, Module, Name) :-
    % translation with priority 
    functor(Decl, Name, 2),
    arg(1, Decl, Pred),
    arg(2, Decl, Prior),
    clause_of(Base, 1, Decl, _, Src, Ln0, Ln1),
      functor(Goal, Name, 3),
      arg(1, Goal, Module),
      arg(2, Goal, Pred),
      arg(3, Goal, Prior),
      do_add_trans(Goal, Decl, Src, Ln0, Ln1),
      fail.
activate_translation(_, _, _).

do_add_trans(Goal, Decl, Src, Ln0, Ln1) :-
    ( call_trans(Goal) -> true
    ; message(error0, ['{In ',Src]),
      message_lns(warning, Ln0, Ln1,
                  [Decl,' - declaration failed']),
      message(error0, '}')
    ).

% (Avoid meta-expansions)
call_trans(add_clause_trans(M, Tr, Prior)) :- add_clause_trans(M, Tr, Prior).
call_trans(add_goal_trans(M, Tr, Prior)) :- add_goal_trans(M, Tr, Prior).

:- export(end_goal_trans/1).
end_goal_trans(M) :-
    goal_trans(M, T, _),
    call_goal_trans(T),
    fail.
end_goal_trans(_).

call_goal_trans(T) :-
    arg(1, T, end_of_file),
    '$meta_call'(T),
    !.

% ---------------------------------------------------------------------------
:- doc(section, "Compile clauses").

compile_clauses(Base, Module, Mode) :-
    expand_clause(before_mexp, 0, 0, Module, _, _, _), % Translator initialization
    current_fact(clause_of(Base,H,B,Dict,Src,Ln0,Ln1), CRef),
      erase(CRef),
      asserta_fact(location(Src,Ln0,Ln1), Ref),
      ( number(H) ->
          true
      ; H = '\3\rawbody'(H0) -> % (special case, do not expand body)
          head_expansion(H0, Module, H2),
          compile_clause(Mode, H2, B, Module) % (do not expand body)
      ; module_expansion(H, B, Module, Dict, Mode, _, _, H2, B2),
        compile_clause(Mode, H2, B2, Module)
      ),
      erase(Ref),
    fail.
compile_clauses(_, _, _).

:- export(module_expansion/9).
module_expansion(H, B, Module, Dict, Mode, H0, B0, H2, B2) :-
    expand_clause(before_mexp, H, B, Module, Dict, H0, B0),
    expand_head_body(H0, B0, Module, Dict, Mode, H1, B1),
    expand_clause(after_mexp, H1, B1, Module, Dict, H2, B2).

expand_head_body(H0, B0, Module, Dict, Mode, H2, B2) :-
    ( Mode = interpreted,
      current_fact(interpret_srcdbg(Module)) ->
      srcdbg_expand(H0,B0,H1,B1,Dict,Dict1),
      Dbg = yes
    ; H1 = H0, B1 = B0, Dbg = no
    ),
    head_expansion(H1, Module, H2),
    body_expansion(B1, Module, -, compile, B2),
    (
        Dbg = yes ->
        complete_dict(H2-B2, Dict, [Dict1], Dict2),
        % Variables generated by the compiler placed in Dict1
        reverse(Dict2, Dict1)
    ;
        Dict1 = Dict
    ).

compile_clause(ql(_), Head, Body, _Module) :- !,
    compile_clause(Head, Body). % in pl2wam
compile_clause(wam, Head, Body, _Module) :- !,
    compile_clause(Head, Body). % in pl2wam
compile_clause(incoreql(Pr), Head, Body, Module) :- !,
    incore_mode(incore(Pr), Head, Mode),
    ( Mode = incore(Pr) ->
          compile_clause(Head, Body) % in pl2wam
    ; compile_clause_in_mode(ql(Pr), Head, Body),
      % multifile or interpreted % TODO: repeating work?
      compile_clause_incore(Mode, Head, Body, Module)
    ).
compile_clause(Mode, Head, Body, Module) :- % incore(Pr) or interpreted
    incore_mode(Mode, Head, NewMode),
    compile_clause_incore(NewMode, Head, Body, Module).
          
compile_clause_incore(multifile(Mode), Head, Body, Module) :- !,
    add_multifile_clause(Head, Body, Module, Mode).
compile_clause_incore(interpreted, Head, Body, _) :- !,
    assert_clause(Head, Body).
compile_clause_incore(Mode, Head, Body, _) :-
    compile_clause_in_mode(Mode, Head, Body).

% ---------------------------------------------------------------------------
:- doc(section, "Compile initialization/1, on_abort/1").

compile_goal_decl(DN, Base, Module, Mode) :-
    functor(Decl, DN, 1),
    findall(loc(Decl,Src,Ln0,Ln1),
            clause_of(Base, 1, Decl, _Dict, Src, Ln0, Ln1),
            Decls),
    compile_goal_decls(Decls, DN, Module, Mode).

compile_goal_decls([], _, _, _) :- !.
compile_goal_decls(Decls, DN, Module, Mode) :-
    functor(DeclM, DN, 1),
    arg(1, DeclM, Module),
    compile_goal_decls_(Decls, DeclM, Module, Mode).

compile_goal_decls_([], _, _, _).
compile_goal_decls_([loc(Decl,Src,Ln0,Ln1)|_], DeclM, Module, Mode) :-
    asserta_fact(location(Src,Ln0,Ln1), Ref),
    arg(1, Decl, Goal),
    body_expansion(Goal, Module, -, compile, Goal1),
    ( DeclM = initialization(XX) ->
        DeclM2 = 'multifile:$initialization'(XX)
    ; DeclM = on_abort(XX) ->
        DeclM2 = 'multifile:$on_abort'(XX)
    ; fail
    ),
    compile_clause(Mode, DeclM2, Goal1, Module),
    erase(Ref),
    fail.
compile_goal_decls_([_|Decls], DeclM, Module, Mode) :-
    compile_goal_decls_(Decls, DeclM, Module, Mode).

% ---------------------------------------------------------------------------
:- doc(section, "Compile module meta information").
% ldlibs, dependencies, etc. as multifile preds

compile_ldlibs(Base, Module, Mode) :-
    findall('internals:load_lib'(M, F), module_lib(Base, F, M), LLibs),
    list_to_conjunction(LLibs, LoadLibs),
    compile_clause(Mode, 'multifile:$ldlibs'(Module), LoadLibs, Module).

module_lib(Base, Lib, LibMod) :-
    Lib = library(_),
    uses_or_adds(Base, Lib),
    base_name(Lib, LibBase),
    defines_module(LibBase, LibMod).

uses_or_adds(Base, File) :- uses(Base, File).
uses_or_adds(Base, File) :- adds(Base, File).

list_to_conjunction([X],X):- !.
list_to_conjunction([X|More],'basiccontrol:,'(X,Next)):-
    list_to_conjunction(More,Next).
list_to_conjunction([],'basiccontrol:true').

compile_dependences(Base, Module, Mode) :-
    uses_or_adds(Base, File2),
      base_name(File2, Base2), defines_module(Base2, Module2),
      compile_clause(Mode, 'multifile:$u'(Module, Module2), 'basiccontrol:true', Module),
    fail
      ; true.

include_module_data(Mode, Module) :-
    compile_clause(Mode, 'multifile:$current_module'(Module), 'basiccontrol:true', Module),
    include_meta_args(Mode, Module),
    retract_fact(runtime_module_exp), !,
    include_runtime_data(Mode, Module).
include_module_data(_, _).

include_meta_args(Mode, M) :-
    meta_args(M, P),
      compile_clause(Mode, 'multifile:$meta_args'(M, P), 'basiccontrol:true', M),
    fail.
include_meta_args(_, _).

include_runtime_data(Mode, M) :-
    imports(M, IM, F, N, EM),
      compile_clause(Mode, 'multifile:$imports'(M, IM, F, N, EM), 'basiccontrol:true', M),
    fail.
include_runtime_data(Mode, M) :-
    multifile(M, F, N),
      compile_clause(Mode, 'multifile:$multifile'(M, F, N), 'basiccontrol:true', M),
    fail.
include_runtime_data(Mode, M) :-
    defines(M, F, N),
      compile_clause(Mode, 'multifile:$defines'(M, F, N), 'basiccontrol:true', M),
    fail.
include_runtime_data(_, _).

% include_decl(Mode, Decl, F, A) :-
%       ((Mode = ql(Pr) ; Mode = incoreql(Pr)) ->
%           functor(P, F, A),
%           proc_declaration_in_mode(ql(Pr), Decl, P, F, A)
%       ; Mode = wam ->
%           functor(P, F, A),
%           proc_declaration_in_mode(wam, Decl, P, F, A)
%       ; true
%       ).

:- export(c_itf_internal_pred/2).
% Internal predicates introduced during c_itf processing
% (for runtime module expansion and some module hooks)
c_itf_internal_pred('$primitive_meta_predicate',2).
c_itf_internal_pred('$current_module',1).
c_itf_internal_pred('$ldlibs',1).
c_itf_internal_pred('$multifile',3).
c_itf_internal_pred('$load_libs',0).
c_itf_internal_pred('$meta_args',2).
c_itf_internal_pred('$u',2).
c_itf_internal_pred('$initialization',1).
c_itf_internal_pred('$on_abort',1).
c_itf_internal_pred('$imports',5).
c_itf_internal_pred('$defines',3).

:- export(c_itf_internal_pred_decl/1).
% Declarations from c_itf_internal_pred/2
c_itf_internal_pred_decl(Decl) :-
    ( Decl = multifile(FA) -> true
    ; Decl = discontiguous(FA) -> true
    ; Decl = dynamic(FA) -> true
    ; fail
    ),
    nonvar(FA),
    FA = Pred/Arity,
    c_itf_internal_pred(Pred,Arity).

% ---------------------------------------------------------------------------
:- doc(section, "Compile mod name").

compile_set_currmod(ql(_), Module) :- !,
    mod_declaration(Module). % in pl2wam
compile_set_currmod(wam, Module) :- !,
    mod_declaration(Module).
compile_set_currmod(incoreql(Pr), Module) :- !,
    mod_declaration_in_mode(ql(Pr), Module),
    incore_decl_mod(Module).
compile_set_currmod(_Mode, Module) :-
    incore_decl_mod(Module).

incore_decl_mod(Module) :-
    '$set_currmod'(Module).

compile_unset_currmod(ql(_)) :- !.
compile_unset_currmod(wam) :- !.
compile_unset_currmod(_Mode) :- '$set_currmod'(0).

% flatten mod name into an atom
flatten_mod_name(Module0, Module) :-
    ( Module0 = user(Base) ->
        atom_concat('user$$$', Base, Module)
    ; Module0 = Module
    ).

% ---------------------------------------------------------------------------
:- doc(section, "Compile declarations").

compile_multifile_decls(Base, Module, Mode) :-
    def_multifile(Base, F, A, D),
      module_concat(multifile, F, MF),
      functor(MP, MF, A),
      compile_decl(Mode, multifile, MP, MF, A, Module),
      low_dyn_decl(D, D0),
        compile_decl(Mode, D0, MP, MF, A, Module),
    fail.
compile_multifile_decls(_, _, _).

compile_dyn_decls(Base, Module, Mode) :-
    dyn_decl(Base, F, A, D),
      low_dyn_decl(D, D0),
      module_concat(Module, F, MF),
      functor(MP, MF, A),
      compile_decl(Mode, D0, MP, MF, A, Module),
    fail.
compile_dyn_decls(_, _, _).

low_dyn_decl(data,       dynamic).
low_dyn_decl(dynamic,    dynamic).
low_dyn_decl(concurrent, concurrent).

:- export(incore_mode_of/2). % TODO: temporarily exported (for pl2wam.pl)
:- data incore_mode_of/2. % Predicate HEAD was compiled with MODE, one of
                      % {interpreted, incore(unprofiled), incore(profiled)}
                      % or multifile(Mode) been Mode one of that

:- export(pred_module/2).
:- data pred_module/2. % Predicate HEAD was loaded from MODULE
                   % (does not include multifile predicates)

compile_decl(ql(_), Decl, P, F, A,_Module) :- !,
    proc_declaration(Decl, P, F, A). % in pl2wam
compile_decl(wam, Decl, P, F, A,_Module) :- !,
    proc_declaration(Decl, P, F, A).
compile_decl(incoreql(Pr), Decl, P, F, A, Module) :- !,
    proc_declaration_in_mode(ql(Pr), Decl, P, F, A),
    incore_decl_pred(Decl, P, F, A, incore(Pr), Module).
compile_decl(Mode, Decl, P, F, A, Module) :-
    incore_decl_pred(Decl, P, F, A, Mode, Module).

incore_decl_pred(Decl, P, F, A,_Mode,_Module) :-
    incore_mode_of(P, _), !,
    error_in_lns(_,_,error, [Decl,' declaration of ',~~(F/A),' too late']).
incore_decl_pred(multifile, P, F, A, Mode, Module) :- !,
    ( '$predicate_property'(P, _, Bits) ->
        check_multifile_type(Module, F, A, Bits)
    ; incore_internal_mode(Mode, IM),
      '$define_predicate'(F/A, IM),
      '$set_property'(P, multifile)
    ).
incore_decl_pred(Dynamic, P,_F,_A,_Mode,_Module) :- % dynamic or concurrent
    incore_mode(interpreted, P, Mode),
    ( Mode = interpreted ->
          '$set_property'(P, Dynamic)
    ; Mode = multifile(interpreted) ->
          true
    ; '$set_property'(P, Dynamic) ->
          retract_fact(incore_mode_of(P, _)),
          asserta_fact(incore_mode_of(P, multifile(interpreted)))
    ; true % errors are given by preceding clause
    ).

check_multifile_type(Module, F, A, Bits) :-
    multifile_pred(F, F_),
    multifile(Module, F_, A, Dyn),
    low_dyn_decl(Dyn, DynMode),
    \+ dynmode_has_bit(DynMode, Bits), !,
    error_in_lns(_,_,error, ['Multifile predicate ',~~(F_/A),' defined ',
                 DynMode,' in ',Module,' but currently is not']).
check_multifile_type(_, _, _, _).

% MF is the module qualified name for multifile predicate F
multifile_pred(MF, F) :- atom_concat('multifile:', F, MF).

dynmode_has_bit(dynamic, Bits) :- Bits/\2 =:= 2.
dynmode_has_bit(concurrent, Bits) :- Bits/\1 =:= 1.

incore_internal_mode(interpreted, interpreted).
incore_internal_mode(incore(Profiling), Profiling).

% ---------------------------------------------------------------------------
:- doc(section, "Add multifile clauses (at runtime)").

add_multifile_clause(H, B, M, Mode) :-
    get_expanded_multifile(H, M, NH, Mode),
    % (asserted before M is instantianted in NH)
    functor(NH, _, A),
    arg(A, NH, M),
    assert_clause(NH, B).

:- data expanded_multifile/2. % Multifile predicate HEAD is expanded with
                          % NEWPRED

get_expanded_multifile(H, _, P, _) :- expanded_multifile(H, P), !.
get_expanded_multifile(H, M, P, Mode) :-
    functor(H, F, A),
    ( atom(M) -> atom_concat([F, '$', M, '$ex$'], Base)
    ; atom_concat(F, '$user$ex$', Base)
    ),
    new_mp_name(Base, N),
    A1 is A+1,
    functor(H0, F, A),
    functor(P, N, A1),
    copy_args(A, H0, P),  % Last arg of P is module name
    assertz_fact(expanded_multifile(H0, P)),
    compile_clause_incore(Mode, H0, P, _),
    H = H0, % copy args from head
    '$define_predicate'(N/A1, interpreted),
    '$set_property'(P, dynamic).

:- data mp_index/2. % TODO: clean?

new_mp_name(Base, P) :-
    ( retract_fact(mp_index(Base, I)) -> true ; I = 0),
    I1 is I+1,
    asserta_fact(mp_index(Base, I1)),
    number_codes(I, N),
    atom_codes(Base, S),
    append(S, N, PS),
    atom_codes(P, PS).

% ---------------------------------------------------------------------------
:- doc(section, "Compilation mode for incore predicates").

% Determines the compilation mode of a predicate
incore_mode(_, Head, Mode) :- incore_mode_of(Head, Mode), !.
incore_mode(CurrMode, Head, Mode) :-
    functor(Head, F, A),
    functor(Head0, F, A),
    ( '$predicate_property'(Head0, Enter, Bits) ->
      % Existing predicate, should be multifile or from user
      ( Bits/\8 =:= 8 -> % multifile
            mode_from_enter(Enter, MMode),
            Mode = multifile(MMode)
      ; ( retract_fact(pred_module(Head0, Module)) ->
              Place = Module
        ; Place = 'this executable'
        ),
        error_in_lns(_,_,warning, ['predicate ',~~(F/A),' from ',Place,
                     ' is being redefined']),
        '$abolish'(Head0),
        incore_internal_mode(CurrMode, IM),
        '$define_predicate'(F/A, IM),
        Mode = CurrMode
      )
    ; incore_internal_mode(CurrMode, IM),
      '$define_predicate'(F/A, IM),
      Mode = CurrMode
    ),
    asserta_fact(incore_mode_of(Head0, Mode)).

mode_from_enter(0, incore(unprofiled)). % COMPACTCODE            
mode_from_enter(1, incore(unprofiled)). % COMPACTCODE_INDEXED    
mode_from_enter(2, incore(profiled)).   % PROFILEDCODE           
mode_from_enter(3, incore(profiled)).   % PROFILEDCODE_INDEXED   
mode_from_enter(8, interpreted).        % INTERPRETED            

% ---------------------------------------------------------------------------
:- doc(section, "Data for compiler_pass state").

:- export(location_t/1).
:- prop location_t/1 + regtype # "Identifies a source line range in a file.
   @includedef{location_t/1}".

location_t(loc(File, L1, L2)) :- atm(File), int(L1), int(L2).

:- export(location/3).
:- pred location/3 => atm * int * int.
:- data location/3.

:- export(location/1).
:- pred location(Loc) => location_t(Loc) # "Unifies @var{Loc} with the
    locator of the current source line range.  If this information
    can not be obtained, leaves @var{Loc} uninstantiated.".

location(Loc) :- (location(Src, Ln0, Ln1) -> Loc = loc(Src, Ln0, Ln1) ; true).

:- data compiling_src/1.

:- data mexpand_error/0.
set_mexpand_error :-
    ( current_fact(mexpand_error) -> true
    ; assertz_fact(mexpand_error)
    ).

:- data runtime_module_exp/0.

uses_runtime_module_expansion :- % Called from engine(mexpand)
    ( current_fact(runtime_module_exp) -> true
    ; asserta_fact(runtime_module_exp)
    ).

del_compiler_pass_data :-
    retractall_fact(mexpand_error),
    retractall_fact(runtime_module_exp),
    retractall_fact(location(_,_,_)),
    retractall_fact(compiling_src(_)),
    retractall_fact(last_error_in_src(_)).

% ---------------------------------------------------------------------------
:- doc(section, "Errors for compiler_pass").

% This predicate is called from mexpand
module_warning(Error) :-
    module_warning_mess(Error, Type, MessL),
    ( put_doing(Type),
      current_fact(location(Src,L0,L1)) ->
        Location = location(Src,L0,L1)
    ; Location = none
    ),
    ( Location = none, ciaopp_expansion ->
        true % ignore
    ; ( Type = error -> set_mexpand_error ; true ),
      ( Location = location(Src,L0,L1) ->
          put_src_if_needed(Type, Src),
          message_lns(Type, L0, L1, MessL)
      ; message(Type, MessL)
      )
    ).

module_warning_mess(not_defined(F, N,_M), error,
    ['Predicate ',~~(F/N),' undefined in source']).
module_warning_mess(not_imported(F, N,_M, QM), error,
    ['Bad module qualification of ',~~(F/N),
     ', predicate not imported from module ',QM]).
module_warning_mess(imported_needs_qual(F, N, M), warning,
    ['Unqualified predicate call to ',~~(F/N),
     ' assumed to local version, calls to predicate imported from ',M,
     ' must be qualified']).
module_warning_mess(imported_needs_qual(F, N, M0, M), warning,
    ['Unqualified predicate call to ',~~(F/N),
     ' assumed to module ',M,', calls to predicate imported from ',M0,
     ' must be qualified']).
module_warning_mess(bad_pred_abs(PA), error,
    ['Bad predicate abstraction ',~~(PA),
     ' : head functor should be ''''']).
module_warning_mess(big_pred_abs(PA,N), error,
    ['Predicate abstraction ',~~(PA),
     ' has too many arguments: should be ',N]).
module_warning_mess(short_pred_abs(PA,N), error,
    ['Predicate abstraction ',~~(PA),
     ' has too few arguments: should be ',N]).

% ---------------------------------------------------------------------------
:- doc(section, "Instance of 'mexpand' for compiler_pass").

% Enable or disable custom module expansion required by ciaopp
:- data ciaopp_expansion_enabled/0.

:- export(set_ciaopp_expansion/1).
set_ciaopp_expansion(true) :-
    current_fact(ciaopp_expansion_enabled), !.
set_ciaopp_expansion(true) :-
    asserta_fact(ciaopp_expansion_enabled).
set_ciaopp_expansion(false) :-
    retractall_fact(ciaopp_expansion_enabled).

ciaopp_expansion :- current_fact(ciaopp_expansion_enabled).

:- include(library(compiler/mexpand)).

mexpand_meta_args(M, P) :-
    meta_args(M, P).
mexpand_imports(M, IM, F, N, EM) :-
    imports(M, IM, F, N, EM).
mexpand_defines(M, F, N) :-
    defines(M, F, N).
mexpand_defines(M, F, N) :-
    functor(Meta, F, N),
    meta_args(M, Meta),
    meta_inc_args(Meta, N, N1),
    defines(M, F, N1).
mexpand_multifile(M, F, N) :-
    multifile(M, F, N).

multifile(M, F, N) :- multifile(M, F, N, _DynMode).

% ---------------------------------------------------------------------------
:- doc(section, "Head module expansion").

head_expansion('$:'(H), _, H) :- !. % For use by code translators
head_expansion(H, M, NH) :-
    functor(H, F, N),
    head_expansion0(H, F, N, M, H1),
    meta_expansion_keep_arity(F, N, H1, M, NH).

meta_expansion_keep_arity(F, N, H, M, NH) :-
    possibly_meta_expansion_head(F, N, H, M, M, NH, no, no),
    functor(NH, _, N), % addmodule meta expansion not applied 
    !.
meta_expansion_keep_arity(_, _, H, _, H).

possibly_meta_expansion_head(F, N, A1, M, RM, NA, G, G_) :-
    functor(Meta, F, N),
    % JF: it does not take into account expand_inside/2
    mexpand_meta_args(RM, Meta), !,
    functor(A1, F_, N_),
    assertz_fact(head_expansion, Ref),
    meta_expansion_args(1, N_, A1, M, compile, Meta, fail,
                        NAL, G, G_),
    erase(Ref),
    NA =.. [F_|NAL].
possibly_meta_expansion_head(_F,_N, A1,_M,_RM, A1, G, G). % No meta expansion

head_expansion0(H, F, N, M, NH):-
    multifile(M, F, N), !,
    module_concat(multifile, H, NH).
head_expansion0(H, _, _, M, NH):-
    module_concat(M, H, NH).

% ---------------------------------------------------------------------------
:- doc(section, "Dynamic incore (direct to memory) module loading").

:- set_prolog_flag(multi_arity_warnings, off).

:- export(multifile/1).
% (defined as multifile/2 with addmodule)
:- meta_predicate multifile(addmodule).

multifile(F/A, Mod) :-
    functor(_P, F, A), % Check if valid
    compile_clause(interpreted, 'multifile:$multifile'(Mod, F, A), 'basiccontrol:true', Mod).

:- set_prolog_flag(multi_arity_warnings, on).

:- export(module_loaded/4).
:- data module_loaded/4. % MODULE was loaded from SOURCE at TIME with MODE
                     % one of {interpreted, unprofiled, profiled}

:- export(static_module/1).
static_module(M) :-
    current_module(M),
    \+ module_loaded(M, _, _, _).

:- export(static_base/1).
static_base(Base) :-
    defines_module(Base, Module),
    static_module(Module).

:- export(use_mod/3).
% Use a module dynamically (incremental compilation and load)
% NOTE: internal, does not clean c_itf data afterwards
use_mod(File, Imports, ByThisModule) :-
    use_mod_common(File, module, Imports, ByThisModule).

use_mod_common(File, Type, Imports, ByThisModule) :-
    Fake_Base = executable(ByThisModule, File),
    store_imports(Imports, File, Fake_Base, 0, 0),
    new_in_mode(In),
    process_files_from_(File, In, Type,
                        load_compile, static_base, c_itf:false, needs_reload),
    del_in_mode(In),
    % (make sure that ItfLevel=1 data is available for gen_imports/1)
    base_name(File, Base),
    get_file_itf(1, Base, Type),
    itf_follow_reexports(Base),
    %
    gen_imports(Fake_Base),
    retractall_fact(imports_all(Fake_Base, _)),
    ( current_fact(module_error) ->
        message(error0, ['{Compilation aborted}']),
        signal_compilation_error,    
        retractall_fact(module_error),
        retractall_fact(imports_pred(Fake_Base, _, _, _, _, _, _)),
        discard_delayed_dynlinks
    ; base_name(File, Base),
      defines_module(Base, Module),
      include_dyn_imports(ByThisModule, Module, Fake_Base),
      ( make_delayed_dynlinks -> true % JFMC
      ; message(error0, ['{Dynamic link failed}'])
      ),
      do_initialization(Module)
    ).

:- export(use_mod_user/2).
% Like use_mod/3 but accepts user files.
use_mod_user(File, ByThisModule) :-
    use_mod_common(File, any, all, ByThisModule).

%% Old Code JF[]
% use_mod_user(File) :-
%         process_files_from_(File, in, any, 
%                           load_compile, static_base, false, needs_reload),
%       ( make_delayed_dynlinks -> true % JFMC
%       ; message(error0, ['{Dynamic link failed}']),
%         fail
%       ), !,
%         base_name(File, Base),
%         defines_module(Base, Module),
%         do_initialization(Module).
% use_mod_user(_) :- !, % JFMC
%       discard_delayed_dynlinks,
%       fail.

:- data needs_ini/1.

do_initialization(Module) :-
    findall(M, retract_fact(needs_ini(M)), L),
    comps_needs_ini(L),
    initialize_module(Module).

comps_needs_ini([]).
comps_needs_ini([M|Ms]) :-
    '$u'(N,M),
    retract_fact(initialized(N)), !,
    comps_needs_ini([N,M|Ms]).
comps_needs_ini([_|Ms]) :-
    comps_needs_ini(Ms).

:- data dyn_imports/2. % MODULE was imported dynamically from OTHERMODULE

include_dyn_imports(M, IM, B) :-
    retract_fact(imports_pred(B, _, F, N, _, _, E)),
    ( E = '.' -> EM = IM ; file_defines_module(E, EM) ),
%          assert_clause(imports(M, IM, F, N, EM), 'basiccontrol:true'),
      assert_clause('multifile:$imports'(M, IM, F, N, EM), 'basiccontrol:true'),
    fail.
include_dyn_imports(M, IM, _) :-
    asserta_fact(dyn_imports(M, IM)).

:- data load_action/4.

load_compile(Base) :- % JFMC
    load_so(Base),
    fail.
load_compile(Base) :-
    retract_fact(load_action(Base, Module, Mode, Load_Action)), !,
    do_load_action(Base, Module, Mode, Load_Action).
load_compile(Base) :-
    compute_load_action(Base, Module, Mode, Load_Action), !,
    do_load_action(Base, Module, Mode, Load_Action).
load_compile(_).

needs_reload(Base) :- % JFMC
    compute_load_action(Base, Module, Mode, Load_Action),
    asserta_fact(load_action(Base, Module, Mode, Load_Action)), !.
needs_reload(Base) :-
    load_so(Base),
    fail.

% Success if we need to read source and recompile, loads object and fails
% if we don't need to recreate object, else fails
compute_load_action(Base, Module, Mode, Load_Action) :-
    defines_module(Base, Module),
    itf_filename(Base, ItfName),
    modif_time0(ItfName, ItfTime),
    file_data(Base, PlName, _Dir),
    compilation_mode(PlName, Module, Mode),
    ( Mode = interpreted(_) ->
        ( modif_time0(PlName, PlTime),
          ( PlTime >= ItfTime -> FTime = PlTime ; FTime = ItfTime),
          not_changed(Module, Base, Mode, FTime) -> fail
        ; Load_Action = load_interpreted(PlName, Base, Module)
        )
    ; po_filename(Base, PoName),
      modif_time0(PoName, PoTime),
      ( ( ItfTime = 0, PoTime = 0 % (for read-only file systems)
        ; PoTime < ItfTime ) ->
          \+ not_changed(Module, Base, fail, _), % to give message
          Load_Action = load_make_po(Base, PlName, PoName, Mode, Module)
      ; not_changed(Module, Base, Mode, PoTime) -> fail
      ; abolish_module(Module),
        % TODO: debug, get ItfLevel=1 data (it should not be needed)
        %   get_file_itf(1, Base, Mode),
        %   itf_follow_reexports(Base),
        generate_multifile_data(Base, Module),
        qload_dyn(PoName, Module),
        retractall_fact(multifile(_,_,_,_)),
        asserta_fact(needs_ini(Module)),
        module_loaded_now(Module, Base, Mode),
        fail
      )
    ).

generate_multifile_data(Base, M) :-
    def_multifile(Base, F, A, Def),
      assertz_fact(multifile(M, F, A, Def)),
    fail.
generate_multifile_data(_,_).

:- meta_predicate do_load_action(?, ?, ?, goal).

do_load_action(Base, Module, Mode, Goal) :-
    abolish_module(Module),
    call(Goal),
    asserta_fact(needs_ini(Module)),
    module_loaded_now(Module, Base, Mode).

not_changed(Module, Base, Mode, Time) :-
    module_loaded(Module, OldBase, LoadTime, OldMode),
    ( OldBase = Base -> true
    ; message(note,
              ['redefining module ',Module,' from ',OldBase,' to ',Base]),
      fail
    ),
    OldMode = Mode,
    LoadTime >= Time.

module_loaded_now(Module, Base, Mode) :-
    retractall_fact(module_loaded(Module, _, _, _)),
    now(Now),
    assertz_fact(module_loaded(Module, Base, Now, Mode)).

% ---------------------------------------------------------------------------
:- doc(section, "Delayed dynamic link (for .so)").

:- data delayed_dynlink/3.
:- data dynlink_error/0.

%%  BUG: (???) THIS CODE DOESN'T WORK, the retract_fact makes weird things...
%% 
%% make_delayed_dynlinks :-
%%      retract_fact(delayed_dynlink(Base, Module, Decls)),
%%        so_filename(Base, SoName),
%%        ( build_foreign_interface_explicit_decls(Base, Decls),  
%%          file_exists(SoName),
%%          check_dynlink(SoName, Module) ->
%%          true
%%        ; abolish_module(Module),
%%          set_fact(dynlink_error)
%%        ),
%%      fail.
%% make_delayed_dynlinks :-
%%      \+ retract_fact(dynlink_error).

make_delayed_dynlinks :-
    reading_from(_), !,
    % Prevent make_delayed_dynlinks during read_record_file_/3,
    % which may happen during incremental compilation of programs
    % containing compilation modules and foreign interface (see
    % 'comp_fi_cm_reset' bug).
    %
    % NOTE: compilation modules cannot currently use (or depend on
    %   modules using) the foreign interface builder.
    true.
make_delayed_dynlinks :-
    retractall_fact(dynlink_error),
    findall(delayed_dynlink(Base, Module, Decls), delayed_dynlink(Base, Module, Decls), Xs),
    retractall_fact(delayed_dynlink(_, _, _)),
    make_delayed_dynlinks_2(Xs),
    \+ retract_fact(dynlink_error).

make_delayed_dynlinks_2([delayed_dynlink(Base, Module, Decls)|Xs]) :- !,
    make_delayed_dynlink(Base, Module, Decls),
    make_delayed_dynlinks_2(Xs).
make_delayed_dynlinks_2([]) :- !.

make_delayed_dynlink(Base, Module, Decls) :-
    so_filename(Base, SoName),
    ( build_foreign_interface_explicit_decls(Base, Decls) -> true ; fail ),
    file_exists(SoName), 
    check_dynlink(SoName, Module),
    !.
make_delayed_dynlink(_, Module, _) :-
    abolish_module(Module),
    set_fact(dynlink_error).

discard_delayed_dynlinks :-
    retractall_fact(delayed_dynlink(_, _, _)).

:- data foreign_library/2.

check_dynlink(SoName, Module) :-
    message(debug, ['Calling ',check_dynlink(SoName, Module)]),
    current_fact(foreign_library(Module, LastSoTime)), !,
    modif_time(SoName, SoTime),
    ( SoTime > LastSoTime ->
        retract_fact(foreign_library(Module, LastSoTime)),
        foreign_dynlink(SoName, Module),
        assertz_fact(foreign_library(Module, SoTime))
    ; true
    ).
check_dynlink(SoName, Module) :-
    message(debug, ['First time',''(check_dynlink(SoName, Module))]),
    modif_time(SoName, SoTime),
    foreign_dynlink(SoName, Module),
    assertz_fact(foreign_library(Module, SoTime)),
    message(debug, ['Asserted ',foreign_library(Module, SoTime)]),
    message(debug, ['Ended check_dynlink']).

check_dynunlink(Module) :-
    retract_fact(foreign_library(Module, _)),
    dynunlink(Module).

foreign_dynlink(_SoName, Module):-
    '$module_is_static'(Module), 
    !,
    % Do not load foreign code (already in the executable)
    %
    % TODO: make sure that this does not happen (bytecode must be static too)
    ( static_module(Module) ->
        true
    ; message(warning, ['foreign code of module ',Module,
                        ' already in executable, but the module is not static!'])
    ).
foreign_dynlink(SoName, Module):-
    % We change directory before loading a foreign library to be
    % able to load dependencies (e.g. third parties) relatively to
    % to ciao_root. 
    ( ciao_root(CiaoRoot), file_exists(CiaoRoot) -> 
        % TODO: is there a better way? this seems a bit weak
        working_directory(CurrentDir, CiaoRoot)
    ; working_directory(CurrentDir, CurrentDir)
    ),
    ( catch(dynlink(SoName, Module), Error, OK=exception(Error)) ->
        true
    ; OK=no
    ),
    working_directory(_, CurrentDir), 
    ( var(OK) -> true
    ; OK = exception(E) -> throw(E)
    ).

% Make foreign interface gluecode (if necessary)
:- use_module(exemaker, [needs_interface/2]).
make_gluecode(Base) :- % JFMC
    findall(X, decl(Base, X), Decls),
    ( do_interface(Decls) ->
        assertz_fact(needs_interface(Base, Decls))
    ; true
    ).

load_so(Base) :- % JFMC
    \+ current_fact(delayed_dynlink(Base, _, _)),
    findall(X, decl(Base, X), Decls),
    ( do_interface(Decls) ->
        defines_module(Base, Module),
        asserta_fact(delayed_dynlink(Base, Module, Decls))
    ; so_filename(Base, SoName),
      file_exists(SoName),
      defines_module(Base, Module),
%?          abolish_module(Module),
%?          assertz_fact(current_module(Module)),
      check_dynlink(SoName, Module)
    ).

% ---------------------------------------------------------------------------
:- doc(section, "Module unload").

:- trust pred do_on_abolish(G) : cgoal(G).
:- multifile do_on_abolish/1.

do_on_abolish(Head) :- retract_fact(pred_module(Head, _M)).

:- data renamed_multifile/4. % Predicate HEAD was renamed to F/A in MODULE
:- data pending_renamed_mf/4. % Pending renaming (generate wrapper clauses only when needed)

:- export(abolish_module/1).
abolish_module(Module) :-
    retract_fact(initialized(Module)),
    fail.
abolish_module(Module) :-
    retract_fact(dyn_imports(M, Module)),
    retract_clause('multifile:$imports'(M, Module, _, _, _), 'basiccontrol:true'),
    fail.
abolish_module(Module) :-
    expanded_multifile(_, P),
    functor(P, _, A),
    arg(A, P, Module),
    retract_clause(P, _),
    fail.
abolish_module(Module) :-
    retract_fact(pending_renamed_mf(_, Module, _, _)),
    fail.
abolish_module(Module) :-
    retract_fact(renamed_multifile(_, F, A, Module)),
    functor(Pred, F, A),
    '$abolish'(Pred),
    fail.
abolish_module(Module) :-
    retract_fact(pred_module(Pred, Module)),
    '$abolish'(Pred),
    fail.
abolish_module(Module) :-
    check_dynunlink(Module), % JFMC
    fail.
abolish_module(_).

:- export(unload_mod/1).
unload_mod(File) :-
    find_pl_filename(File, _, Base, _),
    retract_fact(module_loaded(Module, Base, _, _)),
    abolish_module(Module),
    % (delete cached itf)
    delete_time_of_itf_data(Base),
    delete_itf_data_opt(Base).

% ---------------------------------------------------------------------------
:- doc(section, "Compilation mode (interpreted, bytecode, etc.)").

:- export(interpret_file/1).
:- data interpret_file/1.   % SOURCE will be interpreted when loaded

:- export(interpret_module/1).
:- data interpret_module/1. % MODULE will be interpreted when loaded

:- export(interpret_srcdbg/1).
:- data interpret_srcdbg/1. % SRCDBG will be expanded when loaded

compilation_mode(_, Module, interpreted(srcdbg)) :- 
    current_fact(interpret_srcdbg(Module)), !.
compilation_mode(Source, Module, interpreted(raw)) :-
    (interpret_file(Source) ; interpret_module(Module)), !.
compilation_mode(_, _, Profiling) :-
    current_prolog_flag(compiling, Profiling).

% ---------------------------------------------------------------------------
:- doc(section, "Load actions for load_compile").

load_interpreted(Source, Base, Module) :-
    now_doing(['Consulting ',Source]),
    compiler_pass(Source, Base, Module, interpreted, _OK),
    end_doing,
    % TODO: unload module if OK is not 'yes'
    compute_pred_module(Module).

load_make_po(Base, Source, PoName, Profiling, Module) :-
    now_doing(['Compiling ',Source]),
    file_buffer_begin(PoName, Buffer, Stream),
    Mode = incoreql(Profiling),
    flatten_mod_name(Module, FlatModule), % TODO: use flat name in more places?
    reset_counter(FlatModule),
    set_compiler_mode_out(Mode, Stream),
    % TODO: unload module if OK is not 'yes'
    compiler_pass(Source, Base, Module, Mode, OK),
    ( OK = yes ->
        ( file_buffer_commit(Buffer) ->
            fmode(Source, FMode),
            chmod(PoName, FMode)
        ; true % TODO: show warning?
        )
    ; file_buffer_erase(Buffer) % TODO: keep previous version instead?
    ),
    % TODO: add an option for just 'incore'? (it was done before when .po was not writable)
    % Mode = incore(Profiling),
    % flatten_mod_name(Module, FlatModule), % TODO: use flat name in more places?
    % reset_counter(FlatModule),
    % set_compiler_mode(Mode),
    % % TODO: unload module if OK is not 'yes'
    % compiler_pass(Source, Base, Module, Mode, _OK)
    %
    end_doing,
    compute_pred_module(Module).

compute_pred_module(M) :-
    retract_fact(incore_mode_of(Head, Mode)),
      \+ Mode = multifile(_),
        asserta_fact(pred_module(Head, M)),
    fail.
compute_pred_module(_).

% ---------------------------------------------------------------------------
:- doc(section, "Loading of .po files (QL)").

qload_dyn(File, Module) :-
    now_doing(['Loading ',File]),
    '$push_qlinfo',
    '$open'(File, r, Stream),            % Gives errors
    ( qload_dyn_s(Stream, Module) -> true
    ; error_in_lns(_,_,warning,
                   ['could not load ', File,
                    ' (missing, corrupt, or wrong .po version)'])
    ),
    '$pop_qlinfo',
    cleanup_compilation_data,
    close(Stream),
    end_doing,
    compute_pred_module(Module).

qload_dyn_s(Stream, Module) :-
    '$qread'(Stream, Version),
    poversion(Version),
    repeat,
      '$qread'(Stream, Goal),
    ( Goal= -1
    ; ql_step(Goal, Module), fail
    ), !.

ql_step('internals:$set_currmod'(Mod), _Module) :- !,
    '$set_currmod'(Mod).
ql_step('internals:$define_predicate'(N/A, Profiling), Module) :-
    functor(Head, N, A), !,
    incore_mode(incore(Profiling), Head, Mode),
    handle_multifile_ql(Mode, Head, Profiling, Module).
ql_step('internals:$define_predicate'(Pred, Profiling), Module) :-
    ql_basepred(Pred, Base), !,
    ( get_renamed_multifile(Base, N, A, Module) ->
        subst_basepred(Pred, N/A, NewPred),
        '$define_predicate'(NewPred, Profiling)
    ; '$define_predicate'(Pred, Profiling)
    ).
ql_step('internals:$set_property'(Head,Prop), Module) :-
    ql_set_prop(Prop, Head, Module), !.
ql_step('internals:$interpreted_clause'(F/A,(H :- B)), Module) :-
    functor(Pred, F, A), !,
    ( get_renamed_multifile(Pred, N,_A, Module) -> % TODO: why _A?
        functor(NH, N, A),
        copy_args(A, H, NH),
        assert_clause(NH, B)
    ; assert_clause(H, B)
    ).
ql_step('internals:$compiled_clause'(Pred,Obj,Mode,Data), Module) :-
    ql_basepred(Pred, Base),  !,
    ( get_renamed_multifile(Base, N, A, Module) ->
        subst_basepred(Pred, N/A, NewPred),
        '$compiled_clause'(NewPred, Obj, Mode, Data)
    ; '$compiled_clause'(Pred,Obj,Mode,Data)
    ).
% % if gauge
% ql_step(install_clause_model(Pred/I,Counters),_Module) :-
%       ql_basepred(Pred, Base), !,
%       incore_mode_of(Base, incore(profiled)), 
%       install_clause_model(Pred/I, Counters).
% ql_step(install_insn_model(Pred/I,Counters),_Module) :-
%       ql_basepred(Pred, Base), !,
%       incore_mode_of(Base, incore(profiled)), 
%       install_insn_model(Pred/I, Counters).
ql_step(Goal, _) :-
    error_in_lns(_,_,warning, ['Invalid po item ',Goal]).

ql_set_prop(multifile, Head, Module) :-
    functor(Head, F, A),
    ( pending_renamed_mf(Head, Module, _, _) -> true % do nothing, wait until it really exists
    ; '$set_property'(Head, multifile) ->
        ( multifile_pred(F, F_),
          multifile(Module, F_, A, Dyn),
          low_dyn_decl(Dyn, Dynamic),
          member(Dynamic, [dynamic, concurrent]) ->
            '$set_property'(Head, Dynamic),
            IM = interpreted,
            OldMode = incore(_),
            NewMode = multifile(interpreted)
        ; OldMode = incore(IM),
          NewMode = multifile(OldMode)
        ),
        retract_fact(incore_mode_of(Head, OldMode)),
        asserta_fact(incore_mode_of(Head, NewMode)),
        handle_multifile_ql(NewMode, Head, IM, Module)
    ; '$predicate_property'(Head, _, Bits) ->
        check_multifile_type(Module, F, A, Bits)
    ; true
    ).
ql_set_prop(Prop, Head, Module) :-
    contains1([dynamic, concurrent], Prop),
    ( get_renamed_multifile(Head, N, A, Module) -> % TODO: this was on the 'else', why?
        retract_fact(incore_mode_of(Head, _)),
        asserta_fact(incore_mode_of(Head, multifile(interpreted))),
        functor(NHead, N, A),
        '$set_property'(NHead, Prop)
    ; '$set_property'(Head, Prop)
    ).

% TODO: backport 'regmod' and clause mark from optim-comp (it fixes multifile_chpt, multifile_retract bugs)
handle_multifile_ql(incore(_), _, _, _).
handle_multifile_ql(multifile(Mode), Head, IM, Module) :-
    % Just mark that we need a renamed clause
    asserta_fact(pending_renamed_mf(Head, Module, Mode, IM)).
%       get_renamed_multifile(Head, _N, _A, Module). % NOTE: uncomment to undo partial fix for multifile_chpt

% Get the renamed multifile, add wrapper the first time
get_renamed_multifile(Head, N, A, Module) :- 
    renamed_multifile(Head, N0, A0, Module),
    !,
    N = N0, A = A0.
get_renamed_multifile(Head, N, A, Module) :- 
    retract_fact(pending_renamed_mf(Head, Module, Mode, IM)),
    functor(Head, F, A),
    ( atom(Module) -> atom_concat([F, '$', Module, '$ql$'], Base)
    ; atom_concat(F, '$user$ql$', Base)
    ),
    new_mp_name(Base, N),
    functor(R, N, A), % TODO: compute in get_renamed_multifile instead
    copy_args(A, Head, R),
    add_multifile_clause(Head, R, Module, Mode),
    functor(R, N, A),
    '$define_predicate'(N/A, IM),
    asserta_fact(renamed_multifile(Head, N, A, Module)).


ql_basepred((F/_-_)/_, Base) :- !,
    ql_basepred(F, Base).
ql_basepred(N/A, Head) :-
    functor(Head, N, A).

subst_basepred((F/N-M)/L, R, (NF/N-M)/L) :- !,
    subst_basepred(F, R, NF).
subst_basepred(_, R, R).

% ---------------------------------------------------------------------------
:- doc(section, "Error messages").

doing_verbose(VF) :-
    current_prolog_flag(verbose_compilation, VF).

:- include(.(c_itf_messages)).

error_in_lns(L0, L1, Type, Msg) :-
    put_doing(Type),
    ( var(L0) -> message(Type, Msg)
    ; message_lns(Type, L0, L1, Msg)
    ).

% ---------------------------------------------------------------------------
:- doc(section, "Dynamic predicates (without module expansion)").

assert_clause(Head, Body) :-
    '$compile_term'([Head|Body], Ptr), 
    '$current_clauses'(Head, Root),
    '$insertz'(Root, Ptr).

retract_clause(Head, Body) :-
    '$current_clauses'(Head, Root), 
    '$current_instance'(Head, Body, Root, Ptr, no_block),
    '$erase'(Ptr),
    '$unlock_predicate'(Root).


