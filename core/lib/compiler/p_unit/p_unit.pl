:- module(p_unit, [
    preprocessing_unit/3,
    program/2,
    filtered_program_clauses/3,
    replace_program/2,
    %
    native_to_prop/2,
    prop_to_native/2,
    native_to_props_visible/2,
    dynamic_or_unknown_predicate/1,
    % Assertions
    get_assertion/2,
    add_assertion/1,
    % Directives
    add_directive/1,
    erase_directive/1,
    type_of_directive/2, % TODO: check (see code)
    % Predicate index
    pr_key_clean/0,
    pr_key_add/1, % TODO: only from p_unit (and tgd)
    pr_key_get/1, % TODO: only from p_printer (and tgd)
    add_defined_pred/2,
    new_internal_predicate/3,
    new_predicate/3,
    internal_predicate_names/1,
    predicate_names/1,
    multifile_predicate_names/1,
    %
    curr_language/1,
    %
    inject_output_package/1,
    %
    % TODO: move to clause_db or similar?
    add_output_package/1,
    get_output_package/1,
    add_output_operator/3,
    get_output_operator/3,
    % Comments
    add_comment/1,
    get_comment/1,
    cleanup_comment_db/0,
    % Commented (%) assertions
    cleanup_commented_assrt/0,
    get_commented_assertion/2,
    %
    cleanup_punit/0, % TODO: update with other cleanup_* preds here
    %
    get_call_from_call_assrt/7
], [assertions, basicmodes, regtypes, datafacts, hiord, nativeprops, define_flag]).

% ---------------------------------------------------------------------------
:- doc(title, "Preprocessing Unit Handling Library").

:- doc(author, "The Ciao Development Team").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Francisco Bueno").
:- doc(author, "Jose F. Morales"). % (moved and integrated into core,
                                   % fixes, optimizations, etc.)

:- doc(module, "This library provides predicates to load, access,
   modify, and pretty print module preprocessing units
   @cindex{preprocessing unit}. A preprocessing unit of a module is
   made up of the following elements:

   @begin{itemize}
   @item the module source (including dependencies, predicates,
     assertions, declarations, etc.),

   @item the assertions for the predicates directly imported by the module,

   @item the properties (and their assertions) defined in the modules
     @bf{transitively} imported by the module (up to a file that does not
     export any property).
   @end{itemize}

   This is similar to the information stored in @lib{compiler/c_itf}
   but extended to include the definitions of the exported and local
   properties transitively used by assertions of the exported
   predicates (and properties). This is necessary to be able to
   correctly interpret such assertions. When loading a preprocessing
   unit, this library generates @tt{.ast} files @cindex{.ast files},
   to cache this information.

   @begin{alert}
   Note that this is a @bf{superset} of the preprocessing unit, since:
   a) @tt{.ast} files may contain property definitions (including their
   assertions) not used in any assertion for an exported predicate
   b) not all predicates may be imported from a module (and then not all
   such properties may be actually needed)
   @end{alert}

   Other modules and files in this library:
   @begin{itemize}
   @item @lib{compiler/p_unit/p_unit_db}: database of the data
     collected by the predicates exported by this library (use with
     care).
   @item @lib{compiler/p_unit/p_printer}: pretty print the current
     module (source) stored in the program unit database.
   @item @lib{compiler/p_unit/p_unit_hooks}: (included file) 
     hook definitions to customize the behavior for treating native
     properties, regular types, printing program point information,
     etc.
   @end{itemize}
   ").

%% The preprocessing unit is made up of the file code, the assertion
%% interfaces of modules imported (which are called the @em{related
%% files}), and the definitions of the exported properties and of all
%% local properties transitively used by the exported properties for
%% files which export a property transitively used by one of the exported
%% properties of the related files.

% ---------------------------------------------------------------------------
% Wishlist and known bugs

:- doc(bug,"Further integration with c_itf, more control over
   process_files_from/7 so that internal calls are not needed, avoid
   restoring itf info (JF)").

% TODO: older list of bugs, review

:- doc(bug,"2. May clauses not of the current module be erased?.").
:- doc(bug,"3. Allow for native(regtype).").
:- doc(bug,"4. Properties native and regtype must now be used in assertions
    with a Pred which is a most general goal.").
:- doc(bug,"5. Every component creating new predicates should use 
    new_predicate here. Check.").
% Done when collecting them:
% :- doc(bug,"6. The calls assertion should be unique.").
:- doc(bug,"7. Builtin tables should be generated off-line!.").
% Seems ok now:
% :- doc(bug,"8. There might be info missing for imported predicates; e.g.,
%       the meta-predicate declarations.").
:- doc(bug,"9. Avoid the need for several native declarations. E.g.,
    for prop var(A) + ( native(free(A)), native(var(A)) ).").
:- doc(bug,"10. Meta terms should not be here.").
%:- doc(bug,"11. visible is not working").
:- doc(bug,"12. Type symbols is not what we want in native_prop").
:- doc(bug,"13. curr_language/1 is a bit naive.").
:- doc(bug,"14. Put make_prop_visible/1 to work.").
:- doc(bug,"15. At least \+ (maybe others?) is not expanded properly.
    See the current kludge in type_of_goal(imported,_) and
    type_of_goal(metapred,_). This shows up in, e.g., peephole.pl").
:- doc(bug,"16. Have a look to expansion of basic types. Things are rather
    strange now:
                 typedef('basic_props:num',[num]).
                 typedef('arit:arithexpression',[num,['basic_props:num']]).
           ").

% (p_asr bugs)
:- doc(bug, "1. Should expand module qualifications in the
   relevant directives: initialization, on_abort, ... (multifile, dynamic,
    data, and meta_predicate are handled via itf_db.").

:- doc(bug, "2. Should go into higher order properties and check the
   arguments for import/export also (and should probably look at the
   meta-predicate declarations for these)?").

:- doc(bug, "3. Opaque properties are not handled yet.").

:- doc(bug, "4. Save assertion heads WITH modes (non-normalized) and 
   normalize them only when asserting in the database.").

:- doc(bug, "5. No way of expanding when reading the .asr. Currently
   not reading them!").

:- doc(bug, "6. Add support for something like the ciaoc -u option.").

%% :- doc(bug,"7. Several copies of the same assertions remain in DB.").

%% Solved:
%% :- doc(bug,"10. Implicit importation of builtin modules from another 
%%      builtin module does not work: properties are not read in. This is 
%%      relevant when using package pure for properties cgoal/1 and iso/1 in
%%      basiccontrol. This was part of the previous bug: now solved.").

%% Solved with set_ciaopp_expansion(true)
%% :- doc(bug,"11. Things like this won't work:
%%    current_itf(imports,,(rt_module_exp(_483,fact,mmatrixpaco,-,fail,_488),
%%                          set_fact(_488)),_197)
%%    .").

:- doc(bug, "12. Should properties be defined only in terms of other
    properties? Currently, if this is not the case, predicates used
    in the definition of properties are not cached.").

:- doc(bug, "13. The modedef of parametric modules that may appear in the
    output will be wrong since call/2 is module expanded for the current
    module instead of for the proper hiord_rt:call/2.").

:- doc(bug, "14. When saving the assertions of dynamic.pl:
    WARNING: (lns 343-345) Predicate current_predicate/1 undefined 
    in source").

% :- doc( bug, "15. When loading an user file (no module
%    declaration), the error:
%    call basename(prelude.pl,user(/usr/cvs/Benchmarks/ciaopp/types/headunify))
%    ?  {ERROR: atomic_basic:atom_codes/2, arg 1 - expected atom, found
%    user(...)} appears" ).

% ---------------------------------------------------------------------------

:- use_package(library(compiler/p_unit/p_unit_argnames)).

:- use_module(library(assertions/c_itf_props)). % (documentation)

:- use_module(library(messages)).

:- use_module(library(aggregates), [findall/3, setof/3]).
:- use_module(library(compiler/c_itf), [opt_suffix/2, set_ciaopp_expansion/1]).

:- use_module(library(lists), [member/2]).
:- use_module(library(vndict),
    [create_dict/2, complete_dict/3, varnamedict/1, varnamesl2dict/2]).
:- use_module(engine(internals), [module_concat/3]).

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(compiler/p_unit/p_unit_db)).
:- use_module(library(compiler/p_unit/program_keys),
    [clause_key/2,cleanup_program_keys/0,rewrite_source_clause/3, clause/1]).
:- use_module(library(compiler/p_unit/native),
    [builtin/2, native_prop_map/3, native_prop_term/1, native_property/2]).

:- reexport(library(compiler/p_unit/p_unit_basic), [type_of_goal/2]).

:- use_module(library(compiler/c_itf), [cleanup_c_itf_data/0]).
:- use_module(library(compiler/p_unit/tr_syntax), [cleanup_tr_syntax/0, traverse_clauses/4]).

:- include(library(compiler/p_unit/p_unit_hooks)).

% ---------------------------------------------------------------------------

:- on_abort(set_ciaopp_expansion(false)). % TODO: (JF) be careful!

:- initialization(opt_suffix(_,'')). % TODO: (JF) be careful!

% ---------------------------------------------------------------------------
%! # Verbosity

:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(runtime_control), [current_prolog_flag/2]).

define_flag(verbose_p_unit, [on,  off], off).

:- export(p_unit_log/1).
p_unit_log(Message) :-
    current_prolog_flag(verbose_p_unit,on), !,
    message(inform, Message).
p_unit_log(_Message).

%% ---------------------------------------------------------------------------

:- use_module(library(hiordlib), [maplist/2]).

:- pred preprocessing_unit(Fs,Ms,E)
   :  list(filename,Fs) => (list(moddesc,Ms), switch(E))
    # "Loads the preprocessing unit of @var{Fs} defining @var{Ms}.".
:- pred preprocessing_unit(F,M,E)
   : filename(F) => ( moddesc(M), switch(E) )
    # "Loads the preprocessing unit of @var{F} defining @var{M}.".

preprocessing_unit(Fs,Ms,E):- Fs=[_|_], !,
    preprocessing_unit_list(Fs,Ms,E,[]).
preprocessing_unit(F,M,E):-
    preprocessing_unit_list([F],[M],E,[]).

preprocessing_unit_list(Fs,Ms,E,Opts):-
    cleanup_pasr,
    % TODO: fixme (see comment below)
    % init related files db for the closure
    %jcf%-Following comment is temporary (it is called from module/1 already)
    %jcf%       cleanup_c_itf_data, % cleanup data from c_itf:process_file/7
    %jcf%       cleanup_clause_db,
    %jcf%       cleanup_assrt_db,
%       cleanup_punit,
    set_ciaopp_expansion(true),
    % note: this includes splitting pred assertions into calls and success
    preprocessing_unit_internal(Fs, Ms, E, Opts),
%       assert_curr_modules(Ms), % TODO: expected in ctchecks_plot?
%       assert_curr_files(Fs,Ms),
    % identify and assert native props
    init_native_props,
    % setup type definitions
    init_types,
    % TODO: code seems to work without this; perhaps because some other ensure_registry_file; however it seems necessary at least to upload types from the registry (see patch_registry_file_/3)
    % TODO: this was done just before build_defined_types_lattice/0 (when current_pp_flag(types,deftypes)), is it fine here?
    % remove disjunctions and all that stuff
    % TODO: clean pr_key here! otherwise we remove a lot of stuff
    pr_key_clean,
    maplist(normalize_clauses, Ms),
    !,
    set_ciaopp_expansion(false). % TODO: move earlier?
preprocessing_unit_list(_Fs,_Ms,_E,_Opts):-
    set_ciaopp_expansion(false),
    fail.

% ---------------------------------------------------------------------------
%! # Preprocessing unit loading

:- compilation_fact(use_trans_opt).
:- compilation_fact(fast_c_itf).

:- use_module(library(lists), [member/2]).
:- use_module(library(ctrlcclean), [ctrlc_clean/1]).
:- use_module(library(errhandle),  [error_protect/2]).
:- use_module(library(compiler/c_itf), [
    process_file/7,
    comp_defines/1,
    defines_module/2, 
    false/1,
    module_error/0, module_error/1
]).
:- use_module(library(compiler/p_unit/assrt_norm)).
:- use_module(library(compiler/p_unit/p_canonical)).
:- use_module(library(compiler/p_unit/aux_filenames), [get_module_filename/3]).

:- data processed_file/1.
:- data related_file/1.
:- data irrelevant_file/1.
:- data file_included_by_package/1.
:- data warned_prop/3.
% (into ast files)
:- data mod_exports_no_props/1.
:- data mod_related_file/2.

:- pred cleanup_pasr # "Clean up all facts that preprocessing_unit_internal/4 asserts.".
cleanup_pasr :-
    retractall_fact(processed_file(_)),
    retractall_fact(related_file(_)),
    retractall_fact(irrelevant_file(_)),
    retractall_fact(file_included_by_package(_)),
    retractall_fact(warned_prop(_, _, _)),
    %
    retractall_fact(mod_exports_no_props(_)),
    retractall_fact(mod_related_file(_,_)).

:- export(there_was_error/1). % for intermod
% (error in c_itf:process_file/7)
there_was_error(yes) :- module_error, !.
there_was_error(yes) :- module_error(_), !.
there_was_error(yes) :- mexpand_error, !.
there_was_error(no).

% TODO: (review)
% DTM: When loading ast file, if we are adding the module to the
% output, i.e., we add one module information to the current one (see
% load_package_info/1), we have to add import fact in
% itf_db. adding_to_module specifies the original (first) loaded module 
:- data adding_to_module/1.

:- pred preprocessing_unit_internal(in(Fs), out(Ms), out(E), in(Opts))
    :: list(filename) * moddesc * switch * list
# "This is the core of the @concept{assertion reader/normalizer}. 
   It accepts some options in @var{Opts}. Also passes on the options
   in @var{Opts} to pass two of the assertion normalizer.

   With the default options it does the following:
   @begin{itemize}
   @item Reads all declarations and code in @var{Fs} and leaves everything asserted 
     in the database. Clauses are stored in @pred{clause_read/7}.
     Assertions are normalized and stored in @pred{assertion_read/9}.
     @var{Ms} are the modules names defined by @var{Fs}.

   @item Also, it reads and normalizes assertions @em{of the exported
     predicates} in all files related to each file in @var{Fs} (i.e.,
     imported by it, directly or by reexportation), leaving them also
     asserted by means of @pred{add_assertion_read/9}. All
     local property definitions which are transitively used by the
     exported properties of the related files are also stored in
     @pred{prop_clause_read/7}. If up to date @tt{.ast}
     files exist for any of the related files, the information is read
     directly from such @tt{.ast} files. @cindex{.ast files}
     Otherwise, the @tt{.pl} file is read and an up to date @tt{.ast}
     file is generated.

   @item The same processing of the previous paragraph is done also
     for files which export a property transitively used by one of the
     exported properties of the related files.
   @end{itemize}

   Since this predicate is intended for gathering file information for
   purposes which can be other than compilation to executable code
   (e.g., generating documentation or in the preprocessor) this
   predicate catches errors and proceeds in cases where file
   processing (e.g., during actual compilation) might normally abort.
   ".

%:- use_module(engine(runtime_control), [statistics/2]).

preprocessing_unit_internal(Fs, Ms, E, Opts) :-
    my_cleanup_c_itf_data,
    %statistics(walltime, [L1|_]),
    % process main file
    process_main_files(Fs, Opts, Ms),
    ( ( member(inject_pkg_into(Mod), Opts)
      ; member(Mod, Ms) % TODO: wrong! only first one?! (see related_files_closure/2 comment)
      ; Ms = [user(_)], Mod = [user] % TODO: why not a list?
      ) ->
        asserta_fact(adding_to_module(Mod)) % TODO: wrong! we may add to several modules! it should be in related_file/1
    ; retractall_fact(adding_to_module(_)) % TODO: review this
    ),
    % traverse the related files closure
    related_files_closure(direct, Opts), % TODO: Should we do it per M in Ms instead?
    retractall_fact(adding_to_module(_)),
    %% check for props in the related files
    delayed_prop_checks,
    %
    %statistics(walltime, [L2|_]),
    %Ld is L2-L1,
    %display(user_error, time_preprocessing_unit_internal(Fs,Ms,Ld,Opts)), nl(user_error),
    % any error upon loading?
    there_was_error(E).

process_main_files([], _Opts, []) :- !.
process_main_files([F|Fs], Opts, [M|Ms]) :- !,
    process_one_file(yes, F, direct, Opts, M),
    process_main_files(Fs, Opts, Ms).

% module M is (resp.) 
% processed/related/processed but irrelevant (a leave in the closure)

related_files_closure(Rel, Opts) :-
    current_fact(related_file(_)), !,
    related_files_closure_(Rel, Opts).
related_files_closure(_Rel, _Opts).

related_files_closure_(Rel, Opts) :-
    retract_fact(related_file(I)),
    \+ current_fact(processed_file(I)),
    \+ user_module(I),
    process_one_file(no, I, Rel, Opts, _M),
    fail.
:- if(defined(use_trans_opt)).
related_files_closure_(_Rel, Opts) :-
    ( member(load_irrelevant, Opts) -> Rel2 = direct
    ; Rel2 = trans
    ),
    related_files_closure(Rel2,Opts).
:- else.
related_files_closure_(_Rel, Opts) :-
    % TODO: %jcf%- To load only the directly related modules (for testing), just 
    %       %jcf%- comment out this line.
    %       related_files_closure(trans,Opts).
    related_files_closure(direct, Opts).
:- endif.

user_module(user). %% 'user' module cannot be treated as a normal module.

do_nothing(_).

%% this file have to assert related_file fact to be processed later.

% Process NF (IsMain=yes if it is the main file)
process_one_file(IsMain, NF, Rel, Opts, M) :-
    error_protect(ctrlc_clean(
            my_process_file(NF, asr, any,
                treat_one_file(IsMain, Rel, M, Opts),
                c_itf:false, asr_readable(IsMain, Rel, M), do_nothing)
        ),fail). % TODO: fail or abort?

:- meta_predicate my_process_file(+, +, +, pred(1), pred(1), pred(1), pred(1)).
:- if(defined(fast_c_itf)).
% TODO: process_file/7 cleans the cache, which is very inefficient for
%   treating recursive imports. process_file_from/7 is not flexible
%   enough to skip some imports and reconsider them later (which may
%   be needed for some import relations where a module is both direct
%   and indirect (due to another import) w.r.t. main). Extend c_itf to
%   allow these use cases, or export the internals.
:- import(c_itf, [cleanup_c_itf_data/0, get_base_name/4, process_file_/9, process_too/2]).
my_process_file(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
    c_itf:get_base_name(File, Base, Pl, Dir),
    c_itf:process_file_(Base, Pl, Dir, Mode, Type, TreatP, StopP, SkipP, RedoP),
    retractall_fact(c_itf:process_too(_,_)). % (not needed)
my_cleanup_c_itf_data :-
    c_itf:cleanup_c_itf_data.
:- else.
my_process_file(File, Mode, Type, TreatP, StopP, SkipP, RedoP) :-
    c_itf:process_file(File, Mode, Type, TreatP, StopP, SkipP, RedoP).
my_cleanup_c_itf_data.
:- endif.

treat_one_file(IsMain, Rel, M, Opts, Base) :-
%    ttt(treat_one_file(IsMain,Rel,M)),
%    message(inform, ['KKlog ', Base]),
    p_unit_log(['{Processing module ', Base, ' (main=', IsMain, ')']),
    del_compiler_pass_data, % (mexpand_error/0, location/3, ...)
    c_itf:defines_module(Base, M),
    assertz_fact(processed_file(Base)),
    assert_itf(defines_module, M, _, _, Base),
    % forces generation of defines/5 data (used below)
    ( IsMain = yes -> c_itf:comp_defines(Base) ; true ),
    % .ast file
    ( IsMain = no ->
        get_module_filename(asr, Base, AsrName),
        open_asr_to_write(AsrName),
        write_asr_fact(defines(M, Base)),
        save_itf_of_to_asr(Base, M)
    ; true
    ),
    % inhibits the expansion of meta-predicates % TODO: comment on IsMain=no, why?
    % (can not!) checks that properties are identifiable
    normalize_assertions(M, Base, Opts), % TODO: Opts was [] if IsMain=yes; why?
    % save clauses, assertions, and itf (everything expanded)
    activate_second_translation(Base, M),
    treat_one_file_exp(IsMain, M, Opts, Base),
    deactivate_second_translation(Base, M),
    end_brace_if_needed,
    % compute next related_file/1 layer
    assert_related_files(Base, M),
    % .ast file
    ( IsMain = no -> close_asr_to_write ; true ),
    % add itf facts to DB
    ( IsMain=yes, member(inject_pkg_into(TargetM), Opts) ->
        % For injected packages, save imports into TargetM and nothing else (JFMC)
        save_itf_injected_imports(M, TargetM)
    ; % TODO: should multifile be saved if IsMain=no?
      save_itf_info_of(Base, M, IsMain)
    ),
    p_unit_log(['}']),
    %
    fill_related(Rel, M).

% Check whether skip treatment for this module. As a side-effect, it
% loads the .ast file, which contains enough information for
% p_unit. Since the .itf info is not loaded, .ast loading feed back
% this information using c_itf:restore_defines/5 and
% c_itf:restore_imports/5.

% TODO: a bit hacky, better way?

% (only failure, .ast file is regenerated)
asr_readable(yes, _Rel, _M, _Base) :- !, % (main)
    fail. % Always reload for main file % TODO: save .exp files a-la optim-comp would speedup some use cases?
asr_readable(no, Rel, M, Base) :- % (related)
%    message(inform, ['KKlog-asr ', Base]),
    ( current_fact(processed_file(Base)) -> throw(bug(file_processed_twice(Base)))
    ; true
    ),
    assertz_fact(processed_file(Base)),
    get_module_filename(pl,  Base, PlName),
    get_module_filename(asr, Base, AsrName),
    file_up_to_date(AsrName, PlName),
    % display('Reading asr file '), display(AsrName), nl,
    read_asr_file(AsrName),
    c_itf:defines_module(Base, M),
    assert_itf(defines_module, M, _, _, Base),
    %
    fill_related(Rel, M).

fill_related(Rel, M) :-
    ( Rel = trans, mod_exports_no_props(M) ->
        % Mark as irrelevant, do not add related files
        assertz_fact(irrelevant_file(M)) % TODO: make sure that this is removed if the file is processed later as 'direct'
    ; % Add related files
      ( % (failure-driven loop)
        current_fact(mod_related_file(M, IMAbs)),
          add_related_file(IMAbs),
          fail
      ; true
      )
    ).

:- use_module(library(system), [modif_time/2]).

:- pred file_up_to_date(+AuxName,+PlName) : ( atm(AuxName), atm(PlName) )
# "Checks that the file named @var{AuxName} is up to date with respect
  to the file named @var{PlName} (@var{AuxName} modification time is
  later than @var{PlName}). It fails if any of the files does not
  exist.".
file_up_to_date(AuxName,PlName):-
    modif_time(AuxName, AuxTime),
    modif_time(PlName, PlTime),
    PlTime < AuxTime.

% ---------------------------------------------------------------------------

save_itf_info_of(Base, M, _IsMain) :-
    defines(Base, F, A, DefType, Meta),
    assert_itf(defines, M, F, A, M), % TODO: also for DefType=implicit? (see next clause)
    save_meta_dynamic(Meta, DefType, M, F, A),
    fail.
save_itf_info_of(Base, M, _IsMain) :-
    defines(Base, F, A, implicit, _Meta),
    assert_itf(impl_defines, M, F, A, M),
    fail.
save_itf_info_of(_Base, M, yes) :- % saving imported preds
    imports(M, IM, F, A, EM),
    ( (EM = '.' ; IM = EM) ->
        assert_itf(imports, M, F, A, IM) % IG define end module and reexported
    ; assert_itf(imports, M, F, A, r(IM,EM)), % TODO: needed for output
      assert_itf(imports, M, F, A, EM)
    ),
    %       save_meta_dynamic(Meta, DefType, M, F, A), %%% IG: here use meta_args
    fail.
% TODO: only for IsMain=yes?
save_itf_info_of(_Base, M, yes) :- % saving meta preds
    meta_args(M, Pred),
    functor(Pred, F, A),
    assert_itf(meta, M, F, A, Pred),
    fail.
% TODO: only for IsMain=yes?
save_itf_info_of(Base, M, yes) :- % saving dynamic/data/concurrent preds
    dyn_decl(Base, F, A, Decl),
    assert_itf(dynamic, M, F, A, Decl),
    fail.
% TODO: dyn_decl/4 is there as long as clause_of/7 is there. The same info is in
% defines/5... I am not sure which predicate is really alive at this phase.
save_itf_info_of(Base, M, yes) :-
    c_itf:exports(Base, F, A, _DefType, _Meta),
    assert_itf(exports, M, F, A, M),
    fail.
% TODO: only for IsMain=yes?
save_itf_info_of(Base, M, yes) :-
    def_multifile(Base,F,A,DefType),
    \+ c_itf_internal_pred(F,A),
    assert_itf(multifile, M, F, A, DefType),
    fail.
save_itf_info_of(_Base, _M, _IsMain).

% (M is the package wrapper module, FromM is the target module for injection)
save_itf_injected_imports(M, TargetM) :- % saving injected imports
    imports(M, IM, F, A, EM),
    ( (EM = '.' ; IM = EM) ->
        assert_itf(injected_imports, TargetM, F, A, IM) % IG define end module and reexported
    ; assert_itf(injected_imports, TargetM, F, A, r(IM,EM)), % TODO: needed for output
      assert_itf(injected_imports, TargetM, F, A, EM)
    ),
    %       save_meta_dynamic(Meta, DefType, M, F, A), %%% IG: here use meta_args
    fail.
save_itf_injected_imports(_M, _TargetM).

save_meta_dynamic(Meta, DefType, M, F, A) :-
    ( Meta\==0 -> assert_itf(meta, M, F, A, Meta)
    ; true
    ),
    ( ( DefType=dynamic ; DefType=data ; DefType=concurrent) ->
       assert_itf(dynamic, M, F, A, DefType)
    ; true
    ).

save_itf_of_to_asr(Base, M) :-
    c_itf:exports(Base, F, A, DefType, Meta),
    Meta \== 0,
%       c_itf:imports(M,_IM,F,A,EndMod),
    write_asr_fact(exports(M, F, A, DefType, Meta)),
    add_exports(M, F, A, DefType, Meta),
    fail.
save_itf_of_to_asr(_Base, _M).

add_exports(M, F, A, DefType, Meta) :-
    ( adding_to_module(CM) ->
        add_indirect_imports(CM, M, F, A) % TODO: needed?
    ; assert_itf(exports, M, F, A, M)
    ),
    restore_defines(M, F, A, DefType, Meta),
    save_meta_dynamic(Meta, DefType, M, F, A).

add_indirect_imports(CM, M, F, A) :-
    c_itf:restore_imports(CM, M, F, A, M), % TODO: wrong, this should be an indirect import!
    assert_itf(indirect_imports, CM, F, A, M).

%% ---------------------------------------------------------------------------
%! ## Compute one layer of related files for preprocessing unit closure

:- use_module(engine(stream_basic), [absolute_file_name/7]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(system), [working_directory/2]).
:- use_module(library(pathnames), [path_split/3]).

:- data seen_related_file/1. % (avoid writing related_file more than once)

assert_related_files(Base, M) :-
    ( check_mod_exports_no_props(Base, M) ->
       % Mark that the module exports no props (useful for Rel=trans)
       % the closure finalizes when there is no property exported
       assertz_fact(mod_exports_no_props(M)),
       write_asr_fact(mod_exports_no_props(M))
    ; true
    ),
    %
    retractall_fact(seen_related_file(_)),
    ( % (failure driven loop)
      imports_pred(Base, IM, _F, _A, _DefType, _Meta, _EndFile), % TODO: use IsMain; consider only props used in assertions and prop bodies if IsMain=no
      \+ user_module(IM),
        resolve_imported_path(Base, IM, IMAbs),
        ( current_fact(seen_related_file(IMAbs)) ->
            true
        ; asserta_fact(seen_related_file(IMAbs)),
          assertz_fact(mod_related_file(M, IMAbs)),
          write_asr_fact(mod_related_file(M, IMAbs))
        ),
        fail % (loop)
    ; true
    ).

add_related_file(IMAbs) :-
    \+ current_fact(processed_file(IMAbs)),
    \+ current_fact(related_file(IMAbs)),
    \+ mod_in_libcache(_, IMAbs),
    assertz_fact(related_file(IMAbs)),
    !.
add_related_file(_IMAbs).

% TODO: needed? always?
resolve_imported_path(Base, IM, IMAbs) :-
    file_path(Base, CWD),
    %%jcf% working_directory/2 is needed to evaluate .() notation.
    working_directory(OldCWD, CWD),
    absolute_file_name(IM, '_opt', '.pl', CWD, _, IMAbs, _),
    working_directory(_, OldCWD).
% OLD:
%       file_path(Base,Path),
%       working_directory(Old,Path),
%       absolute_file_name(IM,'','.pl','.',_,IMAbs,_),
%       working_directory(_Path,Old),

% TODO: check predicate (is '.' needed?)
file_path(Base,Path):-
    path_split(Base,Path0,_),
    ( Path0 = '' -> Path = '.' ; Path = Path0 ).

% Base,M is irrelevant if it does not have any exported prop
check_mod_exports_no_props(Base, M) :-
    relevant_prop(M, Prop),
    functor(Prop, F, A),
    c_itf:exports(Base, F, A, _DefType, _Meta),
    !,
    fail.
check_mod_exports_no_props(_Base, _M).

% TODO: all props in M are relevant according to this definition
relevant_prop(M, Prop) :-
    current_fact(assertion_of(PD, M, _, prop, _, _, _, _LB, _LE)),
    functor(PD,   F, A),
    functor(Prop, F, A).

% ---------------------------------------------------------------------------
%! ## Save clauses, assertions, operators, etc.

% Packages info:
%   is_syntax_package/1, is_included_by_default/1
:- include(.(p_asr_package_info)).

treat_one_file_exp(yes, M, Opts, Base) :- % (main)
    % treat assertions
    get_assertions_of(_, M, Assrt),
    compound_to_simple_assrt(Assrt, NAssrt),
    % Add assertions to DB
    add_assertions(NAssrt),
    % Save original pred assertions
    comment_original_pred_assertions(Assrt),
    % Save clauses
    ( member(inject_pkg_into(_), Opts) -> true % Do not add clauses
    ; assert_clauses_of(Base, M)
    ),
    % Save operators
    assert_operators_of(Base).
treat_one_file_exp(no, M, _Outs, Base) :- % (related)
    save_exported_assertions_of(Base, M),
    save_relevant_properties_of(Base, M).

add_assertions([]).
add_assertions([A|As]) :-
    add_assertion(A),
    add_assertions(As).

% we do have to process include directives. If one include directive
% belongs to a syntax pakage, then all things included from that
% directive will not be added to CiaoPP

assert_clauses_of(Base, _M) :-
    Body = include(File),
    db_directive_of(Base, Body, _, Source, _, _),
    % include directives that belong to a syntax package
    ( get_module_from_path(Source, Module),
        is_syntax_package(Module)
    ; % include directives included by include directives that
      % belongs to a syntax package
      file_included_by_package(Source)
    ),
    absolute_file_name(File,'_opt','.pl','.',_,AbsFile,_),
    atom_concat(AbsFile, '.pl', AbsSourceFile),
    asserta_fact(file_included_by_package(AbsSourceFile)),
    fail.
% --- DTM: make translations tables from source to module...
assert_clauses_of(Base, M) :-
    db_clause_of(_H, Base, M, Head, Body, VarNames, Source, Line0, Line1),
    has_to_be_asserted(M, Head, Body, Source),
    assertz_fact(clause_read(M,Head,Body,VarNames,Source,Line0,Line1)),
    fail.
assert_clauses_of(_Base, _M).

assert_operators_of(Base) :-
    Body = op(OP1, OP2, OP3),
    db_directive_of(Base, Body, _VarNames, _Source, _Line0, _Line1),
    add_output_operator(OP1, OP2, OP3),
    fail.
assert_operators_of(_Base).

get_assertions_of(Pred, M, As) :-
    findall(A, get_one_assertion_of(Pred, M, A), As), % TODO: this never fails!
    !.
get_assertions_of(_Pred, _M, []).

% Add pred assertions to the commented assertions DB. This is
% necessary only for the output.
comment_original_pred_assertions([]).
comment_original_pred_assertions([A|As]) :-
    A = as${type => pred},
    !,
    add_commented_assertion(A),
    comment_original_pred_assertions(As).
comment_original_pred_assertions([_|As]) :-
    comment_original_pred_assertions(As).

% include all from user
has_to_be_asserted(user(_), _Head, _Body, _Source) :- !.
% by default we include everything from our own module
has_to_be_asserted(Module, Head, Body, Source) :-
    get_module_from_path(Source, Module),
    !,
    \+
    ( number(Head),
        ( % include declarations are "included"/read by Ciao
            functor(Body, include, 1) -> true
        ; % use_package directives are not saved in CiaoPP either
            Body = use_package(Package), atom(Package) ->
              % TODO: atom/1 check added to preserve previous behaviour; review it!
              add_use_package(Package)
        ; fail
        )
    ).
%% a directive has to be keep iff it belongs to a package which
%% is not syntax one
has_to_be_asserted(_, Head, Body, Source) :- !,
    get_module_from_path(Source, Module),
    ( is_syntax_package(Module) ->
        % if it is a directive
        ( number(Head) ->
            % it is no necesary to add directives from syntax packages
            add_output_package(Module),
            fail
        ; error_message("Package ~w is said to be syntax " ||
                "package but has the clause: ~w :- ~w. " ||
                "The output will be incorrect.",
                [Module, Head, Body])
        )
    ; \+ is_included_by_default(Module), % not included by default
      \+ file_included_by_package(Source) % not included from a syntax package
    ).

% (for directive, which accepts lists)
add_use_package([]).
add_use_package([P|Ps]) :- !, add_output_package(P), add_use_package(Ps).
add_use_package(P) :- add_output_package(P).

:- use_module(library(pathnames), [path_basename/2, path_splitext/3]).

get_module_from_path(Path, Module) :-
    path_basename(Path, File),
    path_splitext(File, Module, _).

save_exported_assertions_of(Base, M) :-
    ( % (failure-driven loop)
      c_itf:exports(Base, F, A, _DefType, _Meta),
        functor(PD, F, A),
        write_and_save_assertions_of(PD, M),
        fail % (loop)
    ; true
    ).

save_relevant_properties_of(Base, M) :-
    ( % (failure-driven loop)
      relevant_prop(M, Prop),
        save_predicate_clauses_of(Base, M, Prop),
        functor(Prop, F, A),
        ( c_itf:exports(Base, F, A, _DefType, _Meta) -> true % (already saved)
        ; write_and_save_assertions_of(Prop, M)
        ),
        fail % (loop)
    ; true
    ).

write_and_save_assertions_of(P, M) :-
    get_assertions_of(P, M, Assrt),
    compound_to_simple_assrt_same_pred(Assrt, NAssrt),
    add_assertions(NAssrt),
    write_asr_assrts(NAssrt).

save_predicate_clauses_of(Base, M, Prop) :-
    db_clause_of(Prop, Base, M, Head, Body, VarNames, Source, Line0, Line1),
    write_asr_fact(prop_clause_read(M, Head, Body, VarNames, Source, Line0, Line1)),
    add_prop_clause_read(M, Head, Body, VarNames, Source, Line0, Line1),
    fail.
save_predicate_clauses_of(_Base, _M, _Prop).

write_asr_assrts([]).
write_asr_assrts([A|As]) :-
    A = as${
        module => M,
        head => ExpPD,
        compat => Co,
        call => Ca,
        succ => Su,
        comp => Cp,
        status => Status,
        type => Type,
        dic => Dict,
        comment => Cm,
        locator => Loc
    },
    Loc = loc(Source, LB, LE),
    assertion_body(ExpPD, Co, Ca, Su, Cp, Cm, Body1),
    % add_assrt_indirect_imports(M, Body1), % TODO:[see issue #576] originally disabled; both should be disabled or enabled to ensure a consistent behavior
    write_asr_fact(assertion_read(ExpPD, M, Status, Type, Body1, Dict, Source, LB, LE)),
    write_asr_assrts(As).

% ---------------------------------------------------------------------------
%! ## Module expansion of clauses and assertions

% TODO: merge c_itf here, merge second pass with compiler

:- use_module(library(compiler/c_itf), [
    activate_translation/3,
    module_expansion/9, location/3,
    clause_of/7,
    defines/3, defines/5, def_multifile/4,
    c_itf_internal_pred/2, c_itf_internal_pred_decl/1,
    exports/5, imports_pred/7,
    end_goal_trans/1,
    restore_defines/5,
    restore_imports/5,
    % restore_multifile/4, % TODO: not used!
    imports/5,
    meta_args/2,
    dyn_decl/4
]).
% TODO: export these predicates or promote it internally as
%       module_error, at c_itf
:- import(c_itf, [
    mexpand_error/0,
    del_compiler_pass_data/0,
    end_brace_if_needed/0,
    assr_head_expansion/7
]).
:- use_module(library(compiler/translation), [
    expand_clause/6, del_goal_trans/1, del_clause_trans/1
]).
:- use_module(library(formulae), [asbody_to_conj/2]).

%% --- DTM: The Dict should be vnDict (to complete variables and unify with 
%%          clauses one)

get_one_assertion_of(PD, M, As2) :-
    current_fact(assertion_of(PD,M,Status,Type,Body0,Dict,Source,LB,LE)),
    %Type \== test, % Skip tests assertions, not processed here
    assertion_body(PD, Co, Ca, Su, Cp, Cm, Body0),
    LOC = loc(Source, LB, LE),
    head_expand(Type, PD, M, Dict, ExpPD, LOC), % TODO: rename these predicates
    expand_subbody(Co, M, Dict, ECo, LOC),
    expand_subbody(Ca, M, Dict, ECa, LOC),
    expand_subbody(Su, M, Dict, ESu, LOC),
    expand_subbody(Cp, M, Dict, ECp, LOC),
    As2 = as${
        module => M,
        head => ExpPD,
        compat => ECo,
        call => ECa,
        succ => ESu,
        comp => ECp,
        status => Status,
        type => Type,
        dic => Dict,
        comment => Cm,
        locator => Loc
    },
    Loc = loc(Source, LB, LE).

% We process modedef and true here to avoid warnings about undefined
% predicates. --EMM
not_allow_external(modedef, _).
not_allow_external(_,       true). % True is not expanded

% TODO: see c_itf:assr_head_expansion/7, we should not do goal expansions of assertion heads
head_expand(Type, PD, M, Dict, ExpPD, loc(Source, LB, LE)) :-
    not_allow_external(Type, PD),
    !,
    functor(PD, F, A),
    ( functor(Meta, F, A), meta_args(M, Meta) -> true ; Meta = 0 ),
    module_expand(PD, true, M, Dict, ExpPD0, _, Source, LB, LE),
    fix_assrt_head(ExpPD0, Meta, M, ExpPD),
    functor(PD, F, A),
    % TODO: doing this is dangerous (JF)
    ( c_itf:defines(M, F, A) -> true ; assertz_fact(c_itf:defines(M, F, A)) ).
% Using module_expand in this way allows us to write assertions of
% predicates that are in other modules: --EMM
head_expand(_, PD, M, _Dict, ExpPD, _Loc) :- % _Loc=loc(Source, LB, LE)
    functor(PD, F, A),
    % TODO: Treat addmodule(_) or addterm(_) meta? it is not done in c_itf but it was done in the fix_assrt_head version below
    assr_head_expansion(PD, M, F, A, _, ExpPD, _).
    % % TODO: expanded as goal, not as head! this is incorrect (JF)
    % ( functor(Meta, F, A), meta_args(M, Meta) -> true ; Meta = 0 ),
    % module_expand(true, PD, M, Dict, _, ExpPD0, Source, LB, LE),
    % fix_assrt_head(ExpPD0, Meta, M, ExpPD).

% TODO: do not remove this code yet! used in 'modedef'
fix_assrt_head(ExpPD0, 0, _M, ExpPD) :- !,
    ExpPD = ExpPD0. % no meta, no fix
fix_assrt_head(ExpPD0, Meta, M, ExpPD) :-
    ExpPD0 =.. [F|Args],
    Meta =.. [_|MetaArgs],
    fix_assrt_args(Args,MetaArgs,M,NArgs),
    ExpPD =.. [F|NArgs].

fix_assrt_args([],[],_M,[]).
fix_assrt_args([A,_|Args],[MetaArg|MetaArgs],M,[A,_|NArgs]) :-
    ( MetaArg = addmodule(_) ; MetaArg = addterm(_) ),
    !,
    % replace next arg with a fresh var so that the head is normalized
    fix_assrt_args(Args,MetaArgs,M,NArgs).
fix_assrt_args([A|Args],[_MetaArg|MetaArgs],M,[NA|NArgs]) :-
    ( var(A) -> NA = A
    ; A = 'hiord_rt:call'(X) -> NA = X
    ; NA = A % should not happen
    ),
    fix_assrt_args(Args,MetaArgs,M,NArgs).

expand_subbody(C, M, Dict, CO, loc(Source, L0, L1)) :-
    asbody_to_conj(C, CC),
    module_expand(in_assertion_body, CC, M, Dict, _, EC, Source, L0, L1),
    asbody_to_conj(CO, EC).

db_directive_of(Base, Body, VarNames, Source, Line0, Line1) :-
    clause_of(Base, Head, Body, VarNames, Source, Line0, Line1),
    number(Head),
    \+ c_itf_internal_pred_decl(Body).

db_clause_of(Head, Base, M, H, B, VarNames, Source, Line0, Line1) :-
    clause_of(Base, Head, Body, VarNames, Source, Line0, Line1),
    ( number(Head) ->
        \+ c_itf_internal_pred_decl(Body),
        H = Head,
        B = Body
    ; module_expand(Head,Body,M,VarNames,H,B,Source,Line0,Line1)
    ).

activate_second_translation(Base, M) :-
    activate_translation(Base, M, add_clause_trans),
    activate_translation(Base, M, add_goal_trans),
    expand_clause(0, 0, M, _, _, _). % Translator initialization

deactivate_second_translation(_Base, M) :-
    end_goal_trans(M),
    del_goal_trans(M),
    del_clause_trans(M).

module_expand(Head, Body, M, VarNames, H, B, Source, Line0, Line1) :-
    ( module_expansion(Head,Body,M,VarNames,asr,Source,Line0,Line1,_,_,H,B) ->
        (VarNames = [], ! ; true)
    ;
%%          error_message( loc(Source,Line0,Line1),
%%                         "Unable to expand~n  ~q :- ~q",[Head,Body]),
%% DTM: just trying the pretty printer!
%% --- DTM: this is an assertion, not a clause
        error_message(loc(Source, Line0, Line1),
            "INTERNAL ERROR: Unable to expand~n  ~p",
            ['$clause'(Head, Body, VarNames)]),
% Error recovery
        Body=B,
        Head=H
    ).

module_expansion(H, B, Module, Dict, Mode, Src, Ln0, Ln1, H1, B1, H2, B2):-
    asserta_fact(location(Src,Ln0,Ln1), Ref),
    ( c_itf:module_expansion(H, B, Module, Dict, Mode, H1, B1, H2, B2) ->
        true
    ; display(user_error,internal_error(H,B)), nl(user_error) % TODO: wrong
    ),
    erase(Ref).

% ---------------------------------------------------------------------------
%! ## Read/write asr files

:- pred ast_version/1 :: atm
# "Contains a version number which identifies
   the @tt{.ast} files associated with this version of the assertion
   library. Should be changed every time changes are made which render
   the @tt{.ast} files incompatible, since this forces recomputation
   of all such files.".

ast_version('5.0').

% Note: use `ciaodump` to show .ast files (fastrw format)

:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(read), [read/2]).
:- use_module(engine(runtime_control), [module_split/3]).
:- use_module(library(fastrw), [fast_read/2, fast_write/2]).

read_asr_file(AsrName) :-
    catch(open(AsrName, read, Stream), error(_,_), fail),
    (
        ast_version(V),
        read(Stream, v(V)),
        !,
        p_unit_log(['{Reading ', AsrName]),
        read_asr_data_loop(AsrName, Stream),
        close(Stream),
        p_unit_log(['}'])
    ;
        p_unit_log(['{Old version in ', AsrName, '}']),
        close(Stream),
        fail
    ).

%% fast_read/1 version (just fails at end of file)
read_asr_data_loop(F, Stream) :-
    ( fast_read(Stream, X) ->
        ( read_asr_data_loop__action(X) -> true
        ; error_message("ERROR PROCESSING FACT ~w from ast file ~w", [X, F])
        ),
        read_asr_data_loop(F, Stream)
    ; true
    ).

read_asr_data_loop__action(defines(M, Base)) :- !,
    assert_itf(defines_module, M, _, _, Base).
read_asr_data_loop__action(mod_related_file(M, IMAbs)) :- !,
    assertz_fact(mod_related_file(M, IMAbs)).
read_asr_data_loop__action(exports(M, F, A, DefType, Meta)) :- !,
    add_exports(M, F, A, DefType, Meta).
read_asr_data_loop__action(mod_exports_no_props(M)) :- !,
    assertz_fact(mod_exports_no_props(M)).
read_asr_data_loop__action(X) :- X = assertion_read(A1, A2, A3, A4, A5, A6, A7, A8, A9), !,
    % X = assertion_read(_, M, _, _, Body, _, _, _, _),
    % add_assrt_indirect_imports(M, Body), % TODO:[see issue #576] originally enabled; both should be disabled or enabled to ensure a consistent behavior
    add_assertion_read(A1, A2, A3, A4, A5, A6, A7, A8, A9).
read_asr_data_loop__action(X) :- X = prop_clause_read(A1, A2, A3, A4, A5, A6, A7), !,
    add_prop_clause_read(A1, A2, A3, A4, A5, A6, A7).

% % TODO:[see issue #576]
% add_assrt_indirect_imports(M, AssrtBody) :-
%     ( adding_to_module(CM) ->
%         assertion_body(Head, _, _, _, _, _, AssrtBody),
%         functor(Head,   MF, A),
%         functor(Head__, MF, A),
%         ( current_itf(imports(CM,_), Head__, M) ->
%             true
%         ; module_split(MF, _, F),
%           add_indirect_imports(CM, M, F, A)
%         )
%     ;
%         true
%     ).

:- data asr_stream/1. % (enable asr write)

write_asr_header(S) :-
    ast_version(V),
    displayq(S, v(V)),
    display(S, ' .\n').

% (Note: not saved for main file)
write_asr_fact(X) :- current_fact(asr_stream(S)), !,
    fast_write(S, X).
write_asr_fact(_).

open_asr_to_write(AsrName) :-
    retractall_fact(asr_stream(_)),
    ( catch(open(AsrName, write, Stream), error(_,_), fail) ->
        set_fact(asr_stream(Stream)),
        write_asr_header(Stream)
    ; true % (asr storate silently disabled if file cannot be opened)
    ).

close_asr_to_write :-
    ( current_fact(asr_stream(Stream)) ->
        close(Stream),
        retractall_fact(asr_stream(_))
    ; true
    ).

% ---------------------------------------------------------------------------
%! ## Checking that assertion properties are really properties

delayed_prop_checks :- % TODO:[SLOW] do not repeat checking of all modules everytime
    assertion_read(PD, M, _Status, Type, Body, _Dict, S, LB, LE),
    \+ current_fact(irrelevant_file(M)),
    \+ Type = modedef,
    \+ Type = test,
    functor(PD, F, A),
    assertion_body(_NPD, CNDP, CNCP, CNAP, CNGP, _CO, Body),
    Where = loc(S, LB, LE),
    check_properties(CNDP, F, A, M, Where),
    check_properties(CNCP, F, A, M, Where),
    check_properties(CNAP, F, A, M, Where),
    check_properties(CNGP, F, A, M, Where),
    fail.
delayed_prop_checks.

check_properties([], _F, _A, _M, _Where) :- !.
check_properties([(P1;P2)], F, A, M, Where) :- !,
    check_properties(P1, F, A, M, Where),
    check_properties_special_case(P2, F, A, M, Where).
check_properties([Prop|Props], F, A, M, Where) :- !,
    functor(Prop, PF, PA),
    check_property(PF, PA, Prop, F, A, M, Where),
    check_properties(Props, F, A, M, Where).
check_properties(PROP, F, A, M, Where) :-
    throw(error(expecting_list_of_props(PROP,F/A,M,Where), check_properties/5)).

% Here is the case:
%
%  The body assertion _type_ is a list. Then ';' were introduced and
% things like [A;B], with A, B lists, are now accepted.  A problem
% araise when we have something like [A;B;C].  As ';' works like a
% functor, we got ';'(A,(B;C)), with A,B and C list, _BUT_ in:
%
% check_properties( [(P1;P2)],F,A,M,Where):-
%       !,
%       check_properties(P1,F,A,M,Where),
%       check_properties(P2,F,A,M,Where).
%
% P2 is (B;C) so it is not a list!.
%
% Then, here we have the special case:
check_properties_special_case((P1;P2), F, A, M, Where) :- !,
    check_properties(P1, F, A, M, Where),
    check_properties_special_case(P2, F, A, M, Where).
check_properties_special_case(P1, F, A, M, Where) :-
    check_properties(P1, F, A, M, Where).

check_property(call, _PA, _Prop, _F, _A, _M, _Where) :- !.
check_property(';', 2, ';'(A, B), _F, _A, _M, _Where) :-
    check_properties(A, _F, _A, _M, _Where),
    check_properties(B, _F, _A, _M, _Where),
    !.
check_property(PF, PA, _Prop, _F, _A, _M, _Where) :-
    functor(PD, PF, PA),
    assertion_read(PD, _AM, _Status, prop, _Body, _Dict, _S, _LB, _LE),
    !.
check_property(PF, PA, _Prop, _F, _A, M, _Where) :-
    warned_prop(PF, PA, M),
    !.
check_property(PF, PA, _Prop, F, A, M, Where) :-
    ( module_split(PF, M, _),
      current_itf(defines_module, M, _) ->
        warning_message(Where,
            "~w used in an assertion for ~w in ~w is not a property",
            [PF/PA, F/A, M])
    ; note_message(Where,
          "~w used in an assertion for ~w in ~w has not been loaded (this is a bug)",
          [PF/PA, F/A, M])
    ),
    asserta_fact(warned_prop(PF, PA, M)).

% ---------------------------------------------------------------------------
%! # Cleanup

:- pred cleanup_punit # "Clean up all facts that p_unit asserts.".
cleanup_punit :-
    cleanup_punit_local, %local
    cleanup_pasr, %local
    cleanup_c_itf_data, % cleanup data from c_itf:process_file/7
    cleanup_p_unit_db, %local
    %
    cleanup_commented_assrt, %local
    cleanup_comment_db, %local (+codegen_pcpe)
    pr_key_clean. %local (+pr_order_set)

cleanup_punit_local :-
    cleanup_program_keys,
    cleanup_tr_syntax,
    %
    retractall_fact(pl_output_op(_, _, _)),
    retractall_fact(pl_output_package(_)),
    %
    retractall_fact(internal_pred_name(_,_,_)),
    retractall_fact(p_unit:native(_,_)),
    retractall_fact(regtype(_,_)). % local data

% assert_curr_files([],[]).
% assert_curr_files([A|As],[M|Ms]):-
%       just_module_name(A,M),
%       asserta_fact(curr_file(A,M)),
%       assert_curr_files(As,Ms).

%% ---------------------------------------------------------------------------

% Fill type definition for all regtypes
init_types :-
    enum_regtype(Head,Prop),
    get_module_from_sg(Head,Module),%% JCF
    \+ mod_in_libcache(Module,_),  %% JCF: preloaded modules are processed already.
    % definable (i.e., not basic --top, num, etc.-- not [] nor [_|_])
    hook_legal_regtype(Head),
    ( Head==Prop ->
        findall((Head:-Body),
                ( one_type_clause(Head,Body0),
                  unexpand_meta_calls(Body0,Body)
                ), Cls)
    ; Cls=[(Head:-Prop)]
    ),
    ( Cls=[] -> true
    ; hook_insert_regtype(Head,Cls)
    ),
    fail.
init_types :-
    ( hook_post_init_regtypes -> true ; true ).

one_type_clause(Head,Body):-
    clause_read(_,Head,Body,_VarNames,_Source,_LB,_LE).
one_type_clause(Head,Body):-
    % in other (imported) modules
    prop_clause_read(_,Head,Body,_VarNames,_Source,_LB,_LE).

%% ---------------------------------------------------------------------------

% TODO: change name?
:- pred program(P,D) : var * var
   => list(clause) * list(varnamedict)
    # "@var{P} are the clauses (no directives) of the current module
       and @var{D} their dictionaries.".
program(P,D):-
    findall((clause(H,B),Key,Dict),
             current_fact(source_clause(Key,clause(H,B),Dict)),
            P0),
    split1(P0,P,D).

:- pred filtered_program_clauses(+Mods,-P,-D)
   : (list(Mods))
   => list * list(clause) * list(varnamedict)
   #"@var{P} are the clauses of module @var{Mod} and @var{D} their dictionaries.".
filtered_program_clauses(Mods,P,D) :-
    findall((clause(H,B),Key,Dict),
            filtered_source_clause(Mods,Key,clause(H,B),Dict),
             P0),
    split1(P0,P,D).

:- pred filtered_source_clause(+list,-,?,-).
filtered_source_clause(Mods,Key,clause(H,B),Dict) :-
    current_fact(source_clause(Key,clause(H,B),Dict)),
    functor(H,F,A),
    functor(Sg,F,A),
    get_pred_mod_defined(Sg,Mod),
    ( member(Mod,Mods) -> true ; fail ).

:- export(get_pred_mod_defined/2).
:- pred get_pred_mod_defined(+Sg,-Mod) + nondet
   #"Returns the module where a predicate given by goal @var{Sg} is defined. If
    the @var{Sg} is multifile, the modules where it is defined are enumerated.".
get_pred_mod_defined(Sg,Mod) :-
    current_itf(defines_pred,Sg,Mod), !.
get_pred_mod_defined(Sg,Mod) :-
    current_itf(multifile,Sg,Mod).

split1([],[],[]).
split1([(Cl,K,D)|P0s],[Cl:K|Ps],[D|Ds]):-
    split1(P0s,Ps,Ds).

normalize_clauses(M):-
    findall((Cl,Key,Dict),program_clause(M,Cl,Key,Dict),P0),
    split(P0,Cls0,Ds0),
    % --- DTM: This should be separated into 2 tranforms: one to
    %          remove cuts and another to remove disjuntions.
    % TODO: why twice? reuse compiler code instead?
    traverse_clauses(Cls0,Ds0,Cls1,Ds1), % TODO: delay clause_key/2 if traverse_clauses/4 do not need them
    traverse_clauses(Cls1,Ds1,Cls,Ds),
    assert_program(Cls,Ds).

program_clause(M,Cl,Key,Dict):-
    retract_fact(clause_read(M,Head,Body,VarNames,Source,LB,LE)),
    ( number(Head) ->
        Cl=directive(Body)
    ;
        Cl=clause(Head,Body)
    ),
    % create a clause id and a reference to program source
    % TODO: include somewhere else?
    clause_key(Head,Key), % TODO: this has a counter!
    add_clause_locator(Key,loc(Source,LB,LE)),
    % TODO: "El diccionario de variables de c_itf no esta completo!!" is it ok?
    ( VarNames=[] ->
        create_dict((Head,Body),Dict)
    ; varnamesl2dict(VarNames,Dict0),
      complete_dict(Dict0,(Head,Body),Dict)
    ).

split([],[],[]).
split([(Cl,K,D)|P0s],[Cl:K|Ps],[D|Ds]):-
    split(P0s,Ps,Ds).

assert_program([],[]).
assert_program([Cl:Key|Cls],[D|Ds]):-
    rewrite_source_clause(Cl,Key,Clause), % TODO: use rewrite_source_all_clauses/2?
    add_clause(Key,Clause,D),
    assert_program(Cls,Ds).

% ---------------------------------------------------------------------------

:- pred replace_program(P,D) : list(clause) * varnamedict # "The
   database holding the program is updated by first deleting its
   contents and then adding the clauses in @var{P} and dictionaries in
   @var{D}.".
replace_program(Cls,Ds):-
    % TODO: only clauses, not directives; is it OK? (change done by 'jfc')
%jcf%   retractall_fact(source_clause(_,_,_)),
    pr_key_clean,
    retractall_fact(source_clause(_,clause(_,_),_)),
    add_all_clauses(Cls,Ds).

add_all_clauses([],[]).
add_all_clauses([Cl:Key|Cls],[D|Ds]):-
    add_clause(Key,Cl,D),
    add_all_clauses(Cls,Ds).

add_clause(Key,Cl,D) :-
    assertz_fact(source_clause(Key,Cl,D)),
    generate_pr_key(Cl).

%% ---------------------------------------------------------------------------

% TODO: (JF) this requires caching and it must be depend on the module
:- doc(hide,curr_language/1).
curr_language(clp):- % TODO: not steadfast!
    current_fact(source_clause(_Key,directive(impl_defined('.=.'/2)),_D)),
    !.
curr_language(lp).

%% ---------------------------------------------------------------------------

:- use_module(library(terms_check), [variant/2]).

:- pred get_call_from_call_assrt(Sg,M,Status,Call,Source,LB,LE)
   # "Returns in @var{Call}, upon backtracking call patterns from calls
   assertions related to @var{Sg}, in module @var{M}. Also takes care of 
 disjunctions.".
% Removes call patterns for which there exists a success
get_call_from_call_assrt(Sg,M,Status,OneCall,Source,LB,LE) :-
    assertion_read(Sg,M,Status,calls,Body,_Dict,Source,LB,LE),
    assertion_body(Sg,_Compat,FullCall,_Succ,_Comp,_Comm,Body),
    get_single_call(FullCall,OneCall),
    filter_call(OneCall,Sg).

get_single_call([(A;As)],AOut):-!,
    get_one_disjunct((A;As),AOut).
get_single_call(A,A).

get_one_disjunct((A;_),A) :- !.
get_one_disjunct((_;As),A):- !,
    get_one_disjunct(As,A).
get_one_disjunct(A,A).

% Do not take call patterns for which there exists success P:C => S
filter_call(Call,Sg) :- 
    copy_term((Sg,Call),(CpSg,CpCall)),
    assertion_read(CpSg,_M,_Status,success,Body,_Dict,_Source,_LB,_LE),
    assertion_body(CpSg,_Compat,Call_x,_InfoSucc,_Comp,_Comm,Body),
    variant(CpCall,Call_x),
    !,fail.
filter_call(_,_).

%% ---------------------------------------------------------------------------
 % TODO: Not imported anywhere and not used here, remove??
%% :- pred exit_assertion(Goal,Call,Succ) :: cgoal(Goal)
%%    # "There is an exit assertion for @var{Goal} with call
%%    pattern @var{Call} and success pattern @var{Succ}.".
%% exit_assertion(Goal,Call,Succ):-
%%     ( Type=pred ; Type=success ),
%%     ( Status=true ; Status=trust ),
%%     assertion_read(Goal,_M,Status,Type,Body,_Dict,_S,_LB,_LE),
%%     assertion_body(Goal,_Compat,Call,Succ,_Comp,_Comm,Body).

%% ---------------------------------------------------------------------------
:- pred dynamic_or_unknown_predicate(Goal)
   # "@var{Goal} is an atom for a predicate such that all its clauses might not
   be available or may change in the program unit.".
dynamic_or_unknown_predicate(Goal):- type_of_goal(imported,Goal), !.
% TODO: for imported extract module name from goal and check if is loaded.
dynamic_or_unknown_predicate(Goal):- type_of_goal(dynamic,Goal), !.
dynamic_or_unknown_predicate(Goal):- type_of_goal(multifile,Goal), !.
dynamic_or_unknown_predicate(Goal):- type_of_goal(impl_defined,Goal), !.

% ---------------------------------------------------------------------------
%! # Assertions

get_assertion(Goal, As) :-
    As = as(M,Status,Type,Head,Compat,Call,Succ,Comp,Dic,Loc,Comment,_),
    Loc = loc(S, LB, LE),
    assertion_read(Goal, M, Status, Type, Body, Dic, S, LB, LE),
    assertion_body(Head, Compat, Call, Succ, Comp, Comment, Body).

:- export(assertion_set_status/3).
assertion_set_status(X0, Status, X) :-
    X0 = as(M,_,Type,Head,Co,Calls,Success,Comp,Dic,Loc,Comm,Fromwhere),
    X = as(M,Status,Type,Head,Co,Calls,Success,Comp,Dic,Loc,Comm,Fromwhere).

:- export(assertion_set_head/3).
assertion_set_head(A0, Head, A) :-
    A0 = as(M,Status,Type,_,Co,Calls,Success,Comp,Dic,Loc,Comm,Fromwhere),
    A = as(M,Status,Type,Head,Co,Calls,Success,Comp,Dic,Loc,Comm,Fromwhere).

:- export(assertion_set_calls/3).
assertion_set_calls(X0, Calls, X) :-
    X0 = as(M,Status,Type,Head,Co,_,Success,Comp,Dic,Loc,Comm,Fromwhere),
    X = as(M,Status,Type,Head,Co,Calls,Success,Comp,Dic,Loc,Comm,Fromwhere).

:- export(assertion_set_success/3).
assertion_set_success(X0, Success, X) :-
    X0 = as(M,Status,Type,Head,Co,Calls,_,Comp,Dic,Loc,Comm,Fromwhere),
    X = as(M,Status,Type,Head,Co,Calls,Success,Comp,Dic,Loc,Comm,Fromwhere).

:- export(assertion_set_comp/3).
assertion_set_comp(X0, Comp, X) :-
    X0 = as(M,Status,Type,Head,Co,Calls,Success,_,Dic,Loc,Comm,Fromwhere),
    X = as(M,Status,Type,Head,Co,Calls,Success,Comp,Dic,Loc,Comm,Fromwhere).

:- pred add_assertion(Assrt) # "Add assertion @var{Assrt} to internal DB.".
add_assertion(As) :-
    As = as(M,Status,Type,Head,Compat,Calls,Succ,Comp,Dic,AsLoc,Com,_),
    AsLoc = loc(S, LB, LE),
    assertion_body(Head, Compat, Calls, Succ, Comp, Com, Body),
    add_assertion_read(Head, M, Status, Type, Body, Dic, S, LB, LE),
    !. % TODO: why cut?
add_assertion(As) :-
    error_message("Internal Error: add_assertion: Could not add ~p", [As]).

% ---------------------------------------------------------------------------
%! # Directives

:- use_module(library(vndict), [null_dict/1]).

:- pred add_directive(C) : term(C)
# "The directive @var{C} is added to the program data base. This
   directive will be considered as read from the program, i.e.,
   analizers, transformations and output will use/show it.".
% E.g., add_directive(use_module(...)), add_directive(redefining(append/3)).
add_directive(Body) :-
    ( current_fact(source_clause(_Key, directive(Body), _Dict)) -> % TODO: sure?
        true
    ; null_dict(Dict),
      Key = '\6\newdirective', % TODO: key is not unique?!
      assertz_fact(source_clause(Key, directive(Body), Dict))
    ).

:- pred erase_directive(D) : term(D)
    # "Erase directive @var{D} (previously added with @pred{add_directive/1}".
erase_directive(Body) :-
    Key = '\6\newdirective',
    retractall_fact(source_clause(Key, directive(Body), _)).

% TODO: this is using clause_read, which is removed when code is normalized!
:- pred type_of_directive(Type,Body) 
    # "There is a directive of the form @var{:- Type Body}
       (of arity one).".
type_of_directive(Type,Body):-
    functor(D,Type,1),
    arg(1,D,Body),
    clause_read(_M,0,D,_VarNames,_Source,_LB,_LE).

% ---------------------------------------------------------------------------
%! # Predicate index

% TODO: merge with itf_db?

:- use_module(library(compiler/p_unit/unexpand), [add_head_unexpanded_data/1]).

:- data pr_key/1.

:- pred pr_key_clean + det # "Removes all information about predicate order.".
pr_key_clean :-
    retractall_fact(pr_key(_)).

:- pred pr_key_get(K) => cgoal(K) + multi # "Current predicate keys".
% it was predkey
pr_key_get(K) :- pr_key(K).

:- pred pr_key_add(K) => cgoal(K) + det # "Add a predicate key (once)".
pr_key_add(K) :-
    ( pr_key(K) -> true
    ; assertz_fact(pr_key(K))
    ).

generate_pr_key(Cl) :-
    Cl = clause(H,_),
    !,
    functor(H, F, A),
    functor(Key, F, A),
    pr_key_add(Key), % (only added once)
    ( add_head_unexpanded_data(H) -> true ; true ). % TODO: it was marked as a kludge, why? % TODO: may it fail?
generate_pr_key(_). % TODO: not for directives

% TODO: used only in some transformations, check again
:- pred add_defined_pred(ClKey, M) : (term(ClKey), atm(M)) + det
   # "Add the necessary data in itf_db (and @pred{pr_key/1}) to define the
   predicate @var{ClKey} in the module @var{M}.".
add_defined_pred(Key, M) :-
    \+ pr_key(Key),
    functor(Key, F, A),
    assert_itf(defined, M, F, A, _),
    !,
    assertz_fact(pr_key(Key)).
add_defined_pred(_, _).
 
:- pred new_predicate(+F,+A,-NewF) + det
    # "Checks wether there is a predicate @var{F}/@var{A} in the
       program and returns @var{NewF} so that there is no predicate
       @var{NewF}/@var{A} in the program.".
new_predicate(F,A,NewF):-
    curr_module(M), % TODO: choicepoints?
    new_predicate_name(F,F,A,0,NewF),
    assert_itf(defined,M,NewF,A,_).
    
new_predicate_name(TmpF,F,A,N,NewF):-
    current_itf(visible,TmpF,A), !,
    N1 is N+1,
    name(N1,S1),
    "_"=[S],
    atom_codes(Suffix,[S|S1]),
    atom_concat(F,Suffix,TmpF1),
    new_predicate_name(TmpF1,F,A,N1,NewF).
new_predicate_name(NewF,_F,_A,_N,NewF).

:- pred predicate_names(-list)
   # "Returns the specs of predicates defined in the current punit.".
predicate_names(Names):-
    findall(F/A, current_itf(defines,F,A), Names).

:- pred multifile_predicate_names(-list)
   # "Returns the specs of predicates defined in the current punit.".
% TODO: multifile predicate should be included in predicate_names but we lack
% tests to check if it would break something
multifile_predicate_names(NamesMulti):-
    setof(F/A, get_multifile(F,A), NamesMulti).

get_multifile(F,A) :-
    current_itf(multifile,Sg,_),
    functor(Sg,F,A).

:- data internal_pred_name/3.

:- doc(hide,internal_predicate_names/1).
internal_predicate_names(Names):-
    findall((F,A,NF), current_fact(internal_pred_name(F,A,NF)), Names).

:- doc(hide,new_internal_predicate/3).

% this checks clashes with current module predicates, but not between
% internal names (which are names created by CiaoPP, not module-expanded)
new_internal_predicate(F,A,NewF):-
    current_fact(internal_pred_name(F,A,NewF)), !.
new_internal_predicate(F,A,NewF):-
    ( curr_module(user(_)) -> % TODO: "(user(_), user)" is duplicated elsewhere
        M = user
    ; curr_module(M)
    ),
    module_concat(M,F,MF),
    new_predicate_name(MF,MF,A,0,NewF0),
    ( MF==NewF0 -> 
      NewF=F
    ; NewF=NewF0,
      asserta_fact(internal_pred_name(F,A,NewF)) % TODO: why not assertz_fact? (JF)
    ).

% ---------------------------------------------------------------------------
%! # Native props

:- use_module(library(compiler/p_unit/unexpand), [unexpand_meta_calls/2]).
:- use_module(library(streams)).

:- redefining(native/2). % also in basic_props % TODO: rename?
:- data native/2.
:- data regtype/2.

% Fill p_unit:native/2 and regtype/2 from assertions
init_native_props:-
    % (failure-driven loop)
%Nop!   current_itf(visible,Goal,_),
    % only prop assertions
    % TODO: (IC) is ":- prop p(X). :- comp p(X) + native." recognized as a native prop?
    %   possible fix: check that it is prop in one assertion_read/9 query ask for native in another
    assertion_read(Goal,_M,_,prop,Body,_,_,_,_),
    assertion_body(Goal,_Compat,_Call,_Succ,Comp,_Comm,Body),
    % should assert most general goals?
    % can be native several times
    ( builtin(native(Goal,Prop),Native),
      member(Native,Comp),
%         displayq( native_props( Goal , Prop ) ), nl,
      asserta_fact(p_unit:native(Goal,Prop))
    ; true
    ),
    % can be regtype only once
    ( builtin(regtype(Prop0),Regtype),
      member(Regtype,Comp) ->
        unexpand_meta_calls(Prop0,Type), % TODO: why? (JF)
        % displayq(regtype(Goal,Type)), nl,
        asserta_fact(regtype(Goal,Type))
    ; true
    ),
    fail.
%% init_native_props:-
%%         current_fact(regtype(Head,Prop)),
%%         display(regtype(Head,Prop)), nl,
%%      fail.
init_native_props.

% enum_regtype(-Goal, -NProp)
enum_regtype(Goal, NProp) :-
    current_fact(regtype(Goal,NProp0)),
    % TODO: equivalent to prop_to_native/2 in this case
    ( prop_to_native_(Goal,NProp) -> true
    ; NProp=NProp0
    ).

% TODO: original success is commented (wrong)
:- pred prop_to_native(+Prop,-NProp) % => cgoal * native_prop_term
   # "Obtain the native property (lit) @var{NProp} that corresponds to
   the (lit) @var{Prop} user predicate.".

prop_to_native(Prop,_NProp):-
    var(Prop), !, throw(error(instantiation_error(Prop), prop_to_native/2)).
prop_to_native(Prop,NProp2):-
    current_fact(regtype(Prop,NProp0)), !,
    NProp2=regtype(NProp),
    ( prop_to_native_(Prop,NProp) -> true % TODO: why?
    ; NProp=NProp0
    ).
prop_to_native(Prop,NProp):-
    prop_to_native_(Prop,NProp).

% TODO: Creates choicepoints. Intended?
prop_to_native_(Prop,NProp):-
    current_fact(p_unit:native(Prop,NProp)).
prop_to_native_(Prop,NProp):-
    native_property(Prop,NProp). % builtin tables

% TODO: original success is commented (wrong)
:- pred native_to_prop(+NProp,-Prop) % => native_prop_term * cgoal
   # "Obtain the user predicate (lit) @var{Prop} that corresponds to
   the native property (lit) @var{NProp}.".

% TODO: why? simplify
native_to_prop(NProp2,Prop) :-
    ( NProp2 = regtype(NProp) -> RegType=yes ; NProp=NProp2, RegType=no ),
    %
    ( current_fact(p_unit:native(Prop0,NProp)) -> Prop=Prop0 % TODO: bad indexing
    ; native_property(Prop0,NProp) -> Prop=Prop0 % builtin tables % TODO: bad indexing
    ; RegType=yes, current_fact(regtype(Prop0,NProp)) -> Prop=Prop0 % TODO: bad indexing
    ; fail
    ).

%% ---------------------------------------------------------------------------

:- pred native_to_props_visible(Props,Goals) => list(cgoal) * list(cgoal)
    # "Maps native @var{Props} into their corresponding @var{Goals}
      visible in the current module.".
native_to_props_visible([],[]).
native_to_props_visible([I|Info],OutputUser):-
    native_to_props_visible_(I,OutputUser,OutputUser1),
    native_to_props_visible(Info,OutputUser1).

native_to_props_visible_(Prop,OutputUser,OutputUser1):-
    native_prop_map(Prop,P,Vars), !,
    each_to_prop(Vars,P,OutputUser,OutputUser1).
native_to_props_visible_(Prop,[O|OutputUser],OutputUser):-
    native_to_prop_visible(Prop,O).

each_to_prop([V|Vars],P,[O|OutputUser],OutputUser1):-
    functor(Prop,P,1),
    arg(1,Prop,V),
    native_to_prop_visible(Prop,O),
    each_to_prop(Vars,P,OutputUser,OutputUser1).
each_to_prop([],_P,OutputUser,OutputUser).

% TODO: document why?
native_to_prop_visible(NProp,Prop):-
    native_to_prop(NProp,Prop),
    current_itf(visible,Prop,_), !.
native_to_prop_visible(NProp,Prop):-
    native_property(Prop,NProp), !. % builtin tables % TODO: bad indexing
% should be:
%% native_to_prop_visible(NProp,Prop):-
%%      native_to_prop(NProp,Prop), !,
%%      make_prop_visible(Prop).
native_to_prop_visible(NProp,NProp).

% % TYPE_SYMBOLS_NOT_WHAT_WE_WANT
% native_to_prop_visible(NProp,Prop):-
%       functor(NProp,T,1),
% % not really: should check that it is indeed a type!!!
%       rule_type_symbol(T), !,
%       Prop=NProp.
% native_to_prop_visible(NProp,NProp):-
%       curr_module(M),
%       builtin_package(B),
%       ( clause_read(M,0,use_package(B),_,_Source,_LB,_LE)
%       -> true
%        ; assertz_fact( clause_read(M,0,use_package(B),no,0,0,0) )
%       ).

% make_prop_visible(Prop):-
%       functor(Prop,F,A),
%       extract_module(F,M),
%       module_spec(Spec,M), % if it was reversible!
%       functor(G,F,A),
%       assert_itf_kludge(p_unit,imports(F,Spec)).

% ---------------------------------------------------------------------------
%! # Inject packages for output (post-preprocessing unit)
% TODO: per module?

:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(engine(stream_basic), [absolute_file_name/7]).

:- pred inject_output_package(A) : atm(A)
   # "Inject the package @var{A} in the current program database (including the
   output package list). The necesary information from these packages is loaded
   for correct treatment and unexpansion.".

inject_output_package(A) :-
    ( curr_file(_, M) -> true
    ; error_message("inject_output_package/1 with no loaded module"),
      fail
    ),
    ( get_output_package(A) -> % already loaded
        true
    ; % warning_message("Adding package '~q', required for assertion-based output. Update the package list to remove this warning.",[A]),
      atom_concat('wrap_', A, AWrapper), % TODO: using custom modules (under ciaopp/lib/) include those packages (sometimes as reduced versions)
      absolute_file_name(library(AWrapper),'_opt','.pl','.',_,File,_),
      statistics(runtime,[T0,_]),
      load_package_info(M, File),
      statistics(runtime,[T1,_]),
      TotalT is T1 - T0,
      p_unit_log(['{adding missing package ',~~(A), ' in ',time(TotalT), ' msec.}']),
      add_output_package(A)
    ).

:- use_module(library(compiler/p_unit/unexpand), [generate_unexpanded_data/1, clean_unexpanded_data/0]).

load_package_info(M, File) :-
    set_ciaopp_expansion(true), % TODO: try to avoid this
    ( preprocessing_unit_internal([File], _, _, [inject_pkg_into(M)]) -> true ; true ), % TODO: can it fail?
    set_ciaopp_expansion(false),
    % TODO: update unexpanded data or wait until we've loaded everything?
    %   cleaning and recomputing all unexpanded data all the time makes sense (JF)
    clean_unexpanded_data,
    generate_unexpanded_data(M).

% ---------------------------------------------------------------------------
%! # Packages that must be included in the output

% TODO: per module?
% TODO: be careful! this cannot work for all packages (JF)

% TODO: delay "load_package_info/1"?

:- data pl_output_package/1.

:- pred get_output_package(X) # "@var{X} is a package that will be
   included in the output (@tt{module} directive).".
get_output_package(X) :-
    pl_output_package(X).

:- pred add_output_package(A) : atm(A)
   # "Add the package @var{A} to the output packages list".

add_output_package(A) :-
    ( pl_output_package(A) ->
        true
    ; assertz_fact(pl_output_package(A))
    ).

% ---------------------------------------------------------------------------
%! # Operators that must be included in the output

% TODO: per module?

:- data pl_output_op/3.

:- pred add_output_operator(Prec, Type, OP) : (int(Prec),atm(Type),atm_or_atm_list(OP))
   # "Define an operator for output (same arguments as in @pred{op/3}).".

add_output_operator(A, B, C) :-
    asserta_fact(pl_output_op(A, B, C)). % TODO: why not assertz_fact? (JF)

% TODO: review old assertion:
%% :- pred get_output_operator(Pred, Type, OP) : (int(Pred),atm(Type),atm_or_atm_list(OP))
%%    # "Enumerate output operators (same arguments as in @pred{op/3}).".

:- pred get_output_operator(Pred, Type, OP) => int * atm * atm_or_atm_list
   # "Enumerate output operators (same arguments as in @pred{op/3}).".

get_output_operator(A,B,C) :-
    current_fact(pl_output_op(A,B,C)).

% ---------------------------------------------------------------------------
%! # Comment DB

% TODO: per module?
% Note: this is a simplified version for the current uses (see older
%   version in Attic/ for more potential features)

:- pred comment_db(Comment) : string(Comment) + no_rtcheck
   % IG: force no rtcheck because it is a data
   # "Text comments, added at the beginning of the module output".

:- data comment_db/1.

:- pred cleanup_comment_db # "Cleans up the comment db".
cleanup_comment_db :-
    retractall_fact(comment_db(_)).

:- pred add_comment(Comment) : string(Comment)
   # "Add comment to the comment db".
add_comment(Comment) :-
    assertz_fact(comment_db(Comment)).

:- pred get_comment(Comment) => string(Comment)
   # "Retrieves comments from the comment db".
get_comment(Comment) :-
    current_fact(comment_db(Comment)).

% ---------------------------------------------------------------------------
%! # Commented (%) assertions

:- data commented_assrt/1.

cleanup_commented_assrt :-
    retractall_fact(commented_assrt(_)).

% TODO: changing 'fromwhere' may be avoided
:- pred add_commented_assertion(A) : term(A)
   # "Add assertion @var{A} to the commented assertions DB.".
add_commented_assertion(A) :-
    A = as(M,S,T,Head,Compat,Call,Succ,Comp,Dic,Loc,Comm,_), !,
    A1 = as(M,S,T,Head,Compat,Call,Succ,Comp,Dic,Loc,Comm,commented),
    assertz_fact(commented_assrt(A1)).
add_commented_assertion(A) :-
    error_message("INTERNAL ERROR: add_commented_assertion: "||
        "~q is not a valid assertion", [A]).

get_commented_assertion(ClKey, As) :-
    As = as${head => ClKey},
    current_fact(commented_assrt(As)).

% ---------------------------------------------------------------------------
%! # Cached libraries

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(persdb/datadir), [ensure_datadir/2]).
:- use_module(library(compiler/p_unit/p_unit_db), [fake_module_name/1, cleanup_lib_p_unit_db/0]).

% TODO: Currently not used, but necessary if we want to rebuild/reload libcache
:- export(cleanup_libcache/0).
:- pred cleanup_libcache # "Cleans up libcache.".
cleanup_libcache :-
    cleanup_lib_p_unit_db,
    ( hook_cleanup_lib_regtypes -> true ; true ).

:- export(loaded_libcache/0).
:- pred loaded_libcache/0 # "Checks if the libcache is loaded".
loaded_libcache :-
    loaded_lib_p_unit_db.

:- export(load_libcache/1).
:- pred load_libcache(DataDir) # "Load the preprocessed
   library cache (specified in the @tt{core/Manifest/core.libcache.pl}
   module.".

load_libcache(DataDir) :-
    ensure_datadir(DataDir, Dir),
    catch(load_libcache_internal(Dir), error(_,_), throw(error(unable_to_load, load_libcache/1))).

load_libcache_internal(Path) :-
    load_from_file(Path, 'lib_p_unit_db.pl', restore_lib_p_unit_db),
    load_from_file(Path, 'lib_typedb.pl', restore_lib_regtypes).

restore_lib_regtypes(Stream) :- hook_restore_lib_regtypes(Stream), !.
restore_lib_regtypes(_).

% TODO: extend to arbitrary bundles (not only core)
:- export(gen_libcache/1).
:- pred gen_libcache(DataDir) # "Generate the preprocessed library
   cache (specified in the @tt{core/Manifest/core.libcache.pl} module.
   @alert{It cleans the current state of @lib{p_unit}}.".

gen_libcache(DataDir) :-
    cleanup_punit,
    bundle_path(core, 'Manifest/core.libcache.pl', P),
    %
    preprocessing_unit_list([P],_Ms,E,[load_irrelevant]),
    ( E == yes -> throw(error(failed_preprocesssing, gen_libcache/0)) ; true ),
    %assertz_fact(curr_module('core.libcache')),
    %assertz_fact(curr_file(P, 'core.libcache')),
    set_fact(fake_module_name('core.libcache')), % do not cache info of that module
    ensure_datadir(DataDir, Dir),
    gen_libcache_internal(Dir).

:- use_module(engine(runtime_control), [push_prolog_flag/2, pop_prolog_flag/1]). % TODO: do in a better way

gen_libcache_internal(Path) :-
    push_prolog_flag(write_strings, on),
    write_to_file(Path, 'lib_p_unit_db.pl', save_lib_p_unit_db),
    write_to_file(Path, 'lib_typedb.pl', save_lib_regtypes),
    pop_prolog_flag(write_strings).

save_lib_regtypes(Stream) :- hook_save_lib_regtypes(Stream), !.
save_lib_regtypes(_).

:- meta_predicate load_from_file(?, ?, pred(1)).
load_from_file(Path, Name, Pred) :-
    path_concat(Path, Name, F),
    open(F, read, InS),
    Pred(InS),
    close(InS).

:- meta_predicate write_to_file(?, ?, pred(1)).
write_to_file(Path, Name, Pred) :-
    path_concat(Path, Name, F),
    open(F, write, OutS),
    display(OutS, '%% Do not modify this file: it is generated automatically.'),
    nl(OutS),
    Pred(OutS),
    close(OutS).

