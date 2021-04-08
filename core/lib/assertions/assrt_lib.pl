:- module(assrt_lib, [
    %get_code_and_norm_assertions/2,
    get_code_and_related_assertions/5,
    get_code_and_related_assertions_opts/6,
    cleanup_code_and_related_assertions/0, 
    check_code_and_assrt_syntax/1,
    use_pkg/2,
    clause_read/7,
    assertion_read/9,
    assertion_body/7,
    %
    generate_asr_file/2,
    read_asr_file/2,
    rel_data/1,
    %
    print_assertions/1,
    print_unformatted_assertions/1,
    %
    comps_to_goal/3,
    comps_to_goal/4
], [assertions, nortchecks, basicmodes, regtypes, dcg, hiord, datafacts]).

:- doc(title,"Assertion processing library").
:- doc(author,"Manuel Hermenegildo").

:- doc(module,"

This module defines some predicates which are useful for writing
programs which process assertions (as defined in the @lib{assertions}
library). The exported predicates allow reading assertions in all
acceptable syntactic forms and converting them to a normalized format.

If you want to have access to some of the declarations read by the
predicates exported by this file it is also necessary to include the
library @lib{compiler/c_itf}.

").

:- doc(bug, "Should go into higher order properties and check the
   arguments for import/export also (and should probably look at the
   meta-predicate declarations for these)?").

:- doc(bug, "Abridged syntax is incompatible with option -modes.
   E.g.:
   ERROR (assrt_lib): (lns 87-89) arity mismatch in declaration for 
                  compare(?atm,@@term,@@term) in term_compare
   with:
@begin{verbatim}
      :- trust pred compare(?atm,@@term,@@term)
                => member([(=),(>),(<)]) * term * term.
@end{verbatim}
   ").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(read)).

% Other libraries
:- use_module(engine(runtime_control), [set_prolog_flag/2, prolog_flag/3,
      push_prolog_flag/2, pop_prolog_flag/1]).
:- use_module(engine(stream_basic)).
:- use_module(library(terms_io), [term_write/1]).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(internals), [asr_filename/2]).
:- use_module(library(fastrw)).
:- use_module(library(assertions/assrt_write), [write_assertion/6]).
:- use_module(library(compiler/c_itf), [
    cleanup_c_itf_data/0,
    old_file_extension/2,
    process_files_from/7, false/1,
    clause_of/7,
    comp_defines/1, activate_translation/3,
    base_name/2, defines_module/2, file_data/3,
    defines/3, defines_pred/3, exports/5, (multifile)/3,
    c_itf_internal_pred_decl/1,
    imports/5, 
    package/2]).
:- use_module(library(compiler/file_buffer)).
:- use_module(library(ctrlcclean), [ctrlc_clean/1]).
:- use_module(library(errhandle), [error_protect/2]).  
% :- use_module(library(miscprops)).
%% :- use_module(library(formulae),[list_to_conj/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(system), [fmode/2,chmod/2,delete_file/1]).
:- use_module(library(compiler/translation), 
    [expand_clause/6,del_goal_trans/1,del_clause_trans/1]).

%% ---------------------------------------------------------------------------
:- pred asr_version/1 :: int # "Contains a version number which identifies
   the @tt{.asr} files associated with this version of the assertion
   library. Should be changed every time changes are made which render
   the @tt{.asr} files incompatible, since this forces recomputation
   of all such files.".

asr_version('1.3.2').

%% ---------------------------------------------------------------------------
:- pred check_code_and_assrt_syntax(in(I)) :: filename
 
# "This predicate is useful for checking the syntax of the code and
   assertions in a file, as well as imports and exports.  Full
   (semantic) assertion checking must be done with the preprocessor.".
%% ---------------------------------------------------------------------------

check_code_and_assrt_syntax(I):-
    get_code_and_related_assertions(I,_M,_Base,_Suffix,_Dir),
    %% So that it can be used over and over again interactively
    cleanup_code_and_related_assertions.

%% ---------------------------------------------------------------------------
:- pred cleanup_code_and_related_assertions/0
 
# "Cleans up data asserted by assertion/code reader/normalizer.".
%% ---------------------------------------------------------------------------

cleanup_code_and_related_assertions :-
    cleanup_c_itf_data,
    retractall_fact(use_pkg(_,_)),
    retractall_fact(clause_read(_,_,_,_,_,_,_)),
    retractall_fact(assertion_read(_,_,_,_,_,_,_,_,_)).

%% ---------------------------------------------------------------------------
:- pred get_code_and_related_assertions(
    in(I),     go(M),    go(Base), go(Suffix), go(Dir) )
   ::   filename * moddesc * atm     * atm       * atm


# "This is the main entry point to the @concept{assertion
   reader/normalizer}. Reads all declarations and code in @var{I} and
   leaves it asserted in the database, in the format defined in
   @lib{'compiler/c_itf'}. Clauses are stored in @pred{clause_read/7}.
   Used packages are stored in @pred{use_pkg/2}.

   Also, it reads and normalizes @em{all} assertions in this file and
   all related files, leaving them asserted in @pred{assertion_read/9}
   facts. If up to date @tt{.asr} files exist for this or any of the
   related files, the assertion information is read directly from such
   @tt{.asr} files. @cindex{.asr files} Otherwise, the @tt{.pl} file
   is read and an up to date @tt{.asr} file is generated containing
   all assertions in the @tt{.pl} file, normalized and stored as
   @pred{assertion_read/9} facts.

   @var{M} is the name of the module defined by the file. @var{Base} is
   the absolute name of the file @var{I} but with no
   suffix. @var{Suffix} is the file suffix (e.g.,
   '@tt{.pl}'). @var{Dir} is the directory part of the filename (with
   no @tt{/} at the end).

   Since this predicate is intended for gathering file information for
   purposes which can be other than compilation to executable code
   (e.g., generating documentation or in the preprocessor) this
   predicate catches errors and proceeds in cases where file
   processing (e.g., during actual compilation) might normally abort.".

:- pred get_code_and_related_assertions_opts(
    in(I),     in(Opts),    go(M),    go(Base), go(Suffix), go(Dir) )
   ::   filename * list(atm)  * moddesc * atm     * atm       * atm

# "Version which accepts some options in @var{Opts}. In particular,
   @tt{'-v'} produces verbose output for debugging. Also passes
   on the options in @var{Opts} to pass two of the assertion
   normalizer. ".

%% ---------------------------------------------------------------------------

get_code_and_related_assertions(I,M,Base,Suffix,Dir):-
    get_code_and_related_assertions_opts(I,[],M,Base,Suffix,Dir).

get_code_and_related_assertions_opts(I,Opts,M,Base,Suffix,Dir):-
    (  ( prolog_flag(verbose_compilation,on,on) ; member('-v',Opts) )
    -> Verb = verbose
    ;  Verb = quiet ),
%       push_prolog_flag(runtime_checks, no),
    push_prolog_flag(read_assertions, no), % Bug: needs better integration
    push_prolog_flag(keep_assertions, yes), % Do not clean assertions!
    error_protect(ctrlc_clean(
            process_files_from(I, asr, any, 
                               process_file_assertions(I,Verb,Opts), 
                               c_itf:false, c_itf:false, needs_processing(I,Verb))
        ), fail), % TODO: fail or abort? use once_port_reify?
    pop_prolog_flag(keep_assertions), % Do not clean assertions!
    pop_prolog_flag(read_assertions),
%       pop_prolog_flag(runtime_checks),
    get_file_data(I,Base,M,Suffix,Dir).

%get_code_and_norm_assertions(Base,M):-
%       process_file_assertions_(Base,[]),
%       defines_module(Base,M).

get_file_data(I,Base,M,Suffix,Dir):-
    substract_pl(I,_Main,Suffix),
    base_name(I,Base),
    defines_module(Base,M),
    file_data(Base, _PlName, Dir).

substract_pl(FPL,F,'.pl') :-
    atom_concat(F,'.pl',FPL),
    !. %% it ends in .pl
%% else, it does not end in .pl
substract_pl(F,F,'').


%% ---------------------------------------------------------------------------

:- pred clause_read(Base, Head, Body, VarNames, Source, Line0, Line1)
   => (atm(Base), cgoal(Head), filename(Source), int(Line0), int(Line1))
   # "After calling @pred{get_assertions_and_code/5} this predicate
      contains the clauses in the file. The format is the same as that
      of @pred{clause_of/7} in @lib{c_itf}".

:- data clause_read/7.

%% ---------------------------------------------------------------------------

:- pred use_pkg(Base, Pkg)
   => (atm(Base), filename(Pkg))
   # "After calling @pred{get_assertions_and_code/5} this predicate
      contains the packages used in the file. The format is the same
      as that of @pred{package/2} in @lib{c_itf}".

:- data use_pkg/2.

%% ---------------------------------------------------------------------------

:- pred needs_processing/3 # "Controls if a given file needs to be
   fully processed by the assertion reader or it suffices to read its
   @tt{.asr} file. This is controlled by the modification dates of the
   @tt{.pl} and @tt{.asr} files. It also depends on whether it is the
   main file (for which the code is always read) or a file used by
   it.".

%% Main file will be processed (read) even if it hasn't changed.
needs_processing(I,_Verb,Base) :- 
    base_name(I,Base),
    % format("*** needs_proc: Will process fully (later) ~w.asr~n",[Base]),
    !.
%% Aux file, valid .asr file, valid version: read .asr data
needs_processing(_I,Verb,Base) :- 
    \+ old_file_extension(Base, '.asr'),
    read_asr_file_(Base,Verb), % Fails if wrong version!
    !,
    fail.
%% Aux file, invalid .asr file or invalid version: generate .asr data (later).
needs_processing(_I,_Verb,_Base) :- 
    % format("*** needs_proc: Should generate (later) ~w.asr~n",[Base]),
    true.

%% ---------------------------------------------------------------------------

:- pred read_asr_file/2 # "Reads the data in the .asr file. Fails if
   version is different from current version (so that .asr will be
   regenerated).".

read_asr_file(Base,V) :-
    ( ( prolog_flag(verbose_compilation,on,on) ; V=='-v' )
    -> Verb = verbose
    ;  Verb = quiet ),
    retractall_fact(rel_data(_)),
    read_asr_file_(Base,Verb).

read_asr_file_(Base,Verb) :-
    asr_filename(Base, AsrName),
    open(AsrName, read, Stream),
    current_input(CI),
    set_input(Stream),
    ( asr_version(V),
      read(v(V)), 
      !
    ; verb_message(Verb,['{Old version in ',AsrName,'}']),
      set_input(CI),
      close(Stream),
      fail
    ),
%%        working_directory(OldDir, Dir),
    verb_message(Verb,['{Reading ',AsrName]),
    read_asr_data_loop(Verb),
    set_input(CI),
    close(Stream),
    verb_message(Verb,'}').
%%        working_directory(_, OldDir).


%% read/1 version
%% read_asr_data_loop :-
%%      repeat,
%%      fast_read(X),
%%      ( X == end_of_file
%%      ; assertz_fact(X), % asserts clauses of assertion_read/9
%%        % format("*** Asserted: ~w~n",[X]),
%%        fail ).

%% fast_read/1 version (just fails at end of file)
read_asr_data_loop(Verb) :-
    (  fast_read(X)
    -> assertz_fact(X), % asserts clauses of assertion_read/9
                        % ...and now also rel_data/1 (PBC)
       % verb_message(Verb,[ 'Asserted: ',X]),
       read_asr_data_loop(Verb)
    ;  true ).

%% ---------------------------------------------------------------------------

:- pred process_file_assertions/4 # "Processes the assertions in a
   file, generating the corresponding @tt{.asr} file. In the case of the
   main file, also reads in the code.".

process_file_assertions(I,_Verb,Opts,Base) :- 
    % Main file
    base_name(I,Base),
    !,
    process_file_assertions_(Base,Opts).
process_file_assertions(_I,Verb,_Opts,Base) :-
    % Other files
    defines_module(Base,M),
    normalize_assertions(M,Base,[]),
    generate_asr_file(Base,Verb,related).

process_file_assertions_(Base,Opts):-
    c_itf:comp_defines(Base), %% force generation of defines/5 data
    defines_module(Base,M),
    normalize_assertions(M,Base,Opts),
    %% We do not generate the asr file for main (it could be only 
    %% partly normalized due to -modes option
    %  generate_asr_file(Base,Verb,main), %% MH2
    %% c_itf erases the clauses, so we must save them here 
    %% (or do the processing inside c_itf...)
    %% Second translation -PBC
    activate_translation(Base,M,add_clause_trans),
    activate_translation(Base,M,add_goal_trans),
    expand_clause(0,0,M,_,_,_), % Translator initialization
    save_clause_of(Base,M),
    save_use_pkg(Base,M),
    %% deactivate translations
    del_goal_trans(M),
    del_clause_trans(M).

save_clause_of(Base,M):-
    clause_of(Base,Head,Body,VarNames,Source,Line0,Line1),
    ( number(Head) ->
        \+ c_itf_internal_pred_decl(Body),
        H=Head,
        B=Body
    ; % do the "second expansion"
%         messages_basic:message(user, ['{Original: ',(Head:-Body)]),
      expand_clause(Head,Body,M,VarNames,H,B)
%         ,messages_basic:message(user, ['{Expanded: ',(H:-B)])
    ),
    % one more patch!!
    ( var(VarNames) -> VarNames=[] ; true ),
    assertz_fact(clause_read(Base,H,B,VarNames,Source,Line0,Line1)),
    fail.
save_clause_of(_Base,_M).

save_use_pkg(Base,_M):-
    ( % ( failure-driven loop)
      package(Base,Pkg),
        assertz_fact(use_pkg(Base,Pkg)),
        fail
    ; true
    ).

:- push_prolog_flag(multi_arity_warnings,off).

generate_asr_file(Base,V) :-
    ( ( prolog_flag(verbose_compilation,on,on) ; V=='-v' )
    -> Verb = verbose
    ;  Verb = quiet ),
    generate_asr_file(Base,Verb,related).

:- pred generate_asr_file/3 # "Does the actual generation of the
   @tt{.asr} file. Only assertions related to exported predicates are
   saved. Assertions for non-exported predicates remain in the database
   only for the main file (@tt{main} in fourth argument).".

generate_asr_file(Base,Verb,Component) :-
    asr_filename(Base, AsrName),
    verb_message(Verb,['{Generating ',AsrName]),
    file_data(Base, PlName, _Dir),
    fmode(PlName, Mode),
    file_buffer_begin(AsrName, Buffer, Stream),
    current_output(CO),
    set_output(Stream),
    asr_version(V),
    term_write(v(V)),
    write_asr_data_of(Base,Component),
    write_rel_data,
    set_output(CO),
    ( file_buffer_commit(Buffer) -> % TODO: warning on failed commit
        chmod(AsrName, Mode)
    ; message(error0, ['{In ',PlName]),
      message(warning, ['cannot create ',AsrName]), % TODO: error?
      message(error0, '}')
      % read_asr_file_(Base,Verb) % TODO: why? disabled --JF
    ),
    verb_message(Verb,'}').

:- pop_prolog_flag(multi_arity_warnings).

%% Normalization has occurred by now
write_asr_data_of(Base,Component) :-
    defines_module(Base,M),
    collect_prop_defs(Base,M,Component), %% MH1
    prolog_flag(write_strings, Old, on),
    print_exported_assertions_as_facts(Base,M,Component),
    set_prolog_flag(write_strings, Old).

%% Extra data for interfacing modules:
:- data rel_data/1.

write_rel_data:-
    rel_data(X),
    fast_write(rel_data(X)),
    fail.
write_rel_data.

%% user files: all assertions exported
print_exported_assertions_as_facts(Base,user(F),_Component) :-
    !,
    ( assertion_read(PD,user(F),Status,Type,Body,Dict,S,LB,LE),
%%        writeq(assertion_read(PD,user(F),Status,Type,Body,Dict,S,LB,LE)),
%%        write(' .'), nl,
      fast_write(assertion_read(PD,user(F),Status,Type,Body,Dict,S,LB,LE)),
      fail
    ; true ),
    ( clause_of(Base,0,doc(P,CBody),VNs,S,L0,L1),
      ( P = Fu/A ; functor(P,Fu,A) ),
      defines_pred(Base,Fu,A),
      fast_write(
                 clause_read(Base,0,doc(P,CBody),VNs,S,L0,L1)),
      assertz_fact(
                 clause_read(Base,0,doc(P,CBody),VNs,S,L0,L1)),
      fail
    ; true ).
%% else, module: check for exports
print_exported_assertions_as_facts(Base,M,Component) :-
    ( assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE),
      functor(PD,F,A),
      ( exports(Base, F, A, _DefType, _Meta)
      ->
%%        writeq(assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE)),
%%        write(' .'), nl,
         fast_write(assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE))
       ; delete_assertion_if_not_main_file(Component,
                     assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE))
      ),
      fail
    ; true ),
    ( clause_of(Base,0,doc(P,CBody),VNs,S,L0,L1),
      ( P = Fu/A ; functor(P,Fu,A) ),
      exports(Base,Fu,A,_DefType,_Meta),
      fast_write(
                 clause_read(Base,0,doc(P,CBody),VNs,S,L0,L1)),
      assertz_fact(
                 clause_read(Base,0,doc(P,CBody),VNs,S,L0,L1)),
      fail
    ; true ),
    print_reexported_assertions_as_facts(Base,M).

delete_assertion_if_not_main_file(main,_Assertion).
delete_assertion_if_not_main_file(related,Assertion):-
    retract_fact(Assertion).


%% :- data pending_reexport/4.

%% special case: take care also of assertions reexported by a module!
print_reexported_assertions_as_facts(Base,M) :-
    ( exports(Base, F, A, _DefType, _Meta),
      reexported_from(Base,M,F,A,MI),
      functor(PD,F,A),
      (  assertion_read(PD,MI,Status,Type,Body,Dict,S,LB,LE)
      -> 
         %% writeq(assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE)),
         %% write(' .'), nl, 
         fast_write(assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE))
      ;  true
         %% MH2 Actually, this seems to be working now
         %% message(user, ['{In ',Base,'.pl']),
         %% message(warning,['pending reexport for ',F,'/',A,' from ',MI]),
         %% %% asserta_fact(pending_reexport(PD,Base,M,MI))
         %% message(user, '}')
      ),
      fail
    ; true ).

%% Only one solution needed
reexported_from(Base,M,F,A,MI) :- 
    \+ defines_pred(Base, F, A),
    imports(M, MI, F, A),
    !.

imports(M, MI, F, A):- imports(M, MI, F, A, _).

:- set_prolog_flag(multi_arity_warnings, off).

verb_message(verbose,Message) :-
    messages_basic:message(inform, Message).
verb_message(quiet,_Message).
% verb_message(verbose,Type,Message) :-
%       messages_basic:message(Type,Message).
% verb_message(quiet,_Type,_Message).

:- set_prolog_flag(multi_arity_warnings, on).

%% ---------------------------------------------------------------------------
%% Made behaviour depend on whether it is main file or component 
%% Avoids repetition for main file MH1
:- pred collect_prop_defs(Base,M,Component) : moddesc(M)
   # "This predicate collects clauses defining properties that are exported
      by module @var{M} and leaves them as @pred{clause_read/7} facts, and
      writing those facts to the @tt{.asr} file.".

collect_prop_defs(Base,M,Component):- 
    assertion_read(PD,M,_AStatus,prop,_ABody,_ADict,_S,_LB,_LE),
    % Check that the prop is exported
    functor(PD,F,A),
    exports(Base, F, A, _DefType, _Meta),
    clause_of(Base,PD,Body,Dict,S,LB,LE), 
    %% If main, all *clauses* have already been asserted!
    (  Component = main
    -> true 
    ;  assertz_fact(clause_read(Base,PD,Body,Dict,S,LB,LE)) ),
    fast_write(clause_read(Base,PD,Body,Dict,S,LB,LE)),
    fail.
collect_prop_defs(_Base,_M,_Component). 

% ===========================================================================
% Assertion normalization

%:- export(normalize_assertions/3).
:- export(normalize_assertion/9).
:- export(norm_goal_prop/3). 
:- export(denorm_goal_prop/3).
:- include(library(assertions/assrt_norm_common)).
% (see assrt_norm_common.pl for details on hooks)

% (hook)
ignore_norm_props(modedef). % modedefs already transformed -- leave as is 

% (hook)
pass_one_cleanup(M) :-
    % (just in case they where added and expanded in c_itf:read_assertion/6) % TODO: better integration
    retractall_fact(assertion_read(_,M,_,_,_,_,_,_,_)).

% (hook)
pass_two_not_required(modedef). %% modedefs already transformed in pass one -- leave as is
%pass_two_not_required(test).    %% tests do not require transformation

% (hook)
pass_two_check_body(test,_,_,_,_,_,_,_) :- !. %% Unit-Tests will be checked when compiled, not here % TODO: why? due to unit-test only imports? (JF)
pass_two_check_body(texec,_,_,_,_,_,_,_) :- !.
pass_two_check_body(_AType,M,AM,F/A,NPropAss,S,LB,LE) :-
    check_body_properties(M,AM,F,A,NPropAss,S,LB,LE).

% (hook)
get_source_assrt(Base,Assrt,Dict,S,LB,LE) :-
    clause_of(Base,1,Assrt,Dict,S,LB,LE).

% (hook)
add_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE) :-
    assertz_fact(assertion_read(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE)).

% (hook)
get_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE) :-
    current_fact(assertion_read(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE)).

% (hook)
del_assertion_of(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE) :-
    retract_fact(assertion_read(PD,M,AStatus,AType,NNAss,Dict,S,LB,LE)).

:- pred assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)
   => ( moddesc(M), assrt_status(Status), assrt_type(Type),
        assrt_body(Body), dictionary(Dict), int(LB), filename(Source),
        int(LE) )
   # "Each fact represents that an assertion for @var{Goal} has been read
   in module @var{M}, which has status @var{Status} and is of type
   @var{Type}.  @var{Body} is the actual body of the
   assertion. @var{Dict} contains the names of the variables which
   appear in the assertion. @var{Source} is the file in which the
   assertion appears (treats included files correctly). @var{LB} and
   @var{LE} are the first and last line numbers in this source file in
   which the assertion appears (if the source is not available or has
   not been read @var{LB}=@var{LE}=0).  @var{Goal} may be normalized
   or not, i.e., it may contain modes or properties, but it is always
   a term of the same functor and arity as the predicate it represents
   (i.e., it is not in Functor/Arity format). @var{Body} is always
   normalized (but the properties or property conjunctions inside may
   not -- see @pred{normalize_assertions_pass_one/1} and
   @pred{normalize_assertions_pass_two/1}.".

:- data assertion_read/9. 

% (hook)
:- success assertion_body(Pred,Compat,Call,Succ,Comp,Comm, Asst)
    : (ground(Pred), ground(Compat), ground(Call), ground(Succ), ground(Comp), ground(Comm))
    => ground(Asst).

:- success assertion_body(Pred,Compat,Call,Succ,Comp,Comm, Asst)
    : ground(Asst)
    => (ground(Pred), ground(Compat), ground(Call), ground(Succ), ground(Comp), ground(Comm)).

assertion_body(Pred,Compat,Call,Succ,Comp,Comm,
          (Pred::Compat:Call=>Succ+Comp#Comm)).

% (hook)
% TODO: different in ciaopp
% No disjunctions supported yet... 
norm_normal_props((Prop,Rest),[Prop|NRest]) :- !,
    norm_normal_props(Rest,NRest). 
norm_normal_props(FinalProp,[FinalProp]).

% ===========================================================================

%% ---------------------------------------------------------------------------
:- pred check_body_properties(
   in(CurrentMod,moddesc),in(AssrtMod,moddesc),
   in(F,atm),in(A,int),in(NAss,nabody),in(S),in(LB),in(LE))

    # "Checks each property in the body of assertion @var{NAss}
      (see @pred{check_property/4}). Checks only assertions in the
      current module @var{CurrentMod}.".
%% ---------------------------------------------------------------------------

check_body_properties(M,M,F,A,NAss,S,LB,LE):- !,
    assertion_body(_NPD,CNDP,CNCP,CNAP,CNGP,_CO,NAss),
    check_properties(CNDP,F,A,M,S,LB,LE),
    check_properties(CNCP,F,A,M,S,LB,LE),
    check_properties(CNAP,F,A,M,S,LB,LE),
    check_properties(CNGP,F,A,M,S,LB,LE).
check_body_properties(M,AM,_Functor,_Arity,_NAss,_S,_LB,_LE):- 
    M \== AM.

%% ---------------------------------------------------------------------------
:- pred check_properties(Props,in(F,atm),in(A,int),in(M,moddesc),
                     in(S,atm),in(LB,int),in(LE,int))

    : list(Props)

    # "Checks each property in @var{Props} (see @pred{check_property/4}).".
%% ---------------------------------------------------------------------------

check_properties([],_F,_A,_M,_S,_LB,_LE).
check_properties([Prop|Props],F,A,M,S,LB,LE) :-
    check_property(Prop,F,A,M,S,LB,LE),
    check_properties(Props,F,A,M,S,LB,LE).

%% ---------------------------------------------------------------------------
:- pred check_property(+Prop,in(F,atm),in(A,int),in(M,moddesc),
                   in(S,atm),in(LB,int),in(LE,int)) 

# "Checks that, for a property @var{Prop} (which appears in a
   declaration for @var{F}/@var{A} in module @var{M}), a definition
   for that property is visible to that module.".
%% ---------------------------------------------------------------------------

%% 0.8 version: the decls are not sometimes available yet...
%% but line numbers available!
check_property(Prop,F,A,M,S,LB,LE) :- 
       %% simple_message("** Checking ~w",[Prop]),
    functor(Prop,PF,PA),
    (  ( imports(M,_PM,PF,PA)
       ; defines(M,PF,PA) 
       ; multifile(M, PF, PA))
    -> true
    ;  warning_message(loc(S,LB,LE),
              "~w used in assrt for ~w in ~w not defined or imported",
              [PF/PA,F/A,M])
    ),
    !.

%% 0.7 version
%% check_property(Prop,F,A,M) :- 
%%      %% simple_message("** Checking ~w",[Prop]),
%%      functor(Prop,PF,PA),
%%      functor(NProp,PF,PA),
%%      (  assertion_read(NProp,PM,_AStatus,Type,_NPropAss,_Dict,_LC),
%%         propfunctor(Type)
%%            %% simple_message("** Found ~w as a ~w in module ~w",[NProp,Type,PM])
%%      -> true
%%      ;  warning_message("no decl for prop ~w used in assrt for ~w in ~w",
%%                [PF/PA,F/A,M] ) ),
%%      (  ( imports(M,PM,PF,PA)
%%         ; defines(M,PF,PA) )
%%      -> true
%%      ;  warning_message("~w used in assrt for ~w in ~w not defined or imported",
%%                [PF/PA,F/A,M] ) ),
%%      !.

comp_to_goal_assrt(Comp, Body0, Body) :-
    Comp  =.. [PropName, _|Args],
    Body0 =.. [PropName, Body|Args].

:- pred comps_to_goal/3 #
    "This predicate allows to compound a list of global properties in to
    sucessive meta-calls".

comps_to_goal(Comp) -->
    comps_to_goal(Comp, comp_to_goal_assrt).

:- pred comps_to_goal/3 # "This predicate allows to compound a list of
    global properties in to successive meta-calls, but in the
    third argument you can use your own selector.".

:- meta_predicate comps_to_goal(?, pred(3), ?, ?).
comps_to_goal([],             _) --> [].
comps_to_goal([Check|Checks], Goal) -->
    comps_to_goal2(Checks, Check, Goal).

:- meta_predicate comps_to_goal2(?, ?, pred(3), ?, ?).
comps_to_goal2([], Check, Goal) -->
    Goal(Check).
comps_to_goal2([Check|Checks], Check0, Goal) -->
    Goal(Check0),
    comps_to_goal2(Checks, Check, Goal).

:- test comps_to_goal(Comp, Goal, Pred) :
    (
        Comp = [not_fails(p(A)), is_det(p(A)), exception(p(A), exc)],
        Pred = p(A)
    ) =>
    (
        Goal = not_fails(is_det(exception(p(A),exc)))
    ).

% ---------------------------------------------------------------------------

:- use_module(library(lists), [append/3]).

:- export(prop_apply/3).
:- export(prop_unapply/3).
:- export(prop_argvar/2).

% prop_apply(+P, +X, -PX): From P(A1,...,An) and X to P(A1,...,An,X)
prop_apply(P, X, PX) :-
    P =.. [F|Args],
    append(Args,[X],ArgsX),
    PX =.. [F|ArgsX].

% prop_unapply(+PX, -P, -X): From P(A1,...,An,X) to P(A1,...,An) and X
prop_unapply(PX, P, X) :-
    PX =.. [F|ArgsX],
    append(Args,[X],ArgsX),
    !, % TODO: cut needed? (make sure that there are no chpts)
    P =.. [F|Args].

% prop_argvar(+PX,-X): From P(A1,...,An,X) obtain X
prop_argvar(PX, X) :-
    functor(PX,_,N),
    arg(N,PX,X).

%% ---------------------------------------------------------------------------
:- pred print_assertions(M) :: moddesc # "Prints the assertions stored
   in the database as @pred{assertion_read/9} facts, performing some
   pretty-printing and simplification (e.g., eliminating empty
   fields). If @var{M} is instantiated, only information on module
   @var{M} is printed. Otherwise information for all modules is
   printed.".
%% ---------------------------------------------------------------------------

print_assertions(M) :-
    messages_basic:message(inform, '{Printing assertions read '),
    assertion_read(PD,M,Status,Type,Body,Dict,_S,_LB,_LE),
    %% Using now version in library(assrt_write)
    write_assertion(PD,Status,Type,Body,Dict,status),
    fail.
print_assertions(_M) :-
    messages_basic:message(inform, '}').

%% ---------------------------------------------------------------------------
:- pred print_unformatted_assertions(M) :: moddesc # "Prints the
   assertions stored in the database as @pred{assertion_read/9} facts,
   in a raw format (no attempt is made to simplify the assertions). If
   @var{M} is instantiated, only information on module @var{M} is
   printed. Otherwise information for all modules is printed.".
%% ---------------------------------------------------------------------------

print_unformatted_assertions(M) :-
    messages_basic:message(inform, '{Printing assertions read'),
    assertion_read(PD,M,Status,Type,Body,Dict,_S,_LB,_LE),
    %% Using now version in library(assrt_write)
    local_write_assertion(PD,Status,Type,Body,Dict,status,M),
    fail.
print_unformatted_assertions(_M) :-
    messages_basic:message(inform, '}').

local_write_assertion(PD,Status,Type,Body,_Dict,_Flag,M) :-
    assertion_body(PD,DP,CP,AP,GP,CO,Body),
    messages_basic:message(inform, ['(in module ',M,':)']),
    messages_basic:message(inform, [':- ',Status,' ',Type,' ',PD,
                   ' :: ',DP,' : ',CP,' => ',AP,' + ',GP,' # ',CO]).

