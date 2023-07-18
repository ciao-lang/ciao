:- module(_, [
    % ------
    % (itf_db)
    assert_itf/5, % assert_itf_kludge/2,
    current_itf/3,
    retract_itf/5,
    get_module_from_sg/2,
    mod_in_libcache/2,
    loaded_lib_p_unit_db/0,
    % ------
    % (assrt_db)
    assertion_read/9,
    add_assertion_read/9,
    remove_assertion_read/9,
    removeall_assertion_read/9,
    ref_assertion_read/10,
    assertion_of/9,
    % ------
    % (clause_db)
    clause_read/7, 
    prop_clause_read/7,
    add_prop_clause_read/7,
    clause_locator/2, 
    maybe_clause_locator/2, 
    add_clause_locator/2,
    literal_locator/2, 
    source_clause/3
], [assertions, basicmodes, datafacts, regtypes, nativeprops]).

:- doc(title, "p_unit database").

:- doc(module, "This module stores the database for storing code and
   assertions in @lib{p_unit}.

   The database is two-layered, where the @tt{lib_} facts stores data
   saved and preserved for the @em{libcache}.").

% TODO: document this module
:- doc(bug,"1. There are invalid clause-keys in calls to clause_locator.
    E.g., from entries or exports during analysis.").
:- doc(bug,"2. We should get rid of dummy clause locators.").

% itf db
:- use_module(library(compiler/p_unit/unexpand), [unexpand_meta_calls/2]).
% assrt db
:- use_module(library(assertions/assertions_props)).
:- use_module(library(assertions/c_itf_props)).
:- use_module(library(terms_check), [variant/2]).
:- use_module(library(messages), [warning_message/3, location_t/1]).
% clause db
:- use_module(library(compiler/p_unit/program_keys), [clkey/1, clid_of_atomid/2, orig_clause_id/2]).

% ===========================================================================
%! # Data

:- export(curr_module/1).
:- export(curr_file/2).
:- export(defines/3).
:- export(imports/4).
:- export(exports/2).
:- export(multifile/2).
:- export(meta/2).
:- export(dynamic/1).
:- export(impl_defines/2).
:- export(defines_module/2).
:- export(defines_module_rev_idx/2).
:- export(pgm_assertion_read/9).

:- data curr_module/1.
:- data curr_file/2.
:- data defines/3.
:- data imports/4.
:- data exports/2.
:- data multifile/2.
:- data meta/2.
:- data dynamic/1.
:- data impl_defines/2.
:- data defines_module/2.
:- data defines_module_rev_idx/2. % reverse index (IG)
:- data assertion_of/9. 
:- data pgm_assertion_read/9.
:- data clause_read/7.
:- data pgm_prop_clause_read/7.
:- data source_clause/3.
:- data locator/2.

% (libcache)
:- data lib_defines/3.
:- data lib_imports/4.
:- data lib_exports/2.
:- data lib_multifile/2.
:- data lib_meta/2.
:- data lib_dynamic/1.
:- data lib_impl_defines/2.
:- data lib_defines_module/2.
:- data lib_defines_module_rev_idx/2. % (reverse index)
:- data lib_assertion_read/9.
:- data lib_prop_clause_read/7.

% ---------------------------------------------------------------------------

:- export(fake_module_name/1). % TODO: for lib_* generation
:- data fake_module_name/1.

% ---------------------------------------------------------------------------
%! # Cleanup

:- export(cleanup_p_unit_db/0).
cleanup_p_unit_db :-
    cleanup_itf_db,
    cleanup_assrt_db,
    cleanup_clause_db.

cleanup_itf_db :-
    retractall_fact(defines(_,_,_)),
    retractall_fact(imports(_,_,_,_)),
    retractall_fact(exports(_,_)),
    retractall_fact(multifile(_,_)),
    retractall_fact(meta(_,_)),
    retractall_fact(dynamic(_)),
    retractall_fact(curr_module(_)),
    retractall_fact(curr_file(_,_)),
    retractall_fact(impl_defines(_,_)),
    % defines_module not cleaned here?, added by IG
    retractall_fact(defines_module(_,_)),
    retractall_fact(defines_module_rev_idx(_,_)).

:- export(cleanup_assrt_db/0). % TODO: needed for some assertion rewrites
:- doc(cleanup_assrt_db, "Cleanups the assrt database.").
cleanup_assrt_db :-
    retractall_fact(pgm_assertion_read(_,_,_,_,_,_,_,_,_)),
    retractall_fact(assertion_of(_,_,_,_,_,_,_,_,_)).

:- export(cleanup_clause_db/0). % TODO: needed for some clause rewrites
:- doc(cleanup_clause_db,"Cleans up the clause_db database.").
cleanup_clause_db :-
    retractall_fact(pgm_prop_clause_read(_,_,_,_,_,_,_)),
    retractall_fact(clause_read(_,_,_,_,_,_,_)),
    retractall_fact(source_clause(_,_,_)),
    retractall_fact(locator(_,_)).

:- export(cleanup_lib_p_unit_db/0).
:- pred cleanup_lib_p_unit_db # "Cleans up all facts of lib_* predicates.".
cleanup_lib_p_unit_db :-
    retractall_fact(lib_defines(_,_,_)),
    retractall_fact(lib_imports(_,_,_,_)),
    retractall_fact(lib_exports(_,_)),
    retractall_fact(lib_multifile(_,_)),
    retractall_fact(lib_meta(_,_)),
    retractall_fact(lib_dynamic(_)),
    retractall_fact(lib_defines_module(_,_)),
    retractall_fact(lib_defines_module_rev_idx(_,_)),
    retractall_fact(lib_impl_defines(_,_)),
    %
    retractall_fact(lib_assertion_read(_,_,_,_,_,_,_,_,_)),
    %
    retractall_fact(lib_prop_clause_read(_,_,_,_,_,_,_)).

% ===========================================================================
%! # Module interface (itf)

:- use_module(library(compiler/c_itf), [module_expansion/9]).

assert_itf(defined,M,F,A,_Type):- % already expanded
    assertz_fact(defines(F,A,M)).
assert_itf(defines,M,F,A,_Type):-
    functor(Goal0,F,A),
    goal_module_expansion(Goal0, M, Goal),
    functor(Goal,FG,A),
    assertz_fact(defines(FG,A,M)).
assert_itf(imports,M,F,A,r(IM,EM)) :- !, % reexported predicates reexported
    functor(Goal0,F,A),
    goal_module_expansion(Goal0, EM, Goal),
    % unexpand_meta_calls(Goal1,Goal), % TODO: remove? related with addmodule?
    % TODO: this depends on type_of_goal which, at some point calls current_itf(meta, Goal, Meta)
    assertz_if_needed(imports(Goal,M,EM)),
    assertz_if_needed(imports(Goal0,M,r(IM,EM))). % (unexpanded goal for unexpand.pl)
assert_itf(imports,M,F,A,EM):-
    functor(Goal0,F,A),
    goal_module_expansion(Goal0, EM, Goal),
    % unexpand_meta_calls(Goal1,Goal), % TODO: remove? related with addmodule?
    assertz_if_needed(imports(Goal,M,EM)).
assert_itf(indirect_imports,M,F,A,EM):-
    functor(Goal0,F,A),
    goal_module_expansion(Goal0, EM, Goal),
    % unexpand_meta_calls(Goal1,Goal), % TODO: remove? related with addmodule?
    assertz_if_needed(indirect_imports(Goal,M,EM)).
assert_itf(exports,M,F,A,_M):-
    functor(Goal0,F,A),
    goal_module_expansion(Goal0, M, Goal),
    assertz_if_needed(exports(Goal,M)).
assert_itf(new_exports,M,F,A,_M):-
    functor(Goal,F,A),
    assertz_if_needed(exports(Goal,M)).
assert_itf(multifile,M,F,A,_DynType):-
    functor(Goal0,F,A),
    goal_module_expansion(Goal0, M, Goal),
    assertz_fact(multifile(Goal,M)).
assert_itf(meta,M,F,A,Meta0):-
    functor(Goal0,F,A),
    goal_module_expansion(Goal0, M, Goal),
    functor(Goal,MF,_),
    Meta0 =.. [_|As],
    Meta =.. [MF|As],
    assertz_fact(meta(Goal,Meta)).
assert_itf(dynamic,M,F,A,_Deftype):-
    functor(Goal0,F,A),
    goal_module_expansion( Goal0 , M , Goal ),
    assertz_if_needed(dynamic(Goal)).
assert_itf(defines_module,M,_,_,Base):-
    ( current_fact(defines_module(Base,M)) ->
        true
    ;
        assertz_fact(defines_module(Base,M)),
        assertz_fact(defines_module_rev_idx(M, Base))
    ).
assert_itf(impl_defines,M,F,A,_DynType):-
    functor(Goal0,F,A),
    goal_module_expansion( Goal0 , M , Goal ),
    assertz_fact(impl_defines(Goal,M)).

assertz_if_needed(indirect_imports(Goal,M,EM)) :-
    ( current_fact(imports(Goal,M,EM,_)) -> true
    ; assertz_fact(imports(Goal,M,EM,indirect))
    ).
assertz_if_needed(imports(Goal,M,EM)) :-
    ( current_fact(imports(Goal,M,EM,Mode)) ->
        ( Mode = direct -> true
        ; retractall_fact(imports(Goal,M,EM,_)),
          assertz_fact(imports(Goal,M,EM,direct))
        )
    ; assertz_fact(imports(Goal,M,EM,direct))
    ).
assertz_if_needed(exports(Goal,M)) :-
    ( current_fact(exports(Goal,M)) -> true
    ; assertz_fact(exports(Goal,M))
    ).
assertz_if_needed(dynamic(Goal)) :-
    ( current_fact(dynamic(Goal)) -> true
    ; assertz_fact(dynamic(Goal))
    ).

% TODO: why? remove?
% assert_itf_kludge(remote,imports(Goal,IM)):-
%     ( current_fact(imports(Goal,IM)) -> true
%     ; assertz_fact(imports(Goal,IM))
%     ).

% {ERROR (p_asr): ERROR PROCESSING FACT exports(basiccontrol,\+,1,static,\+goal)
%   from ast file}

goal_module_expansion(if(A,B,C), _basiccontrol, if(A,B,C)) :- !.
goal_module_expansion(','(A,B), _basiccontrol, ','(A,B)) :- !.
goal_module_expansion(';'(A,B), _basiccontrol, ';'(A,B)) :- !.
goal_module_expansion(^(A,B), _basiccontrol, ^(A,B)) :- !.
goal_module_expansion('->'(A,B), _basiccontrol, '->'(A,B)) :- !.
goal_module_expansion(\+A, _basiccontrol, \+A) :- !.
goal_module_expansion(!, _basiccontrol, !) :- !.
goal_module_expansion(Goal, M, GoalExpanded) :-
    % TODO: location/3?
    c_itf:module_expansion(Goal, true, M, _, asr, _, _, GoalExpanded, _Body).

:- pred retract_itf(+Class,_M0,+F,+A,_M)
   # "This predicate allows removing itf information when it is no longer true.
      This can happen for example during program transformation.".
retract_itf(exports,M0,F,A,_M):-
    functor(Goal,F,A),
    retract_fact(exports(Goal,M0)).

current_itf(visible,Goal,X):-
    var(X),
    visible_goal(Goal).
current_itf(visible,F,A):-
    nonvar(A),
    visible_spec(F,A).
current_itf(defines,F,A):-
    current_fact(defines(F,A,_)).
current_itf(defines,F,A):-
    lib_defines(F,A,_).
current_itf(defines_pred,G,M):-
    current_fact(defines(F,A,M)),
    functor(G,F,A).
current_itf(defines_pred,G,M):-
    lib_defines(F,A,M),
    functor(G,F,A).
current_itf(imports(M,Mode),Goal,IM):- % TODO: fixed arity in current_itf/3 is weird (JFMC)
    current_fact(imports(Goal,M,IM,Mode)).
current_itf(imports(M,Mode),Goal,IM):-
    lib_imports(Goal,M,IM,Mode).
% TODO: use a different predicate for imports relation. Uses of this case of
% current_itf look strange... I.e., if an expanded literal appears in an
% expanded body, then it is obvious that the pred is imported.
current_itf(imports,Goal,IM):- % IG change name to imported? do not confuse with compiler
    current_fact(imports(Goal,_M,IM,_)).
current_itf(imports,Goal,IM):-
    lib_imports(Goal,_M,IM,_).
current_itf(exports,Goal,M):- % IG change name to exported? do not confuse with compiler
    current_fact(exports(Goal,M)). 
current_itf(exports,Goal,M):-
    lib_exports(Goal,M).
current_itf(exports,Goal,user(File)):-
    curr_file(_,user(File)),
    current_fact(defines(F,A,user(File))),
    functor(Goal,F,A).
current_itf(multifile,Goal,M):-
    current_fact(multifile(Goal,M)).
current_itf(multifile,Goal,M):-
    lib_multifile(Goal,M).
current_itf(meta,Goal,Meta):-
    current_fact(meta(Goal,Meta)).
current_itf(meta,Goal,Meta):-
    lib_meta(Goal,Meta).
current_itf(dynamic,Goal,_Deftype):-
    current_fact(dynamic(Goal)).
current_itf(dynamic,Goal,_Deftype):-
    lib_dynamic(Goal).
current_itf(defines_module,M,Base):-
    defines_module_(M, Base).
current_itf(impl_defines,Goal,M):-
    current_fact(impl_defines(Goal,M)).
current_itf(impl_defines,Goal,M):-
    lib_impl_defines(Goal,M).

defines_module_(M, Base) :-
    var(M),
    ( current_fact(defines_module(Base, M))
    ; lib_defines_module(Base,M)
    ).
defines_module_(M, Base) :-
    nonvar(M), var(Base),
    ( current_fact(defines_module_rev_idx(M, Base))
    ; lib_defines_module_rev_idx(M, Base)
    ).

% TODO: This is wrong, visibility depends on the module (except for multifiles); add M (JFMC)
visible_goal(Goal):-
    current_itf(imports,Goal,_).
visible_goal(Goal):-
    current_itf(defines,F,A),
    functor(Goal,F,A).
visible_goal(Goal):-
    current_fact(multifile(Goal,_)).

visible_spec(F,A):-
    current_itf(defines,F,A).
visible_spec(F,A):-
    current_itf(imports,Goal,_),
    functor(Goal,F,A).
visible_spec(F,A):-
    current_fact(multifile(Goal,_)),
    functor(Goal,F,A).

% ---------------------------------------------------------------------------

:- use_module(library(compiler/p_unit/aux_filenames), [just_module_name/2]).
:- use_module(engine(runtime_control), [module_split/3]).

:- pred get_module_from_sg(+Sg, ?Module) => term * atm + (not_fails, is_det)
   # "@var{Module} is the name of the module for the predicate to which call
   pattern @var{Sg} corresponds.".
% IG: another sort of module_split?
get_module_from_sg(Sg,Module) :-
    current_itf(imports,Sg,Module0), atom(Module0), !,
    % TODO: just_module_name/2 not needed? is Module a path?
    ( just_module_name(Module0,Module) -> true ; Module = Module0).
get_module_from_sg(Sg,Module) :-
    current_itf(defines_pred,Sg,Module0), !,
    % TODO: just_module_name/2 not needed? is Module a path?
    ( just_module_name(Module0,Module) -> true ; Module = Module0).
get_module_from_sg(Sg,Module) :-
    % TODO: why? (inefficient!)
    functor(Sg,MF,_),
    module_split(MF, M, _), !,
    M = Module.
get_module_from_sg(_,''). %% '\+/1' has no module in Sg. % TODO: ??
%%%% IG: \+ is removed with a syntactic transformation, this clause can be
%%%% removed

% ---------------------------------------------------------------------------

% TODO: good indexing?
:- pred mod_in_libcache(M,Base) # "Module @var{M} with basename
   @var{Base} is a module already preloaded into libcache.".

mod_in_libcache(M,Base):-
    lib_defines_module(Base,M).

:- pred loaded_lib_p_unit_db # "Checks if libcache is loaded".
loaded_lib_p_unit_db :-
    % (just check one predicate)
    \+ \+ lib_assertion_read(_,_,_,_,_,_,_,_,_).

% ===========================================================================
%! # Assertions

:- pred assertion_of(Goal,M,Status,Type,Body,Dict,Source,LB,LE)
:: ( moddesc(M), assrt_status(Status), assrt_type(Type),
     assrt_body(Body), dictionary(Dict), int(LB), filename(Source),
     int(LE) ) + no_rtcheck
# "Each fact represents an assertion for @var{Goal} 
   in module @var{M}, which has status @var{Status} and is of type
   @var{Type}.  @var{Body} is the actual body of the
   assertion. @var{Dict} contains the names of the variables which
   appear in the assertion. @var{Source} is the file in which the
   assertion appears (treats included files correctly). @var{LB} and
   @var{LE} are the first and last line numbers in this source file in
   which the assertion appears (if the source is not available or has
   not been read @var{LB}=@var{LE}=0).  @var{Goal} is always
   a term of the same functor and arity as the predicate it represents
   (i.e., it is not in Functor/Arity format). It may be normalized
   or not, i.e., it may contain modes or properties in its arguments, 
   depending on the
   normalizations options (see @pred{opts/1}). @var{Body} is always
   normalized, but the properties or property conjunctions inside may
   not -- see @pred{normalize_assertions_pass_one/1} and
   @pred{normalize_assertions_pass_two/1} in @lib{assrt_norm}.".

:- doc(assertion_read/9,"Same as @tt{assertion_of/9} but assertions
   are already normalized and fully expanded (including module names).").
assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
    pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE).
assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
    lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE).

:- pred add_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)
# "Adds an entry for an assertion located in a preprocessing unit
   module (but not in library modules).".
add_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
    ( allowed_head(Type,Goal) ->
        assertz_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE))
    ; warning_message(loc(Source, LB, LE),
                      "Assertion head syntax ~q, assertion ignored (try loading the 'modes' package)", [Goal])
    ).

allowed_head(Type,Goal) :-
    Type \= entry, !,
    functor(Goal,F,A),
    functor(Goal0,F,A),
    variant(Goal,Goal0).
allowed_head(_,_).

:- pred remove_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)
# "Removes an entry for an assertion located in a preprocessing unit
   module (but not from library modules).".
remove_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
    retract_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).
% remove_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
%       retract_fact(lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).

:- pred removeall_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)
# "Removes all entries matching arguments for assertions located in a
   preprocessing unit module (but not from library modules).".
removeall_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
    retractall_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).
% removeall_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
%       retractall_fact(lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).

:- pred ref_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE,Ref)
   # " Enumerates assertions and their reference @var{Ref}. Warning: this
   predicate is very dangerous and error prone. It must be used with care.".

:- doc(bug, "This predicate is very dangerous and error prone.").

ref_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE,Ref):-
    current_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE),Ref).
%%%%%%% !.
ref_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE,Ref):-
    current_fact(lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE),Ref).

% TODO: merge incanal_db.pl here
:- export(assertion_type/2).
assertion_type(assertion_read(_,_,_,Type,_,_,_,_,_),Type).
assertion_type(pgm_assertion_read(_,_,_,Type,_,_,_,_,_),Type).

% ===========================================================================
%! # Clauses (and other directives)

:- doc(clause_read(M, Head, Body, VarNames, Source, LB, LE),
   "Each fact is a clause of module @var{M}.
    The clause is @var{Head:-Body} (if a directive, @var{Head} is a number,
    see @pred{c_itf:clause_of/7}). @var{VarNames} contains the names of the 
    variables of the clause. @var{Source} is the file in which the
    clause appears (treats included files correctly). @var{LB} and
    @var{LE} are the first and last line numbers in this source file in
    which the clause appears (if the source is not available or has
    not been read @var{LB}=@var{LE}=0). @var{VarNames} is not a variable, 
    and @var{Head:-Body} is fully expanded, including module names.").

:- doc(prop_clause_read/7,"Same as @tt{clause_read/7} but for the
   properties not in the current module.").
prop_clause_read(M, Head, Body, VarNames, Source, LB, LE):-
    pgm_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE).
prop_clause_read(M, Head, Body, VarNames, Source, LB, LE):-
    lib_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE).

:- pred add_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE)
    # "Adds an entry for a property located in a user module (but not the
    current module).".
add_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE):-
    assertz_fact(pgm_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE)).

:- doc(source_clause(Key,Clause,Dict),"The current module has @var{Clause}
   identified by @var{Key} with variable names @var{Dict}.").

:- pred clause_locator(ClKey,L) :: atm * location_t
    # "The (current module) clause identified by @var{ClKey} is located in the
      source file around @var{L}.".
clause_locator(ClKey,L) :- locator(ClKey,L).

:- pred maybe_clause_locator(ClKey,L) : atm(ClKey) => location_t(L)
# "The (current module) clause with identifier @var{ClKey} either appears
   in the source file and has locator @var{L}, or was generated by
   ciaopp (e.g., during transform(vers)) from some other clause. In
   the latter case it is assigned the locator of that clause (as it
   has no own locator in the source). This functionality is useful on
   file output after source transformations, assertion checking,
   etc.".
maybe_clause_locator(ClKey,L) :-
    find_lines_in_orig_prog(ClKey,L).

%% ---------------------------------------------------------------------------
% TODO: COPIED FROM fixpo_ops.pl
%   move program_keys:orig_clause_id/2 to this module to have all locator
%   processting code together?

%   try to extract locator from original

find_lines_in_orig_prog(ClId,Loc):-
    clause_locator(ClId,Loc),!.
find_lines_in_orig_prog(ClId,Loc):-
    orig_clause_id(ClId,Orig_ClId),
    clause_locator(Orig_ClId,Loc).

:- pred add_clause_locator/2 : clkey * location_t.
add_clause_locator(ClKey, L) :-
    ( locator(ClKey,L) -> true
    ; asserta_fact(locator(ClKey, L))
    ).

:- pred literal_locator/2 : clkey * var => clkey * location_t.
literal_locator(LitKey,L):-
    clid_of_atomid(LitKey,ClKey),
    maybe_clause_locator(ClKey, L). % TODO: are we doing backtracking here?

% ===========================================================================
%! # Saving and restoring data for libcache (lib_*)

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

% Dumping data (for saving)
dump_lib_data(lib_defines(A,B,C)) :- defines(A,B,C), \+ fake_module_name(C).
dump_lib_data(lib_imports(A,B,C,D)) :- imports(A,B,C,D). % , \+ fake_module_name(B). % TODO: otherwise we get 'Unknown predicate' warnings in fixpo_ops
dump_lib_data(lib_exports(A,B)) :- exports(A,B), \+ fake_module_name(B).
dump_lib_data(lib_multifile(A,B)) :- multifile(A,B).
dump_lib_data(lib_meta(A,B)) :- meta(A,B), get_module_from_sg(A,M), \+ fake_module_name(M).
dump_lib_data(lib_dynamic(A)) :- dynamic(A), get_module_from_sg(A,M), \+ fake_module_name(M).
dump_lib_data(lib_defines_module(A,B)) :- defines_module(A,B), \+ fake_module_name(B).
dump_lib_data(lib_impl_defines(A,B)) :- impl_defines(A,B).
%
dump_lib_data(lib_assertion_read(PD,M,Status,Type,Body1,Dict,S,LB,LE)) :-
    assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE),
    assertion_body(Pred,Compat,Call,Succ,Comp,_Comm,Body),
    assertion_body(Pred,Compat,Call,Succ,Comp,"",Body1). %no comment is stored.
%
dump_lib_data(lib_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE)) :-
    prop_clause_read(M, Head, Body, VarNames, Source, LB, LE).

% Adding data (for restoring)
add_lib_data(lib_defines_module(A,B)) :- !,
    assertz_fact(lib_defines_module(A,B)),
    assertz_fact(lib_defines_module_rev_idx(B,A)).
add_lib_data(Fact) :-
    assertz_fact(Fact). % TODO: meta!

% ---------------------------------------------------------------------------

% TODO: port to fastrw (not working)

:- use_module(engine(io_basic)).
:- use_module(library(read), [read/2]).
:- use_module(library(write), [writeq/2]).

%:- use_module(library(fastrw), [fast_read/2, fast_write/2]).

:- export(save_lib_p_unit_db/1).
% Save facts from pgm_* to Stream
save_lib_p_unit_db(Stream):-
    dump_lib_data(Data),
    writeq(Stream,Data),display(Stream,'.'),nl(Stream),
    %fast_write(Stream, Data),
    fail.
save_lib_p_unit_db(_).

:- export(restore_lib_p_unit_db/1).
% Restore facts from Stream to lib_*
restore_lib_p_unit_db(Stream):-
    repeat,
    read(Stream,Fact),
    ( Fact = end_of_file ->
        !
    ; add_lib_data(Fact),
      fail
    ).
%    ( fast_read(Stream,Fact) ->
%        add_lib_data(Fact),
%        fail
%    ; !
%    ).
