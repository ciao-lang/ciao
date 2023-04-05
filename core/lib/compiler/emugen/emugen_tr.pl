:- module(emugen_tr, [], [dcg, fsyntax, assertions, datafacts]).

:- doc(title, "Generator of bytecode emulator").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module generates part of the bytecode emulator
   from a specification of the instruction set and auxiliary data
   structures.").

:- doc(bug, "This is a simplified version of the optim_comp
   generator. Please consider a more complete backport before further
   improvements are included here.").

:- use_module(library(aggregates), [findall/3, bagof/3]).

:- use_module(engine(messages_basic), [message/2]).
:- use_module(library(read)).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(lists)).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(ciaobld(eng_defs), [emugen_code_dir/3]).

:- export(emugen_sent/3).
emugen_sent(0, _, M) :- !,
    clean_db(M).
emugen_sent((:- Decl), [], M) :- !,
    emugen_decl(Decl, M).
emugen_sent((Head :- Body), [], M) :- nonvar(Head), !,
    assertz_fact(clause_def(Head, M, Body)).
emugen_sent((GHead => Body), [], M) :- nonvar(GHead), !,
    ( GHead = (Head, Guard0), nonvar(Guard0), Guard0=[Guard] -> true % guard uses [[_]] notation
    ; Head = GHead, Guard = []
    ),
    assertz_fact(rule_def(Head, M, Guard, Body)).
emugen_sent(end_of_file, _, M) :- !,
    generate_code(M).

% ---------------------------------------------------------------------------
% Declaration processing

% NOTE: 'native' = low level foreign C code with direct access to the
%   Prolog machinery.

% Use .c file source in native code
emugen_decl('$native_include_c_source'(File), M) :- !,
    assertz_fact(use_native(File, M, c)).
% Declare a .h header for the corresponding native code (directly in include/ without any alias)
emugen_decl('$native_include_c_header'(File, noalias), M) :- !,
    assertz_fact(use_native(File, M, h_noalias)).
% Declare a .h header for the corresponding native code (in include/H_ALIAS)
emugen_decl('$native_include_c_header'(File), M) :- !,
    assertz_fact(use_native(File, M, h)).
% Export some item to native code
emugen_decl(native_export(Item, File), M) :- !,
    assertz_fact(native_export(Item, M, File)).
emugen_decl(ftype_def(FType, Id, Format), M) :- !,
    add_ftype_def(FType, Id, Format, M).
emugen_decl('$decl'(Spec), M) :- !,
    put_tkval('\006\dot'(rule(Spec),level), M, decl).
emugen_decl(engine_opts(Opts), M) :- !,
    assertz_fact(engine_opts(Opts, M)).
emugen_decl(engine_stubmain(Opts), M) :- !,
    assertz_fact(engine_stubmain(Opts, M)).
emugen_decl(Decl, M) :- !,
    exec_decl(Decl, M).

% TODO: 
%  - define integrity constraints AND 
%  - automatically maintain the reverse index AND
%  - automatically maintain the max aggregate 

exec_decl(true, _M) :- !.
exec_decl((A,B), M) :- !,
    exec_decl(A, M),
    exec_decl(B, M).
exec_decl(G, M) :-
    % TODO: detect loops!
    functor(G, F, N),
    get_tkval('\006\dot'(rule(F/N),level), M, decl),
    !,
    ( clause_def(G, M, Def) -> true ; fail ), % (once)
    exec_decl(Def, M).
exec_decl(put(K, V), M) :- !,
    put_tkval(K, M, V).
exec_decl(putmax(K, V), M) :- !,
    ( get_tkval(K, M, V0) ->
        ( V > V0 ->
            put_tkval(K, M, V)
        ; true
        )
    ; put_tkval(K, M, V)
    ).
% TODO: Hack to prepare decls from rules
exec_decl([As], M) :- is_list(As), !,
    simp_constrs(As, M, [], _Store).
% TODO: Hack to execute rules from list
exec_decl('$exec_decls'(Xs), M) :- !,
    list_to_conj(Xs, G),
    exec_decl(G, M).
% TODO: Hack to execute rules from lists
exec_decl('$decl'(G), M) :- !, exec_decl(G, M).

% ---------------------------------------------------------------------------
% Code generation

:- use_module(engine(runtime_control), [statistics/2]).

emugen_statistics.
%emugen_statistics :- fail.

generate_code(M) :-
    ( emugen_statistics ->
        message(note, ['Generating code for ', M]),
        statistics(runtime, [T0|_]),
        generate_code_(M),
        statistics(runtime, [T1|_]),
        T is floor(T1 - T0),
        message(note, ['Code generated in ', T, 'ms.'])
    ; generate_code_(M)
    ).

generate_code_(M) :-
    catch(generate_code__(M), E, handler(E)).

generate_code__(M) :-
    ( % (failure-driven loop)
      native_export(Item, M, File),
        code_to_file(M, Item, File),
        fail
    ; true
    ).

get_eng_opts(M, EngOpts) :-
    ( engine_opts(EngOpts, M) -> true ; EngOpts = [] ).

:- use_module(library(system_extra), [mkpath/1]).

code_to_file(M, Item, File) :-
    ( code_to_file_(M, Item, File) -> true
    ; % TODO: emit error in a proper way
      throw(error_during_code_generation(Item))
    ).

code_to_file_(M, Code, File) :-
    this_eng_def(M, Eng),
    %
    code_to_cexp(Code, M, CExp),
    cexp_to_str(CExp, String, []),
    % Emit files in the right builddir path
    emugen_code_dir(Eng, File, DestDir),
    mkpath(DestDir),
    File2 = ~path_concat(DestDir, File),
    string_to_file(String, File2).

% ---------------------------------------------------------------------------

:- use_module(library(bundle/bundle_paths), [reverse_bundle_path/3]).
:- use_module(library(compiler/c_itf), [defines_module/2]).

% Engine definition for M
this_eng_def(M, Eng) :-
    get_eng_opts(M, EngOpts),
    ( defines_module(Base, M) -> true ; false ),
    reverse_bundle_path(Base, Bundle, Rel),
    EngMainSpec = Rel,
    Eng = eng_def(Bundle, EngMainSpec, EngOpts).

% ---------------------------------------------------------------------------

code_to_cexp(code(Code), M, CExp) :-
    emit_code(Code, M, [indent(0)], CExp, []).

emit_code(G, M, Store) -->
    { tr_solve(G, M, Store, Body, _Store) },
    [Body].

% ---------------------------------------------------------------------------
% Error handler and diagnosis

:- use_module(library(compiler/emugen/emugen_errors)).
:- use_module(engine(runtime_control), [set_prolog_flag/2, prolog_flag/3]).

handler(E) :-
    handler_msg(E, Msg),
    !,
    prolog_flag(write_strings, Old, on),
    message(error, Msg),
    set_prolog_flag(write_strings, Old),
    fail.
handler(E) :-
    message(error, ['Unknown error: ', E]),
    fail.

% ---------------------------------------------------------------------------
% Helper predicates

uppercase([]) := [].
uppercase([X|Xs]) := [~uppercase_2(X)|~uppercase(Xs)].

uppercase_2(X0) := X :- X0 >= 0'a, X0 =< 0'z, !,
    X is X0 + 0'A - 0'a.
uppercase_2(X) := X.

emit_uppercase(X) -->
    { Codes = ~uppercase(~atom_codes(X)) },
    emit_string(Codes).

emit_atom(X) -->
    { Codes = ~atom_codes(X) },
    emit_string(Codes).

emit_number(X) -->
    { Codes = ~number_codes(X) },
    emit_string(Codes).

emit_qstring(X) -->
    { transparent_string(X, X2) },
    "\"", emit_string(X2), "\"". % TODO: better escape (see write_c.pl too)

transparent_string([], []) :- !.
transparent_string("\n"||Xs, "\\n"||Ys) :- !,
    transparent_string(Xs, Ys).
transparent_string([X|Xs], [X|Ys]) :- integer(X),
    transparent_string(Xs, Ys).

cexp_to_str(X) --> { is_list(X) }, !, cexp_to_str_(X). % (flatten)
% Annotation for resolution step (ignore)
cexp_to_str('$rs'(_G,X)) --> !, cexp_to_str(X).
% layout, tokens
cexp_to_str((A,B)) --> !, cexp_to_str(A), cexp_to_str(B).
cexp_to_str(true) --> !.
cexp_to_str(X) --> { number(X) }, !, emit_number(X).
cexp_to_str(tk(X)) --> !, emit_atom(X).
cexp_to_str(tk_nl) --> !, "\n".
cexp_to_str(tk_bb(N)) --> !, fmt_bb(N).
cexp_to_str(tk_string(X)) --> !, emit_qstring(X).
cexp_to_str(tk_number(X)) --> !, emit_number(X).
%
cexp_to_str(callexp(N, Xs)) --> !, emit_atom(N), "(", emit_args(Xs), ")".
%
cexp_to_str(X) --> { throw(internal_error_bad_cexp(X)) }.

cexp_to_str_([]) --> [].
cexp_to_str_([X|Xs]) --> cexp_to_str(X), cexp_to_str_(Xs).

emit_args([]) --> !.
emit_args([X]) --> !,
    cexp_to_str(X).
emit_args([X|Xs]) --> 
    cexp_to_str(X), ",",
    emit_args(Xs).

fmt_bb(N) --> { N =< 0 }, !, "".
fmt_bb(N) --> " ", { N1 is N - 1 }, fmt_bb(N1).

is_string([]).
is_string([C|_]) :- integer(C).

is_list([]).
is_list([_|_]).

emit_string([]) --> [].
emit_string([X|Xs]) --> [X], emit_string(Xs).

% ---------------------------------------------------------------------------
% Abstract machine definition database

% native_export(Item, M, File)
:- data native_export/3.

% use_native(File, M, Kind)
:- data use_native/3.

% ftype_def(FType, M, Id, Format)
:- data ftype_def/4.
% id_ftype(Id, M, FType)
:- data id_ftype/3.
% max_ftype(MaxId, M): Maximum Id for ftypes
:- data max_ftype/2.

% key/value table
% tkval(Idx, M, Key, Val)
:- data tkval/4.

% clause_def(Head, M, Body). % TODO: only for level=decl
:- data clause_def/3.
% rule_def(Head, M, Guard, Body). % (_ => _) rules
:- data rule_def/4.

% engine_opts(Opts, M).
:- data engine_opts/2.

% engine_opts(StubMain, M).
:- data engine_stubmain/2.

clean_db(M) :-
    retractall_fact(native_export(_,M,_)),
    retractall_fact(use_native(_,M,_)),
    %
    retractall_fact(ftype_def(_,M,_,_)),
    retractall_fact(id_ftype(_,M,_)),
    retractall_fact(max_ftype(_,M)),
    %
    retractall_fact(tkval(_,M,_,_)),
    retractall_fact(clause_def(_,M,_)),
    retractall_fact(rule_def(_,M,_,_)),
    %
    retractall_fact(engine_opts(_,M)),
    retractall_fact(engine_stubmain(_,M)).

:- use_module(library(indexer/hash), [hash_term/2]). % TODO: slow, add C implementation

tkhash('\006\dot'(G0,Key), Idx) :- !, hash_term(G0-Key,Idx).
tkhash(Key, Idx) :- hash_term(Key, Idx).

put_tkval(Key, M, Val) :-
    tkhash(Key, Idx),
    retractall_fact(tkval(Idx, M, Key, _)),
    assertz_fact(tkval(Idx, M, Key, Val)).

get_tkval(Key, M, Val) :-
    tkhash(Key, Idx),
    current_fact(tkval(Idx, M, Key, Val0)), !,
    Val = Val0.

add_ftype_def(FType, Id, Format, M) :-
    assertz_fact(ftype_def(FType, M, Id, Format)),
    % Update max_ftype
    ( max_ftype(MaxId, M) -> true
    ; MaxId = -1
    ),
    ( Id > MaxId ->
        retractall_fact(max_ftype(_, M)),
        assertz_fact(max_ftype(Id, M))
    ; true
    ),
    % Update reverse index id_ftype
    assertz_fact(id_ftype(Id, M, FType)).

% ---------------------------------------------------------------------------

% TODO: Implement a OR-store? as well as simplifications 
%   E.g., [[mode(r)],[mode(w)]] ==> [[mode(_)]]
% Success if the constraint is consistent with the accumulated
% constraints.
store_tell(Store, Constr) :-
    member(Constr, Store),
    !.

% Replace a constraint in a store
store_replace(Store0, Constr, Store) :-
    functor(Constr, N, A),
    functor(Constr0, N, A),
    select(Constr0, Store0, Store1),
    !,
    Store = [Constr|Store1].
store_replace(Store0, Constr, Store) :-
    Store = [Constr|Store0].

% ---------------------------------------------------------------------------
% TODO: Implement better strategies
% TODO: do something with final Store?

% Rewrite G as Body
% Throw exceptions for diagnosis.
tr_solve(G, M, Store0, Body, Store) :-
    '$metachoice'(Chpt0),
    tr_solve_(G, M, Store0, Store1, positive, Body1), 
    '$metachoice'(Chpt),
    ChptDepth is Chpt-Chpt0,
    ( ChptDepth > 0 -> message(note, ['nondet translation! (', ChptDepth, ') ', G]), fail
    ; true
    ),
    !,
    Store = Store1,
    Body = Body1.
tr_solve(G, M, Store0, Body, Store) :-
    % TODO: more efficient?
    tr_solve_set(G, M, Store0, positive, BodyStores),
    ( BodyStores = [] ->
        % Repeat translation to collect all failed translations
        tr_solve_set(G, M, Store0, all, AllBodyStores),
        unzip(AllBodyStores, Bodies, _),
        throw(no_translation_for(Bodies, Store0))
    ; BodyStores = [_,_|_] ->
        % Repeat translation to collect multiple translations
        tr_solve_set(G, M, Store0, positive, AllBodyStores),
        unzip(AllBodyStores, Bodies, _),
        throw(multiple_possible_translations(Bodies, Store0))
    ; BodyStores = [Body-Store],
      throw(single_translation_multiple_choicepoints) % TODO: since first clause of tr_solve failed
    ).

% Obtain the set of all translations to G, Store0 (for error diagnosis)
% (see tr_solve_)
% TODO: rules should be deterministic; do not use bagof
tr_solve_set(G, M, Store0, Sign, BodyStores) :-
    ( bagof(Body-Store, tr_solve_(G, M, Store0, Store, Sign, Body), BodyStores0) ->
        BodyStores = BodyStores0
    ; BodyStores = []
    ).

% unzip a list of pairs
unzip([], [], []).
unzip([X-Y|XYs], [X|Xs], [Y|Ys]) :- unzip(XYs, Xs, Ys).

% Obtain translations (non-deterministically) to G given Store0.
% If Sign=positive, we get successful translations.
% If Sign=all, we get all translations (including failed). 
tr_solve_(G, M, Store0, Store, Sign, Body) :-
    simp(G, M, Store0, Store, Body),
    ( Sign = positive -> \+ Store = '$fail'
    ; Sign = all -> true
    ).

simp((A,B), M, Store0, Store, R) :- !,
    simp(A, M, Store0, Store1, A1),
    ( Store1 = '$fail' ->
        R = A1
    ; simp(B, M, Store1, Store, B1),
      simp_conj(A1, B1, R)
    ).
% Constraint as guard (committed choice)
simp((CA ; B), M, Store0, Store, R) :- nonvar(CA), CA = (C->A), nonvar(C), C = [C0], is_list(C0), !,
    ( simp_constrs(C0, M, Store0, Store1), \+ Store1 = '$fail' ->
        simp(A, M, Store1, Store, R)
    ; simp(B, M, Store0, Store, R)
    ).
% Choose one alternative
simp((A ; B), M, Store0, Store, R) :- nonvar(A), !,
    ( simp(A, M, Store0, Store, R)
    ; simp(B, M, Store0, Store, R)
    ).
simp(A, M, Store0, Store, R) :-
    simp_lit(A, M, Store0, Store, R).

% List to conjunction
list_to_conj([], A) :- !, A = true.
list_to_conj([A], A) :- !.
list_to_conj([A|As], (A,Bs)) :-
    list_to_conj(As, Bs).

% Conjunction to list
conj_to_list(A, Xs) :-
    conj_to_list_(A, Xs, []).

conj_to_list_(A, Xs, Xs0) :- var(A), !, Xs = [A|Xs0].
conj_to_list_((A,B), Xs, Xs0) :- !,
    conj_to_list_(A, Xs, Xs1),
    conj_to_list_(B, Xs1, Xs0).
conj_to_list_(true, Xs, Xs0) :- !, Xs = Xs0.
conj_to_list_(A, [A|Xs], Xs).

simp_lit([As], M, Store0, Store, R) :- is_list(As), !, % [[...]] notation
    simp_constrs(As, M, Store0, Store),
    ( Store = '$fail' ->
        R = '$fail_lit'
    ; R = true
    ).
simp_lit('$foreach'(Xs, P), M, Store0, Store, R) :- !,
    foreach(Xs, P, M, Store0, Store, Code, []),
    R = Code.
simp_lit('$foreach_sep'(Sep, Xs, P), M, Store0, Store, R) :- !,
    % TODO: improve
    % Like $foreach but emits Sep 
    foreach_sep(Xs, Sep, P, M, Store0, Store, Code, []),
    R = Code.
simp_lit(callexp(N,Args), M, Store0, Store, R) :- !,
    R = callexp(N,Args2),
    simpargs(Args, M, Store0, Store, Args2).
simp_lit(G, _M, Store0, Store, R) :-
    prim_lit(G, G2), % (primitive, no translation)
    !,
    Store = Store0,
    R = G2.
simp_lit(G, M, Store0, Store, R) :- 
    % TODO: detect unfold loops!
    % TODO: allow multi-passes (delay translation if we do not
    % have enough information)
    unfold_lit(G, M, Store0, Store, R).

prim_lit(G, G) :- integer(G).
prim_lit(G, G) :- G = true.
prim_lit(G, G) :- G = tk(_).
prim_lit(G, G) :- G = tk_nl.
prim_lit(G, G) :- G = tk_bb(_).
prim_lit(G, G) :- G = tk_string(_).
prim_lit(G, G) :- G = tk_number(_).
prim_lit(prim(G), G).

simpargs([], _M, Store, Store, []).
simpargs([X|Xs], M, Store0, Store, [Y|Ys]) :-
    simp(X, M, Store0, Store1, Y),
    simpargs(Xs, M, Store1, Store, Ys).

% TODO: add a level to tr_solve
foreach([], _P, _M, Store, Store) --> [].
foreach([X|Xs], P, M, Store0, Store) -->
    foreach_(X, P, M, Store0, Store1),
    foreach(Xs, P, M, Store1, Store).

% TODO: add a level to tr_solve
foreach_sep([], _Sep, _P, _M, Store, Store) --> [].
foreach_sep([X], _Sep, P, M, Store0, Store) --> !,
    foreach_(X, P, M, Store0, Store).
foreach_sep([X|Xs], Sep, P, M, Store0, Store) -->
    foreach_(X, P, M, Store0, Store1),
    [tk(Sep)],
    foreach_sep(Xs, Sep, P, M, Store1, Store).

% (alternative translations are committed)
foreach_(X, P, M, Store0, Store) -->
    { G =.. [P, X] },
    { tr_solve(G, M, Store0, Body, Store) },
    [Body].

unfold_lit(G, M, Store0, Store, R) :-
    ( functor(G, F, N),
      get_tkval('\006\dot'(rule(F/N),level), M, grammar) ->
        % Do not annotate grammar level rules
        R = R0
    ; % Mark resolution step
      R = '$rs'(G, R0)
    ),
    % Obtain clause definitions that match G
    % (or set store to '$fail' state if there is no one)
    ( rule_def(G, M, Guard, Def),
      simp_constrs(Guard, M, Store0, Store1),
      \+ Store1 = '$fail'
      -> % (first where guard holds)
        simp(Def, M, Store1, Store, R0)
    ; % no solution
      Store = '$fail',
      R0 = '$fail_lit'
    ).

simp_constrs([], _M, Store, Store).
simp_constrs([C|Cs], M, Store0, Store) :-
    simp_constr(C, M, Store0, Store1),
    ( Store1 = '$fail' ->
        Store = Store1
    ; simp_constrs(Cs, M, Store1, Store)
    ).

% TODO: use constraints and solve lazily
simp_constr(update(Constraint), _M, Store0, Store) :- !,
    store_replace(Store0, Constraint, Store).
simp_constr(newid(Prefix, Id), _M, Store0, Store) :- !,
    ( store_tell(Store0, emugen_id_counter(N)) -> true
    ; N = 0
    ),
    number_codes(N, NCs),
    atom_codes(Prefix, PrefixCs),
    append(PrefixCs, NCs, IdCs),
    atom_codes(IdAtm, IdCs),
    Id = tk(IdAtm),
    N1 is N + 1,
    store_replace(Store0, emugen_id_counter(N1), Store).

simp_constr(Constraint, M, Store0, Store) :-
    ( simp_constr_(Constraint, M, Store0) ->
        Store = Store0
    ; % no possible code
      Store = '$fail'
    ).

% TODO: check instantiation or delay, allow user-defined built-ins
% TODO: Check negation
simp_constr_(not(G), M, Store) :- !,
    \+ simp_constr_(G, M, Store).
simp_constr_(integer(A), _M, _) :- !, integer(A).
simp_constr_(atom(A), _M, _) :- !, atom(A).
% TODO: Check arithmetic
simp_constr_(A is B, _M, _) :- !,
    A is B.
simp_constr_(A < B, _M, _) :- !, A < B.
simp_constr_(A > B, _M, _) :- !, A > B.
simp_constr_(A =< B, _M, _) :- !, A =< B.
simp_constr_(A >= B, _M, _) :- !, A >= B.
simp_constr_(A = B, _M, _) :- !, A = B.
simp_constr_(A \= B, _M, _) :- !, A \= B.
simp_constr_(conj_to_list(A,B), _M, _) :- !,
    conj_to_list(A,B).
simp_constr_(atom_concat(A,B,C), _M, _) :- !,
    atom_concat(A,B,C).
simp_constr_(uppercase(Ins, InsUp), _M, _) :- !,
    emit_uppercase(Ins, InsUp0, []),
    atom_codes(InsUp, InsUp0).
simp_constr_(get(K, V), M, _Store) :- !,
    get_tkval(K, M, V).
simp_constr_(ftype_def(FType, Id, Format), M, _Store) :- !,
    ftype_def(FType, M, Id, Format).
simp_constr_(collect_and_filter(G, What, Xs), M, Store) :- !,
    collect_and_filter(G, M, What, Store, Xs).
simp_constr_(G, M, _Store) :- decl_fact(G), !,
    fact_query(G, M).
simp_constr_(findall(X, G, Xs), M, _Store) :- !,
    findall(X, findall_query(G, M), Xs).
simp_constr_(range(First, Last, Xs), _M, _Store) :- !, % TODO: do checks
    range(First, Last, Xs).
simp_constr_(map_ftype_id(Xs,Ys), _M, _Store) :- !,
    map_ftype_id(Xs,Ys).
simp_constr_(length(Xs,N), _M, _Store) :- !,
    length(Xs,N).
simp_constr_(Constraint, _M, Store) :-
    store_tell(Store, Constraint),
    !.

% TODO: Generalize for other props
decl_fact(max_ftype(_)). % max_ftype(-)
decl_fact(id_ftype(_,_)). % id_ftype(+,-)
decl_fact(engine_stubmain(_)). % engine_stubmain(-)

% TODO: Generalize for other props
fact_query(max_ftype(MaxFType), M) :- max_ftype(MaxFType, M).
fact_query(id_ftype(Id, FType), M) :- ( id_ftype(Id, M, FType0) -> FType=FType0 ; fail ).
fact_query(engine_stubmain(A), M) :- engine_stubmain(A, M).

% TODO: Generalize for other props
findall_query(use_native(A, B), M) :- use_native(A, M, B).

range(X, Last, Xs) :- X > Last, !, Xs = [].
range(X, Last, [X|Xs]) :-
    X1 is X + 1,
    range(X1, Last, Xs).

simp_conj(true, A, R) :- !, R = A.
simp_conj(A, true, R) :- !, R = A.
simp_conj(A, B, (A, B)).

% Reduce G and collect and filtered primitive elems
collect_and_filter(G, M, What, Store, Insns) :-
    tr_solve(G, M, Store, Body, _),
    ( clean_rs(Body,Body1) -> true
    ; throw(cannot_clean_rs(G, What))
    ),
    ( collect_prims(Body1, M, What, Insns, []) -> true
    ; throw(cannot_collect_prims(G, What))
    ).

clean_rs((A, B), R) :- !,
    clean_rs(A, A2),
    clean_rs(B, B2),
    R = (A2, B2).
clean_rs('$rs'(_G, X), R) :- !,
    clean_rs(X, R).
clean_rs(X, X).

% collect primitives
collect_prims(true, _M, _What) --> !.
collect_prims((A,B), M, What) --> !,
    collect_prims(A, M, What),
    collect_prims(B, M, What).
collect_prims(G, _M, What) --> { prim_filter(G, What) }, !, [G].
collect_prims(_, _, _) --> [].

prim_filter(X, F/N) :- nonvar(X), functor(X, F, N).

map_ftype_id([]) := [].
map_ftype_id([X|Xs]) := [ftype_id(X)| ~map_ftype_id(Xs)].

