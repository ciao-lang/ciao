:- module(actmod_tr, [], [assertions, dcg, datafacts]).

% Translation module for active modules


:- use_module(library(fibers/fibers_tr),
    [mod_error/2, stub_base/2, is_exported_nd/3]).

% ---------------------------------------------------------------------------
% Database to store actmod declarations (first phase)

:- data actmod_flag/2.

cleanup_db(M) :-
    retractall_fact(actmod_flag(M, _)).

set_curr_reg_protocol(M, RegProtocol) :-
    retractall_fact(actmod_flag(M, reg_protocol(_))),
    assertz_fact(actmod_flag(M, reg_protocol(RegProtocol))).

get_curr_reg_protocol(M, RegProtocol) :-
    ( actmod_flag(M, reg_protocol(RegProtocol)) ->
        true
    ; RegProtocol = filebased % default
    ).

% ---------------------------------------------------------------------------

:- use_module(library(lists), [append/3]).
:- use_module(library(aggregates), [findall/3]).

:- export(sentence_tr/3).
sentence_tr(0, 0, M) :- !,
    cleanup_db(M).
sentence_tr((:- Decl), Cs, M) :- !,
    decl_tr(Decl, Cs, M).
sentence_tr(end_of_file, Cs, M) :- !,
    Cs = ['$local_actmod'(M)|Cs0], % An actmod (with a mbox, etc.)
    gen_actmod_main(M, Cs0, Cs1),
    gen_use_reg_protocol(M, Cs1, Cs2),
    gen_actmod_meta(M, Cs2, Cs3),
    Cs3 = [end_of_file],
    cleanup_db(M).
sentence_tr(Sent, Cs, M) :-
    % Rename main/1 for instrumentation
    norm_clause(Sent, Head, Body),
    Head = main(_),
    !,
    head_prime(Head, Head2),
    FArgs2 = main_(term), % TODO: add types to Head2
    %
    % (first occurrence, mark and add needed decls)
    ( actmod_flag(M, actmod_main) ->
        Cs = Cs2
    ; % Instrument main/1
      Cs = [(:- suspendable(FArgs2))|Cs2],
      assertz_fact(actmod_flag(M, actmod_main))
    ),
    %
    Cs2 = [(Head2 :- Body)].

norm_clause(Sent, Head, Body) :-
    ( Sent = (Head :- Body) -> true
    ; Head = Sent, Body = true
    ).

% Renamed for instrumentation
head_prime(Head, Head2) :-
    Head =.. [F|Args],
    atom_concat(F, '_', F2),
    Head2 =.. [F2|Args].

% ---------------------------------------------------------------------------

% TODO: allow 'lazy', etc. in use_module/3 opts (even if not active)

decl_tr(use_module(ModSpec, Imports, Opts), Cs, M) :-
    member(active, Opts),
    !,
    decl_use_active_module(ModSpec, Imports, Opts, Cs, M).
decl_tr(actmod_reg_protocol(RegProtocol), Cs, M) :- !,
    Cs = [],
    set_curr_reg_protocol(M, RegProtocol).
decl_tr(dist_node, Cs, M) :- !, % Include runtime for distributed nodes
    Cs = [],
    assertz_fact(actmod_flag(M, dist_node)).
decl_tr(dist_start(G), Cs, M) :- !, % Default startup goal (for a distributed node)
    Cs = [],
    assertz_fact(actmod_flag(M, dist_start(G))).

:- import(c_itf, [module_from_base/2]). % TODO: export?
:- use_module(library(system), [file_exists/1]).
:- use_module(engine(stream_basic), [absolute_file_name/7]).

decl_use_active_module(ModSpec, Imports, Opts, Cs, M) :-
    lookup_actmod_files(ModSpec, DMod, StubPath),
    % (Assume that DMod is also an ActRef) % TODO: make it optional
    Cs = ['$static_named_actRef'(DMod, DMod)|Cs0],
    % Add registry protocol if needed
    ( member(reg_protocol(RegProtocol), Opts) ->
        Cs0 = ['$dmod_reg_protocol'(DMod, RegProtocol)|Cs1]
    ; Cs0 = Cs1
    ),
    % Libexec prop if needed
    ( member(libexec, Opts) ->
        Cs1 = ['$dmod_prop'(DMod, libexec)|Cs2]
    ; Cs1 = Cs2
    ),
    % ModSpec for this DMod (for binary generation and dynamic load)
    Cs2 = ['$dmod_src'(DMod, ModSpec)|Cs3],
    % Use stub or create default wrappers
    ( has_stub(StubPath) ->
        % Use .stub.pl
        % TODO: create the .stub.pl automatically:
        %   - inline stub, duplicates info!
        %   - re-use lazy load to implement it?
        ( var(Imports) ->
            Cs3 = [(:- use_module(StubPath))]
        ; Cs3 = [(:- use_module(StubPath, Imports))]
        )
    ; ( var(Imports) ->
          mod_error(M, ['Import list cannot be free if stub file is not available']), % TODO: fix it
          fail
      ; def_stub_wrappers(Imports, DMod, Cs3)
      )
    ).

% lookup_actmod_files(+ModSpec, -DMod, -StubPath)
lookup_actmod_files(ModSpec, DMod, StubPath) :-
    absolute_file_name(ModSpec, '_opt', '.pl', '.', _FileName, FileBase, _AbsDir),
    module_from_base(FileBase, DMod),
    stub_base(FileBase, StubPath).

has_stub(StubPath) :-
    atom_concat(StubPath, '.pl', F),
    file_exists(F).

% ---------------------------------------------------------------------------
% Default wrappers for DMod:
%   - when no .stub.pl is available (it may not be consistent with interface)
%   - for blocking calls from non-susp contexts

def_stub_wrappers([], _DMod, []).
def_stub_wrappers([F/A|Ps], DMod, [Def|Defs]) :-
    def_stub_wrapper(F, A, DMod, Def),
    def_stub_wrappers(Ps, DMod, Defs).

def_stub_wrapper(F, A, DMod, Def) :-
    functor(P, F, A),
    Def = (P :- '$actmod_call'(DMod:P)).

% ---------------------------------------------------------------------------
% Load registry protocol implementations

gen_use_reg_protocol(M, Cs, Cs0) :-
    get_curr_reg_protocol(M, RegProtocol),
    atom_concat('regp_', RegProtocol, Pub),
    Cs = [(:- use_module(library(actmod/Pub)))|Cs0].

% ---------------------------------------------------------------------------
% Instrumentation for main/1

% TODO: recover 'ciaoc -a' to force 'dist_node'?
% TODO: add support for initial goal in dist_node command-line arguments

gen_actmod_main(M, Cs, Cs0) :-
    % We had main, wrap it
    actmod_flag(M, actmod_main),
    !,
    Exp = '$ct_mexp_of_type'(goal, main_(Args), G1),
    Cs = [
        % (:- export(main/1)), % Note: Assume it is exported
        (main(Args) :- Exp, '$actmod_start_main'(G1))
        |Cs0].
gen_actmod_main(M, Cs, Cs0) :-
    % Dist node, use start goal
    actmod_flag(M, dist_node),
    !,
    get_curr_reg_protocol(M, RegProtocol),
    ( actmod_flag(M, dist_start(G)) ->
        Exp = '$ct_mexp_of_type'(goal, G, G1),
        G2 = G1
    ; Exp = true,
      G2 = true % No initial goal
    ),
    Cs = [
        (:- use_module(library(actmod/actmod_dist), [])), % for dist_init_args/4, called internally
        (:- export(main/1)),
        (main(Args) :- Exp, '$actmod_start_nohalt'(dist_init_args(RegProtocol, M, Args, G2)))
        |Cs0].
gen_actmod_main(_M, Cs, Cs). % No main/1

% ---------------------------------------------------------------------------

% Defines the '$actmod_exe'/3 wrapper predicate to do dynamic module
% resolution on the selected interface predicates.

gen_actmod_meta(M, Cs, Cs0) :-
    findall('$actmod_exe'(Pred, M, M:Pred), actmod_serves(M, Pred), ExeFacts),
    append(ExeFacts, Cs0, Cs).

actmod_serves(M, Pred) :-
    is_exported_nd(M, F, A),
    functor(Pred, F, A).

