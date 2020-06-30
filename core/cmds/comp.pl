:- module(_, [], [compiler(complang)]).

%! \title Compiler CLI (optim_comp)
%  \author Jose F. Morales
% TODO: merge with ciaoc.pl

:- use_module(compiler(module_deps)).
:- use_module(compiler(module_itf)).
:- use_module(compiler(module_exp)).
:- use_module(compiler(module_ideps)).
:- use_module(compiler(errlog)).
:- use_module(compiler(linker__bytecode)).
:- use_module(compiler(linker__bootstrap)).
:- use_module(compiler(memoize)).
:- use_module(compiler(store)).

:- use_module(engine(runtime_control), [set_prolog_flag/2]).

:- use_module(library(libpaths), [get_alias_path/0]).

:- use_module(compiler(dynload), [use_module/2]). % --exec
:- use_module(library(system), [system/2]).
:- use_module(library(terms), [atom_concat/2]).

% TODO: document: to be safe, dynamic use_module is forbidden in static comp
% TODO: document: this module is loaded to register memoize actions
:- use_module(compiler(all_actions), []).
% TODO: document: this module is loaded to register compilation modules
:- use_module(compiler(basic_compilation_modules), []).

:- public main/1.
main(Args) :-
    call((
      args :: accum <- Args,
      get_opts(Cmd)
    )),
    setup_alias_paths,
    option__comp_stats(ShowTime),
    ( maybe_comp_stats(ShowTime, cmd(Cmd)) -> true
    ; halt(-1)
    ).

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

% TODO: add support for bundles
setup_alias_paths :- option__use_alias_path(on), !,
    get_alias_path,
    ( current_fact(file_search_path(Alias, Path)), % TODO: BUG! breaks if current_fact/1 is not used here
        add_file_search_path(Alias, Path),
        fail
    ; true
    ).
setup_alias_paths.
      
{
:- fluid args :: accum.
get_opts(Cmd) :-
    % verbose compilation
    a('--verbose'), !, option__set_verbose,
    get_opts(Cmd).
get_opts(Cmd) :-
    % show compilation statistics
    a('--comp-stats'), !, option__set_comp_stats,
    get_opts(Cmd).
get_opts(Cmd) :-
    % analize all modules
    a('--analyze-all'), !, option__set_analyze_all,
    get_opts(Cmd).
get_opts(Cmd) :-
    % use CIAOALIASPATH
    a('--use-alias-path'), !, option__set_use_alias_path,
    get_opts(Cmd).
get_opts(Cmd) :-
    % dead instructions
    % TODO: change name
    a('--dead'), a(Name), !, get_dead(Name),
    get_opts(Cmd).
get_opts(do(Action, Spec0)) :-
    a('--do'), a(Action), a(Spec0), !.
get_opts(dynexec(ExecName, Specs0)) :- 
    a('--dynexec'), a(ExecName), !, tail(Specs0).
get_opts(link(ExecName, Specs0)) :- 
    a('--link'), a(ExecName), !, tail(Specs0).
get_opts(bootstrap(ExecName, Specs0)) :-
    a('--bootstrap'), a(ExecName), !, tail(Specs0).
get_opts(recursive_deps(Specs0)) :-
    a('--recursive-deps'), tail(Specs0), !.
get_opts(recursive_archbin_update(Specs0)) :-
    a('--recursive-archbin-update'), tail(Specs0), !.

% TODO: using args.add(X), but it is reading!
a(X) :- args.add(X).

% consume the rest of tokens
tail(X) :-
    X = ~args,
    args <- [].
}.

cmd(do(Action, Spec0)) :-
    store:find_source(Spec0, relpath('.'), Spec),
    functor(ActionG, Action, 1),
    arg(1, ActionG, Spec),
    comp:eval0(ActionG).
cmd(link(ExecName, Specs0)) :-
    store:find_sources(Specs0, relpath('.'), Specs),
    link(Specs, ExecName).
cmd(dynexec(ExecName, Specs0)) :-
    store:find_sources(Specs0, relpath('.'), Specs),
    dynexec(Specs, ExecName).
cmd(bootstrap(ExecName, Specs0)) :-
    store:find_sources(Specs0, relpath('.'), Specs),
    bootstrap(Specs, ExecName).
cmd(recursive_deps(Specs0)) :-
    store:find_sources(Specs0, relpath('.'), Specs),
    recursive_deps(Specs).
cmd(recursive_archbin_update(Specs0)) :-
    store:find_sources(Specs0, relpath('.'), Specs),
    recursive_archbin_update(Specs).

:- data option__verbose/1.
option__verbose(off).
option__set_verbose :- set_fact(option__verbose(on)).

:- data option__comp_stats/1.
option__comp_stats(off).
option__set_comp_stats :- set_fact(option__comp_stats(on)).

:- data option__analyze_all/1.
option__analyze_all(off).
option__set_analyze_all :- set_fact(option__analyze_all(on)).

:- data option__use_alias_path/1.
option__use_alias_path(off).
option__set_use_alias_path :- set_fact(option__use_alias_path(on)).

:- include(compiler(options__interface)).
global__options(Opts) :-
    option__analyze_all(on),
    !,
    Opts = [(:- '$pragma'(analyze_all))].

eval0(ActionG) :-
    Errs = ~errlog.new,
    option__verbose(Verbose),
    Errs.add(verbose(Verbose)),
    Memo = ~memoize.new(Errs),
    Memo.enter,
    Ok = ( Memo.eval0(ActionG) ? yes | no ),
    Memo.leave,
    Memo.delete,
    Errs.delete,
    Ok = yes.

link(Specs, ExecName) :-
    Errs = ~errlog.new,
    option__verbose(Verbose),
    Errs.add(verbose(Verbose)),
    memo :: memoize <- ~memoize.new(Errs),
%define_flag(executables, [static, eagerload, lazyload], eagerload).
%define_flag(self_contained,atom,none).
%define_flag(compress_exec,[yes,no],no).
%   set_prolog_flag(compress_exec, yes),
    set_prolog_flag(executables, eagerload),
    memo.enter,
    Ok = ( linker__bytecode:link(Specs, ExecName, _NativeInfo) ? yes | no ),
    memo.leave,
    memo.delete,
    Errs.delete,
    Ok = yes.

dynexec(Specs, ExecName) :-
    Errs = ~errlog.new,
    option__verbose(Verbose),
    Errs.add(verbose(Verbose)),
    memo :: memoize <- ~memoize.new(Errs),
%define_flag(executables, [static, eagerload, lazyload], eagerload).
%define_flag(self_contained,atom,none).
%define_flag(compress_exec,[yes,no],no).
%   set_prolog_flag(compress_exec, yes),
    set_prolog_flag(executables, eagerload),
    memo.enter,
    Ok = ( recursive_archbin_update__2(Specs),
           linker__bytecode:link(Specs, ExecName, _NativeInfo) ? yes
         | no
         ),
    memo.leave,
    memo.delete,
    Errs.delete,
    Ok = yes.

:- use_module(compiler(dynload), ['$only_static'/0]).
bootstrap(Specs, ExecName) :-
    Errs = ~errlog.new,
    option__verbose(Verbose),
    Errs.add(verbose(Verbose)),
    asserta_fact(dynload:'$only_static'), % TODO: document!!!???
    memo :: memoize <- ~memoize.new(Errs),
    memo.enter,
    Ok = ( linker__bootstrap:make_bootstrap(Specs, ExecName) ?
             yes 
         | no 
         ),
    memo.leave,
    memo.delete,
    Errs.delete,
    Ok = yes.

% ---------------------------------------------------------------------------

:- use_module(compiler(global_pass)).
:- use_module(library(write)).

recursive_deps(Specs) :-
    Errs = ~errlog.new,
    option__verbose(Verbose),
    Errs.add(verbose(Verbose)),
    memo :: memoize <- ~memoize.new(Errs),
    memo.enter,
    ( global_pass:reachable_module_set(Specs, ProcessedSpecs),
      member(P, ProcessedSpecs),
      store:denormalized_spec(P, P2),
      write_spec(P2), nl,
      fail
    ; true
    ),
    memo.leave,
    memo.delete,
    Errs.delete.

write_spec(X) :- atom(X), !, writeq(X).
write_spec(X) :-
    functor(X, N, 1),
    arg(1, X, Y),
    write(N),
    write('/'),
    write_spec(Y).

% ---------------------------------------------------------------------------

:- use_module(compiler(global_pass)).

% TODO: why?!?!?!?
% TODO: DOES NOT WORK!
recursive_archbin_update(Specs) :-
    Errs = ~errlog.new,
    option__verbose(Verbose),
    Errs.add(verbose(Verbose)),
    memo :: memoize <- ~memoize.new(Errs),
    memo.enter,
    recursive_archbin_update__2(Specs),
    memo.leave,
    memo.delete,
    Errs.delete.
{
:- fluid memo :: memoize.
recursive_archbin_update__2(Specs) :-
    global_pass:transitive_archcompile_stop(Specs, in_loader, _ProcessedSpecs).
}.

% TODO: incomplete, it should be all the loader modules
in_loader([engine|_]). 

% ---------------------------------------------------------------------------

% Load dead instructions info
% TODO: this code is temporary for the sabsmach paper...
% TODO: we should create a emulator later... after composing the bytecode
%   in a single file, not in two steps...

:- use_module(library(read)).

:- multifile(unused_opcode/1).
:- data(unused_opcode/1).
:- multifile(comp_option/1).
:- data(comp_option/1).

get_dead(Name) :-
    '$open'(Name, r, Stream),
    repeat,
      read(Stream, X),
      ( X = end_of_file
      ; get_dead__2(X),
        fail
      ), !,
    close(Stream).

get_dead__2(comp_option(X)) :-
    assertz_fact(comp_option(X)).
get_dead__2(unused_opcode(X)) :-
    assertz_fact(unused_opcode(X)).

% ---------------------------------------------------------------------------

:- use_module(engine(runtime_control), [statistics/2]).

:- meta_predicate maybe_comp_stats(?, goal).
maybe_comp_stats(on, G) :- !,
    statistics(walltime, [L1|_]),
    G,
    statistics(walltime, [L2|_]),
    Ld is L2 - L1,
    display(user_error, '{done in '),
    display(user_error, Ld),
    display(user_error, ' ms}'),
    nl(user_error).
maybe_comp_stats(_, G) :- G.

