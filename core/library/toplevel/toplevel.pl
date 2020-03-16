:- module(_, [], [assertions, nortchecks, define_flag, datafacts]).

:- doc(title, "Interactive toplevels").
:- doc(author, "The Ciao Development Team").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides support for creating interactive
   toplevels (also known as @concept{REPL} or @concept{language
   shell}) for Ciao.").

:- if(defined(optim_comp)).
:- use_package(hiord).
:- use_package(compiler(complang_mini)).
:- use_package(compiler(compiler_object)).
:- else.
:- use_module(engine(hiord_rt), [call/1]).
:- endif.

:- if(defined(optim_comp)).
% (all compile capabilities, e.g., compile from source, etc.)
:- use_module(compiler(all_actions), []).
:- endif.

:- use_module(library(debugger)).
:- if(defined(optim_comp)).
:- use_module(engine(debugger_support), [reset_debugger/1]).
:- endif.
%
:- use_module(library(toplevel/toplevel_io)).
:- use_module(library(toplevel/prettysols)).
:- use_module(engine(messages_basic), [message/2]).
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
%
:- use_module(engine(internals), ['$bootversion'/0, '$force_interactive'/0]).
:- use_module(engine(internals), ['$empty_gcdef_bin'/0]).
:- if(defined(optim_comp)).
:- use_module(engine(internals), ['$abolish'/1]). % TODO: why?
:- endif.
:- use_module(library(libpaths), [get_alias_path/0]).

:- use_module(engine(runtime_control), [current_prolog_flag/2, prolog_flag/3]).

% ---------------------------------------------------------------------------
% Access to toplevel_scope (a 'user' module)

% ('user' modules need to be imported with ensure_loaded/1)
:- ensure_loaded(library(toplevel/toplevel_scope)).
:- import(user, ['$shell_module'/1, '$shell_call'/1]).

:- multifile '$shell_call_in_mod'/2.
'$shell_call_in_mod'(Mod, Goal) :-
    current_fact(shell_module(Mod)), !, '$shell_call'(Goal).

:- data shell_module/1. % Module where queries are called

% Get shell module, initialize with '$shell_module'/1 if none
get_shell_module(Mod) :-
    ( current_fact(shell_module(Mod0)) -> true
    ; '$shell_module'(Mod0),
      set_fact(shell_module(Mod0))
    ),
    Mod = Mod0.

% ---------------------------------------------------------------------------

:- multifile exit_hook/0, after_query_hook/0, after_solution_hook/0.

:- if(defined(optim_comp)).
shell_hook(exit) :- ( exit_hook, fail ; true ).
shell_hook(after_query) :- ( after_query_hook, fail ; true ).
shell_hook(after_solution) :- ( after_solution_hook, fail ; true ).
:- else.
:- use_module(engine(hiord_rt), ['$nodebug_call'/1]).
shell_hook(exit) :- ( '$nodebug_call'(exit_hook), fail ; true ).
shell_hook(after_query) :- ( '$nodebug_call'(after_query_hook), fail ; true ).
shell_hook(after_solution) :- ( '$nodebug_call'(after_solution_hook), fail ; true ).
:- endif.

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).
:- use_module(library(operators), [op/3]).
:- use_module(library(read_from_string)). % TODO: fixme

define_flag(prompt_alternatives_no_bindings, [on, off], off).

:- export(toplevel/1). % (not a top-level command)
toplevel(Args) :-
    get_alias_path,
    %
    retractall_fact(quiet_mode),
    interpret_args(Args, true),
    ( quiet_mode -> true ; '$bootversion' ),
    op(900, fy, [(spy),(nospy)]), % TODO: optional?
    shell_body,
    shell_hook(exit).

:- data quiet_mode/0.

interpret_args([], DefLoad) :- !,
    ( DefLoad = true ->
        load_default
    ; true
    ).
interpret_args(['-f'|R], _) :- !, % fast start
    interpret_args(R, false).
interpret_args(['-q'|R], DefLoad) :- !, % quiet mode
    set_fact(quiet_mode),
    interpret_args(R, DefLoad).
interpret_args(['-i'|R], DefLoad) :- !,
    '$force_interactive',
    interpret_args(R, DefLoad).
interpret_args(['--version'], _) :- !,
    '$bootversion', % Display Ciao version
    halt.
interpret_args(['-l',File|R], _) :- !,
    ( file_exists(File) ->
        include(File)
    ; message(warning, ['File not found (-l option): ', File])
    ),
    interpret_args(R, false).
interpret_args(['-u',File|R], DefLoad) :- !,
    use_module(File),
    interpret_args(R, DefLoad).
interpret_args(['-e',Query|R], DefLoad) :- !,
    read_from_atom(Query, Goal),
    '$shell_call'(Goal), % TODO: use query_call instead
    interpret_args(R, DefLoad).
interpret_args(['-p',Prompt|R], DefLoad) :- !,
    top_prompt(_, Prompt),
    interpret_args(R, DefLoad).
interpret_args(_Args, _) :-
    display(
'Usage: ciaosh [-f] [-q] [-i] [-l <File>] [-u <File>] [-p <Prompt>] [-e <Query>]'),
    nl,
    halt(1).

load_default :-
    RCFile = '~/.ciaorc',
    ( file_exists(RCFile) ->
        include(RCFile)
    ; default_shell_package(Package),
      prolog_flag(quiet, QF, warning),
      use_package(Package),
      prolog_flag(quiet, _, QF) 
    ).

:- if(defined(optim_comp)).
default_shell_package(default_for_ciaosh).
:- else.
:- use_module(library(compiler/c_itf), [default_shell_package/1]).
:- endif.

% ---------------------------------------------------------------------------

:- use_module(library(errhandle), [error_protect/2, default_error_message/1]).

:- export('$shell_abort'/0). % (not a top-level command)
'$shell_abort' :- querylevel(_), !, % aborted during a running toplevel
    message(error0, '{ Execution aborted }'),
    % Enter toplevel again
    shell_body,
    shell_hook(exit),
    % TODO: This is useful for batched execution, but it may
    %   produce strange behaviour as an interactive toplevel (add
    %   a way to clear the error status?)
    halt(1).
'$shell_abort' :- % see internals:abort_hooks/0
    halt(1).

:- if(defined(optim_comp)).
shell_body :-
    reset_debugger(_), % TODO: needed?
    intercept(error_protect(top_shell_env, fail), % TODO: captures errors in after_query_hook, goal translations, etc. document?
        control_c,
        do_interrupt_command(0'\n)).
:- else.
shell_body :-
    intercept(error_protect(top_shell_env, fail), % TODO: captures errors in after_query_hook, goal translations, etc. document?
        control_c,
        do_interrupt_command(0'\n)).
:- endif.

top_shell_env :-
    reset_query_level,
    catch(shell_env(_Vars), go_top, top_shell_env).

shell_env(Vars) :-
    repeat,
    shell_query(Vars, Query),
    shell_hook(after_query),
    Query == end_of_file,
    !.

% Note: up/0 & top/0 checked explicitly

shell_query(Dict, Query) :-
    % TODO: unsafe? what if some thread is still running?
    '$empty_gcdef_bin', % Really get rid of abolished predicates
    debugger_info,
    with_top_prompt(get_query(user, Query, Dict, VarNames)),
    Query \== up,
    !,
    ( Query == top ->
        throw(go_top)
    ; valid_solution(Query, Dict, VarNames)
    ).
shell_query(_Dict, end_of_file).

valid_solution(Query, Dict, VarNames) :-
    get_shell_module(ShMod),
    query_call(Query, ShMod, VarNames, Result),
    ( Result = yes(MoreSols) ->
        shell_hook(after_solution),
        % display and validate solution
        display_ok_solution(Dict, MoreSols),
        !, % we do not want more solutions
        display_status(yes)
    ; Result = no ->
        % display no solution
        display_status(no)
    ; Result = exception(E) ->
        % display error
        default_error_message(E),
        display_status(aborted)
    ).

display_status(_Status) :- quiet_mode, !.
display_status(Status) :- top_nl, top_display(Status), top_nl.

% ---------------------------------------------------------------------------

% TODO: FIXME: add (optional) tracertc/0 call in rtcheck/6 intercept
% (here or in the rtchecks modules)
% :- use_module(library(debugger/debugger_lib), [tracertc/0]).

% (Result=no when no more solutions)
query_call(RawQuery, ShMod, VarNames, Result) :-
    shell_expand(RawQuery, ShMod, VarNames, Query),
    ( adjust_debugger ; switch_off_debugger, fail ),
    ( catch(query_call_(Query, ShMod, VarNames, Result),
            E, (Result = exception(E)))
    ; Result = no % TODO: move outside adjust debugger, etc.
    ),
    ( switch_off_debugger ; adjust_debugger, fail ),
    ( Result = yes(_) -> true
    ; ! % remove all auxiliary choicepoints
    ).

:- if(defined(optim_comp)).
query_call_(Query, ShMod, _VarNames, yes(MoreSols)) :-
    '$metachoice'(BeforeChoice),
    '$shell_call_in_mod'(ShMod, Query), % TODO: enable/disable trace on toplevel_scope, failure/exception safe? % TODO: missing debug info VarNames
    '$metachoice'(AfterChoice),
    ( BeforeChoice = AfterChoice -> MoreSols = false ; MoreSols = true ).
:- else.
query_call_(Query, ShMod, VarNames, yes(MoreSols)) :-
    '$metachoice'(BeforeChoice),
    '$shell_call_in_mod'(ShMod, srcdbg_spy(Query, _, _, _, _, d(VarNames, []), _)),
    '$metachoice'(AfterChoice),
    ( BeforeChoice = AfterChoice -> MoreSols = false ; MoreSols = true ).
:- endif.

% Displays a solution (if needed) and validate interactively
display_ok_solution(Dict, MoreSols) :-
    dump_solution(Dict, Solution),
    ( solution_prompt_needed(Solution, MoreSols) ->
        validate_solution(Dict, Solution)
    ; true
    ).

solution_prompt_needed([], MoreSols) :- !,
    % no visible bindings or constraints
    current_prolog_flag(prompt_alternatives_no_bindings, on),
    MoreSols = true.
solution_prompt_needed(_, _).

validate_solution(Dict, Solution) :-
    top_nl,
    current_output(CurrOut), set_output(user_output),
    display_solution(Solution),
    set_output(CurrOut),
    top_display(' ? '), top_flush,
    %
    top_get(C), ( C = 0'\n -> true ; top_skip(0'\n) ), % ask the user
    ( C = 0'y -> true % y(es)
    ; C = 0'Y -> true % Y(es)
    ; C = 0'\n -> true % end of line
    ; C = 0', ->
        % add another question
        inc_query_level,
        shell_env(Dict),
        dec_query_level,
        % return from question, ask again
        validate_solution(Dict, Solution)
    ; fail % another solution
    ).

% ---------------------------------------------------------------------------

:- include(library(toplevel/toplevel_prompt)).

% ---------------------------------------------------------------------------

:- if(defined(optim_comp)).
:- use_module(compiler(shell_frontend)).
:- else.
:- use_module(library(compiler/shell_itf)).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "toplevel commands").

% Create executables
:- if(defined(optim_comp)).
% TODO: fixme
:- else.
:- use_module(library(compiler/exemaker), [make_exec/2]).
:- endif.

:- export(use_module/1).
use_module(M) :-
    get_shell_module(ShMod),
    process_decl(use_module(M), ShMod).

:- export(use_module/2).
use_module(M, Imports) :-
    get_shell_module(ShMod),
    process_decl(use_module(M, Imports), ShMod).

:- export(ensure_loaded/1).
ensure_loaded(Files) :-
    get_shell_module(ShMod),
    process_decl(ensure_loaded(Files), ShMod).

:- export('.'/2).
[File|Files] :-
    ensure_loaded([File|Files]).

:- export(consult/1).
consult([]) :- !.
consult([File|Files]) :- !,
    consult(File),
    consult(Files).
consult(File) :-
    set_debug_mode(File),
    get_shell_module(ShMod),
    process_decl(ensure_loaded(File), ShMod).

:- export(compile/1).
compile([]) :- !.
compile([File|Files]) :- !,
    compile(File),
    compile(Files).
compile(File) :-
    set_nodebug_mode(File),
    get_shell_module(ShMod),
    process_decl(ensure_loaded(File), ShMod).

:- export(use_package/1).
use_package(F) :-
    get_shell_module(ShMod),
    process_decl(use_package(F), ShMod).

:- export(make_exec/2).
:- redefining(make_exec/2).
:- if(defined(optim_comp)).
:- use_module(compiler(errlog)).
make_exec(Files, ExecName) :- errlog:bug(fixmefixmefixme__make_exec_disabled(Files, ExecName)). % TODO: fixme, use comp:dynexec/2
:- else.
make_exec(Files, ExecName) :-
    ( Files = [_|_] -> Files2 = Files ; Files2 = [Files] ),
    exemaker:make_exec(Files2, ExecName).
:- endif.

:- export(include/1).
include(F) :-
    get_shell_module(ShMod),
    process_decl(include(F), ShMod).

:- export(new_declaration/2).
new_declaration(S, ITF) :-
    get_shell_module(ShMod),
    process_decl(new_declaration(S, ITF), ShMod).

:- export(new_declaration/1).
new_declaration(S) :-
    get_shell_module(ShMod),
    process_decl(new_declaration(S), ShMod).

:- export(load_compilation_module/1).
load_compilation_module(File) :-
    get_shell_module(ShMod),
    process_decl(load_compilation_module(File), ShMod).

:- export(add_sentence_trans/2).
add_sentence_trans(P, Prior) :-
    get_shell_module(ShMod),
    process_decl(add_sentence_trans(P, Prior), ShMod).

:- export(add_term_trans/2).
add_term_trans(P, Prior) :-
    get_shell_module(ShMod),
    process_decl(add_term_trans(P, Prior), ShMod).

:- export(add_goal_trans/2).
add_goal_trans(P, Prior) :-
    get_shell_module(ShMod),
    process_decl(add_goal_trans(P, Prior), ShMod).

% ---------------------------------------------------------------------------

:- doc(subsection, "Debugger support").

debugger_info :-
    ( get_debugger_state(State),
      arg(1, State, T),
      \+ T = off ->
        top_display('{'), top_display(T), top_display('}\n')
    ; true
    ).

:- include(library(toplevel/toplevel_debugger)).
