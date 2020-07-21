:- module(_, [], [fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Analyze with CiaoPP").
:- doc(author, "Jose F. Morales (this wrapper module)").
:- doc(author, "Isabel Garcia").

:- doc(module, "This is a wrapper around @apl{ciaopp} to analyze
   bundles. It uses @tt{ciaopp} as an external process so that no hard
   dependencies are introduced.").

% ---------------------------------------------------------------------------

:- use_module(ciaobld(config_common), [cmd_path/4]).
:- use_module(ciaobld(cpx_process), [cpx_process_call/3]).

ciaopp_exec := ~cmd_path(ciaopp, plexe, 'ciaopp').

ciaopp_batch_exec := ~cmd_path(ciaopp, plexe, 'ciaopp-batch').

ciaopp_dump_exec := ~cmd_path(ciaopp, plexe, 'ciaopp-dump').

:- export(invoke_ciaopp/1).
invoke_ciaopp(Args) :-
    cpx_process_call(~ciaopp_exec, Args, []).

:- export(invoke_ciaopp_batch/1).
invoke_ciaopp_batch(Args) :-
    cpx_process_call(~ciaopp_batch_exec, Args, []).

:- export(invoke_ciaopp_dump/1).
invoke_ciaopp_dump(Args) :-
    cpx_process_call(~ciaopp_dump_exec, Args, []).

