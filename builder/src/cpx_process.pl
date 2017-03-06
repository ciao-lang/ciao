:- module(_, [], [assertions, regtypes, isomodes, hiord, fsyntax]).

:- doc(title, "Run Ciao executables as child processes").

:- doc(author, "Jose F. Morales").

:- doc(module, "This module allows the execution of Ciao compiled
   programs (@tt{.cpx}) as child processes (see @lib{process}).").

:- use_module(library(lists), [append/3, select/3]).
:- use_module(library(process)).
:- use_module(library(system), [using_windows/0]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(engine(internals), [ciao_root/1]).

:- export(cpx_process_option/1).
:- regtype cpx_process_option/1.
cpx_process_option(boot). % Use the boot engine
cpx_process_option(X) :- process_option(X).

:- export(cpx_process_call/3).
:- pred cpx_process_call(Cmd, Args, Opts) :
	( atm(Cmd), list(Args, process_arg), list(Opts, cpx_process_option) )
   # "Execute a Ciao binary @var{Cmd} in a child process. It setups
      the environment to use the current local engine. If @tt{boot} is
      added to @var{Opts} then it uses the bootstrap engine.".

cpx_process_call(Cmd, Args, Opts) :-
	( select(boot, Opts, Opts1) ->
	    Env = ~bootciao_env
	; Opts1 = Opts,
	  Env = ~localciao_env
	),
	cpx_process_call_(Cmd, Args, ~merge_env(Env, Opts1)).

% ('CIAOENGINE' env var must be specified in a env(_) option in Opts)
cpx_process_call_(CiaoExec, Args, Opts) :-
	using_windows, !,
        % '#!/...' is not supported in non-POSIX systems
	( member(env(Env), Opts),
          member('CIAOENGINE'=CiaoEngine, Env) -> true
        ; throw(error(unknown_ciaoengine, cpx_process_call_/3))
        ),
        append(Args, ['-C', '-b', CiaoExec], Args2),
        process_call(CiaoEngine, Args2, Opts).
cpx_process_call_(CiaoExec, Args, Opts) :-
	process_call(CiaoExec, Args, Opts).

merge_env(Env, Opts, Opts2) :-
	( select(env(Env0), Opts, Opts1) ->
	    Env2 = ~append(Env, Env0)
	; Env2 = Env, Opts1 = Opts
	),
	Opts2 = [env(Env)|Opts1].

% ---------------------------------------------------------------------------
% Default environments for selecting the local or boot engines

:- use_module(ciaobld(eng_defs), [eng_path/3]).
:- use_module(ciaobld(config_common), [default_eng_def/1, boot_eng_def/1]).

bootciao_env := Env :-
	Eng = ~boot_eng_def,
	% TODO: (un)define CIAOPATH? 
	Env = ['CIAOALIASPATH' = '',
	       'CIAOROOT' = ~ciao_root,
	       'CIAOHDIR' = ~eng_path(hdir, Eng),
	       'CIAOENGINE' = ~eng_path(exec, Eng)].

localciao_env := Env :-
	Eng = ~default_eng_def,
	% TODO: (un)define CIAOPATH? 
	Env = ['CIAOALIASPATH' = '',
	       'CIAOROOT' = ~ciao_root,
	       'CIAOHDIR' = ~eng_path(hdir, Eng),
	       'CIAOENGINE' = ~eng_path(exec, Eng)].

