:- module(_, [], [assertions, regtypes]).

:- doc(title, "Profiling").

:- doc(author, "The Ciao Development Team").

% NOTE: This module provides a user interface to interact with the
%   profiler and it *should* never be part minimal builds, nor an
%   engine module.

:- doc(stability, devel).

:- doc(module, "@cindex{profiling} @cindex{profiler} @cindex{built-in profiler}

   This module provides a simple Prolog-level interface for the
   built-in profiler. This is a basic, relatively low overhead
   profiler, which can be used to determine call counts, times,
   etc. per predicate when executing a goal or an executable.  There
   exist bundles with other, more complex profilers, but which may
   have higher overhead.

   @section{Enabling profiling}

   The profiling instrumentation is disabled in the default engine to
   avoid overhead when not needed.  Thus, in order to be able to use
   the profiler the engine must be recompiled with the
   @tt{debug_level} option set to @tt{profile}.

   For example, after having built and installed Ciao normally (which
   builds the standard engine) you can do:

@begin{verbatim}
./ciao-boot.sh configure --set-flag core:debug_level=profile
./ciao-boot.sh build core.engine
./ciao-boot.sh configure --set-flag core:debug_level=nodebug 
@end{verbatim}

   This will build the engine with profiling and leave the
   configuration back to normal, now with both the standard and the
   profiling engines compiled.

@comment{
%%%%%%%%%%%%%%% BEGIN COMNENTED OUT %%%%%%%%%%%%%%%%%%%%%%%%
@begin{verbatim}
./ciao-boot.sh configure --interactive
@end{verbatim}

   And choose the following flags: 

@begin{verbatim}
...
Please select the configuration mode:

    basic    --  Configure just a minimum set of options.
    advanced --  Configure an extended set of options.

configuration_mode=[advanced] (default: basic) ? advanced
@end{verbatim}

@begin{verbatim}
...
Level of debugging built into the engine (for developers):

   nodebug         -- Do not include debug information or messages
   debug           -- Emulator with C level debugging info available
                      plus extended C compilation warnings
   profile         -- Include profiling options for the emulator
   profile-debug   -- Include profiling and debug options for the
                      emulator
   paranoid-debug  -- Emulator with C level debugging info available
                      plus paranoid C compilation warnings.

debug_level=[nodebug] ? 
@end{verbatim}

  A more direct alternative is to run the
  @tt{builder/etc/build-eng-debug.sh} script which will build build
  several engine versions with different @tt{debug_level} options. The
  different engine versions can co-exist and are selected as shown
  below.
%%%%%%%%%%%%%%% END COMMENTED OUT %%%%%%%%%%%%%%%%%%%%%%%%
}


  @section{Profiling calls or executions}

  Once the standard and profiling engines are compiled you can select
  between these engine versions by setting the @tt{CIAODBG}
  environment variable before starting the top level or compiled
  application.  E.g., for bash-style shells: 

@begin{verbatim}
$ CIAODBG=profile ciaosh
Ciao 1.23-v1.21-1252-ga1a95f7310 (2024-07-21 11:08:37 +0200) [DARWINx86_64] [profile]
?-
@end{verbatim}

   You can then load this module and issue the commands it provides:
@begin{verbatim}
?- use_module(library(profile)).
yes
?- use_module('core/examples/general/nqueens').

yes
?- profile(queens(15,Q),[calls,roughtime]).

Q = [8,5,9,1,10,7,3,12,2,4,6,14,11,13,15] ? 

yes
?- print_profile.
{profile: dump saved in /tmp/ciao__profile.txt}

Profile information:
25 predicates called, 55596 calls made, 0.02 secs. accumulated time
Calls 		 Time (rough) 		 Type    Spec
===== 		 ============ 		 ====    ====
38644 (69.51%) 	 0.015135 (70.18%) 	 Emul    nqueens:no_attack_2/3
8442 (15.18%) 	 0.003347 (15.52%) 	 Emul    nqueens:sel/3
7098 (12.77%) 	 0.002481 (11.50%) 	 Emul    nqueens:no_attack/2
1360 (2.45%) 	 0.000547 (2.54%) 	 Emul    nqueens:queens_2/3
16 (0.03%) 	 0.000014 (0.06%) 	 Emul    nqueens:queens_list/2
4 (0.01%) 	 0.000007 (0.03%) 	 Built   hiord_rt:call/1
3 (0.01%) 	 0.000004 (0.02%) 	 Emul    internals:rt_module_exp/6/0$$33/4
3 (0.01%) 	 0.000003 (0.01%) 	 C       internals:$global_vars_get_root/1
...
1 (0.00%) 	 0.000001 (0.00%) 	 Emul    nqueens:queens/2
@end{verbatim}

  You can also profile a whole compiled program. You can use the
  @tt{CIAORTOPTS} environment variable to pass engine options such as
  @tt{--profile-calls} and @tt{--profile-roughtime}. This example
  executes @apl{ciaopp} performing analysis on file @tt{guardians.pl}
  with call and time profiling, printing the profiling results at the
  end:

@begin{verbatim}
$ cd core/examples/general
$ CIAODBG=profile CIAORTOPTS=\"--profile-calls --profile-roughtime\" ciaopp -A guardians.pl
@end{verbatim}

").

:- use_module(engine(internals), [
    '$profile_flags_set'/1,
    '$profile_flags_get'/1,
    '$profile_dump'/0,
    '$profile_reset'/0
]).
:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).

% ---------------------------------------------------------------------------

:- export(profile_opt/1).
:- doc(profile_opt/1,"@var{X} is a profiler option:
@begin{itemize}
@item @tt{calls}: count number of calls per predicate
@item @tt{roughtime}: rough approximation of execution time (since the
  predicate is called until the next one is called)
@end{itemize}
").
:- regtype profile_opt(X) 
   # "@var{X} is a profiler option.". 

profile_opt(calls).
profile_opt(roughtime).

% (see eng_profile.h)
get_profile_opt(calls, 1).
get_profile_opt(roughtime, 2).

% TODO: share code like this with other preds!
get_profile_opts(Opts, Flags) :-
    get_profile_opts_(Opts, 0, Flags).

get_profile_opts_([], F, F).
get_profile_opts_([Opt|Opts], F0, F) :-
    ( var(Opt) -> throw(error(instantiation_error, profile/2))
    ; get_profile_opt(Opt, OptF) -> F1 is F0 \/ OptF
    ; throw(error(domain_error(profile_opt, Opt), profile/2))
    ),
    get_profile_opts_(Opts, F1, F).

% ---------------------------------------------------------------------------

:- export(profile/2).
:- meta_predicate profile(goal, ?).
:- pred profile(G, Opts) : (callable(G), list(profile_opt, Opts))
   # "Profile execution of @var{G} (as with @pred{once/1})".

profile(G, Opts) :-
    get_profile_opts(Opts, Flags),
    '$profile_flags_get'(OldFlags), % TODO: we should not profile with profiler enabled
    '$profile_reset',
    '$profile_flags_set'(Flags),
    once_port_reify(G, Port),
    '$profile_flags_set'(OldFlags),
    port_call(Port).

:- export(profile/1).
:- meta_predicate profile(goal).
:- pred profile(G) 
   # "Profile execution of @var{G} (as with @pred{once/1}) with
   default options (@tt{calls})".

profile(G) :- profile(G, [calls]).

% ---------------------------------------------------------------------------

:- use_module(library(stream_utils), [file_to_string/2, write_string/1]).

% TODO: improve this interface
:- export(print_profile/0).
:- pred print_profile 
   # "Outputs an ascii printout of the profiling results through
     @tt{standard_user}, and also leaves the result in a file in a
     temporary directory (reporting the name and location of the dump
     file).".
print_profile :-
    '$profile_dump',
    % TODO: hardwired
    file_to_string('/tmp/ciao__profile.txt', Str),
    write_string(Str).
