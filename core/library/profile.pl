:- module(_, [], [assertions, regtypes]).

:- doc(title, "Profiling").

:- doc(author, "The Ciao Development Team").

% NOTE: This module provides a user interface to interact with the
%   profiler and it *should* never be part minimal builds, nor an
%   engine moudule.

:- doc(stability, devel).

:- doc(module, "This module provides a Prolog interface for the
   builtin profiler. See @tt{engine/README.md} for details.").

:- use_module(engine(internals), [
    '$profile_flags_set'/1,
    '$profile_flags_get'/1,
    '$profile_dump'/0,
    '$profile_reset'/0
]).
:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).

% ---------------------------------------------------------------------------

:- export(profile_opt/1).
:- regtype profile_opt(X) 
   # "@var{X} is the a profiler option:
@begin{itemize}
@item @tt{calls}: count number of calls per predicate
@item @tt{roughtime}: rough approximation of execution time (since the
  predicate is called until the next one is called)
@end{itemize}
".

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
print_profile :-
    '$profile_dump',
    % TODO: hardwired
    file_to_string('/tmp/ciao__profile.txt', Str),
    write_string(Str).
