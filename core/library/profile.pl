:- module(_, [], [assertions]).

:- doc(title, "Profiling").

:- doc(author, "The Ciao Development Team").

% NOTE: This module provides a user interface to interact with the
%   profiler and it *should* never be part minimal builds, nor an
%   engine moudule.

:- doc(module, "This module provides a Prolog interface for the
   builtin profiler. See @tt{engine/README.md} for details.").

:- use_module(engine(internals), [
    '$profile_set'/1,
    '$profile_get'/1,
    '$profile_dump'/0,
    '$profile_reset'/0
]).
:- use_module(library(port_reify), [once_port_reify/2, port_call/1]).

:- export(profile/1).
:- meta_predicate profile(goal).
:- pred profile(G) # "Profile execution of @var{G} (as with @pred{once/1})".

profile(G) :-
    '$profile_get'(OldProf), % TODO: we should not profile with profiler enabled
    '$profile_reset',
    '$profile_set'(1),
    once_port_reify(G, Port),
    '$profile_set'(OldProf),
    port_call(Port).

:- use_module(library(stream_utils), [file_to_string/2, write_string/1]).

% TODO: improve this interface
:- export(print_profile/0).
print_profile :-
    '$profile_dump',
    % TODO: hardwired
    file_to_string('/tmp/ciao__profile.txt', Str),
    write_string(Str).
