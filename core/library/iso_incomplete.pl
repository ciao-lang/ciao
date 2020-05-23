:- module(iso_incomplete, [
    absolute_file_name/2,
    open/4,
    close/1,
    close/2,
    close_options/1,
    close_option/1,
    stream_property/2,
    stream_prop/1
], [assertions,isomodes,datafacts]).

:- doc(title, "ISO Prolog compatibility layer").

:- doc(author, "The Ciao Development Team").

:- doc(module, "This module provides some additional ISO Prolog
   predicates whose implementation is not yet complete.").

:- doc(bug, "Introduce iso_incomplete/1 property? Or wait until we can merge them?"). 

:- doc(bug, "at_end_of_stream :- not_yet_implemented.").
:- doc(bug, "at_end_of_stream(_) :- not_yet_implemented.").

:- doc(bug, "set_stream_position(_,_) :- not_yet_implemented.").
 
:- doc(bug, "char_conversion(_,_) :- not_yet_implemented.").
:- doc(bug, "current_char_conversion(_,_) :- not_yet_implemented.").

:- use_module(engine(stream_basic)).

%:- use_module(library(streams)).% TODO:debug

% ---------------------------------------------------------------------------

absolute_file_name(Path, ExpandedPath) :-
    fixed_absolute_file_name(Path, '.', ExpandedPath).

% ---------------------------------------------------------------------------

% Stream aliases (one stream per alias)
:- data '$alias_stream'/2.
% Inverse index (from stream to aliases)
:- data '$stream_alias'/2.

% TODO: argument order changed!
:- pred open(+sourcename, +io_mode, ?stream, +open_option_list).

open(File, Mode, Stream, Opts) :-
    get_aliases(Opts, OtherOpts, Aliases),
    stream_basic:open(File, Mode, Stream, OtherOpts),
    set_aliases(Aliases, Stream).

% Get (and check) aliases options
get_aliases([], [], []).
get_aliases([Opt|Opts], OtherOpts, Aliases) :-
    ( Opt = alias(Alias) ->
        check_alias(Alias),
        OtherOpts = OtherOpts0,
        Aliases = [Alias|Aliases0]
    ; OtherOpts = [Opt|OtherOpts0],
      Aliases = Aliases0
    ),
    get_aliases(Opts, OtherOpts0, Aliases0).

% Alias must be an atom and it cannot be reused
check_alias(Alias) :-
    ( atom(Alias) -> true
    ; throw(error(type_error(atom, Alias), 'stream_basic:$open'/4-4))
    ),
    ( '$alias_stream'(Alias, _) ->
        throw(error(permission_error(open, source_sink, alias(Alias)),
                    'stream_basic:$open'/4-4))
    ; true
    ).

set_aliases([], _).
set_aliases([Alias|Aliases], Stream) :-
    set_alias(Alias, Stream),
    set_aliases(Aliases, Stream).

set_alias(Alias, Stream) :-
    % Remove alias 
    ( retract_fact('$alias_stream'(Alias,S)),
      retract_fact('$stream_alias'(S,Alias)),
        fail
    ; true
    ),
    % Assert (two indices)
    asserta_fact('$alias_stream'(Alias, Stream)),
    asserta_fact('$stream_alias'(Stream, Alias)).

% Remove aliases associated to Stream
remove_aliases(Stream) :-
    ( retract_fact('$stream_alias'(Stream, Alias)),
      retract_fact('$alias_stream'(Alias, _)),
        fail
    ; true
    ).

% alias_stream(?Alias,?Stream): Stream is the stream associated to Alias
alias_stream(Alias, Stream) :- nonvar(Alias), !,
    '$alias_stream'(Alias, Stream).
alias_stream(Alias, Stream) :- % (nondet)
    '$stream_alias'(Stream, Alias).

resolve_stream_alias(Alias, Stream) :- atom(Alias),
    '$alias_stream'(Alias, Stream0), !, Stream = Stream0.
resolve_stream_alias(Stream, Stream).

:- pred close(@stream,@close_options). 

:- doc(bug, "close/2 not complete w.r.t. iso standard.").

close(S, _) :-
    resolve_stream_alias(S, S2),
    ( nonvar(S2) -> remove_aliases(S2) ; true ),
    stream_basic:close(S2).

:- prop close_options(L) + regtype
   # "@var{L} is a list of @prop{close_option/1}.".

close_options([]).
close_options([O|Os]):-
    close_option(O),
    close_options(Os).

:- prop close_option(O) + regtype 
   # "@var{O} is an option for close/2: @includedef{close_option/1}.".

close_option(force(true)).
close_option(force(false)).

:- pred close(@stream). 
close(S) :- close(S, []).

:- pred stream_property(?stream, ?stream_prop).

:- doc(bug, "stream_property/2 not complete w.r.t. iso standard.").

stream_property(S, P) :-
    current_stream(File, Mode, S),
    ( P = file_name(File)
    ; P = mode(Mode)
    ; P = alias(Alias), alias_stream(Alias, S)
    ; % (last case)
      ( Mode = read -> P = input ; P = output )
    ).

:- prop stream_prop(P) + regtype
   # "@var{P} is a valid stream property: @includedef{stream_prop/1}".  

stream_prop(input).
stream_prop(output).
stream_prop(file_name(File)) :- atm(File).
stream_prop(mode(Mode)) :- atm(Mode).
stream_prop(alias(Alias)) :- atm(Alias).

