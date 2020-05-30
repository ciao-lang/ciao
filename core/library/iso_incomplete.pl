:- module(iso_incomplete, [
    absolute_file_name/2,
    open/4,
    close/1,
    close/2,
    close_options/1,
    close_option/1,
    stream_property/2,
    stream_prop/1,
    %
    set_input/1, 
    set_output/1, 
    %
    get_code/2, 
    peek_code/2, 
    put_code/2, 
    nl/1, 
    tab/2, 
    get_byte/2, 
    put_byte/2, 
    display/2, 
    displayq/2, 
    %
    get_char/2, 
    peek_char/2, 
    put_char/2, 
    %
    read/2, 
    read_term/3,
    %
    write_term/3,
    write/2, 
    writeq/2, 
    write_canonical/2, 
    print/2, 
    printq/2, 
    portray_clause/2
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

:- use_module(engine(io_basic)).
:- use_module(engine(stream_basic)).
:- use_module(library(iso_char)).
:- use_module(library(read)).
:- use_module(library(write)).

% ---------------------------------------------------------------------------

absolute_file_name(Path, ExpandedPath) :-
    fixed_absolute_file_name(Path, '.', ExpandedPath).

% ---------------------------------------------------------------------------
% (Extended stream properties)

% Stream aliases (one stream per alias)
:- data '$alias_stream'/2.
% Inverse index (from stream to aliases)
:- data '$stream_alias'/2.

% Stream type
:- data '$stream_type'/2.

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

set_stream_type(Stream, Type) :-
    ( Type = text -> true ; set_fact('$stream_type'(Stream, Type)) ).

get_stream_type(Stream, Type) :-
    ( '$stream_type'(Stream, Type0) -> true ; Type0 = text ),
    Type = Type0.

% ---------------------------------------------------------------------------

% TODO: argument order changed!
:- pred open(+sourcename, +io_mode, ?stream, +open_option_list).

open(File, Mode, Stream, Opts) :-
    get_open_opts(Opts, OtherOpts, text, Type, Aliases),
    stream_basic:open(File, Mode, Stream, OtherOpts),
    set_stream_type(Stream, Type),
    set_aliases(Aliases, Stream).

% Get (and check) aliases options
get_open_opts([], [], Type, Type, []).
get_open_opts([Opt|Opts], OtherOpts, Type0, Type, Aliases) :-
    ( Opt = alias(Alias) ->
        check_alias(Alias),
        OtherOpts = OtherOpts0,
        Type1 = Type0,
        Aliases = [Alias|Aliases0]
    ; Opt = type(Type1) ->
        check_stream_type(Type1),
        OtherOpts = OtherOpts0,
        Aliases = Aliases0
    ; OtherOpts = [Opt|OtherOpts0],
      Type1 = Type0,
      Aliases = Aliases0
    ),
    get_open_opts(Opts, OtherOpts0, Type1, Type, Aliases0).

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

check_stream_type(Type) :-
    ( var(Type) ->
        throw(error(instantiation_error, 'stream_basic:$open'/4-4))
    ; Type = binary -> true
    ; Type = text -> true
    ; throw(error(domain_error(stream_option, type(Type)), 'stream_basic:$open'/4-4))
    ).

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

:- pred stream_property(?stream, ?stream_prop).

:- doc(bug, "stream_property/2 not complete w.r.t. iso standard.").

stream_property(S, P) :-
    current_stream(File, Mode, S),
    ( P = file_name(File)
    ; P = mode(Mode)
    ; P = alias(Alias), alias_stream(Alias, S)
    ; P = type(Type), get_stream_type(S, Type)
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

% ---------------------------------------------------------------------------

set_input(S) :- resolve_stream_alias(S, S2), stream_basic:set_input(S2).
set_output(S) :- resolve_stream_alias(S, S2), stream_basic:set_output(S2).

get_code(S,A) :- resolve_stream_alias(S, S2), io_basic:get_code(S2,A).
peek_code(S,A) :- resolve_stream_alias(S, S2), io_basic:peek_code(S2,A).
put_code(S,A) :- resolve_stream_alias(S, S2), io_basic:put_code(S2,A).
nl(S) :- resolve_stream_alias(S, S2), io_basic:nl(S2).
tab(S,A) :- resolve_stream_alias(S, S2), io_basic:tab(S2,A).
get_byte(S,A) :- resolve_stream_alias(S, S2), io_basic:get_byte(S2,A).
put_byte(S,A) :- resolve_stream_alias(S, S2), io_basic:put_byte(S2,A).
display(S,A) :- resolve_stream_alias(S, S2), io_basic:display(S2,A).
displayq(S,A) :- resolve_stream_alias(S, S2), io_basic:displayq(S2,A).

get_char(S,A) :- resolve_stream_alias(S, S2), iso_char:get_char(S2,A).
peek_char(S,A) :- resolve_stream_alias(S, S2), iso_char:peek_char(S2,A).
put_char(S,A) :- resolve_stream_alias(S, S2), iso_char:put_char(S2,A).

read(S,A) :- resolve_stream_alias(S, S2), read:read(S2,A).
read_term(S,A,B) :- resolve_stream_alias(S, S2), read:read_term(S2,A,B).

write_term(S,A,B) :- resolve_stream_alias(S, S2), write:write_term(S2,A,B).
write(S,A) :- resolve_stream_alias(S, S2), write:write(S2,A).
writeq(S,A) :- resolve_stream_alias(S, S2), write:writeq(S2,A).
write_canonical(S,A) :- resolve_stream_alias(S, S2), write:write_canonical(S2,A).
print(S,A) :- resolve_stream_alias(S, S2), write:print(S2,A).
printq(S,A) :- resolve_stream_alias(S, S2), write:printq(S2,A).
portray_clause(S,A) :- resolve_stream_alias(S, S2), write:portray_clause(S2,A).
