:- module(iso_incomplete, [], [assertions,isomodes,datafacts,hiord]).

:- doc(title, "ISO Prolog compatibility layer").

:- doc(author, "The Ciao Development Team").

:- doc(module, "This module provides some additional ISO Prolog
   predicates whose implementation is not yet complete.").

:- doc(bug, "Introduce iso_incomplete/1 property? Or wait until we can merge them?"). 

:- doc(bug, "set_stream_position(_,_) :- not_yet_implemented.").
 
:- doc(bug, "char_conversion(_,_) :- not_yet_implemented.").
:- doc(bug, "current_char_conversion(_,_) :- not_yet_implemented.").

:- use_module(engine(io_basic)).
:- use_module(engine(stream_basic)).
:- use_module(library(io_alias_redirection), [get_stream/2]).
:- use_module(library(iso_char)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(operators)).

% ---------------------------------------------------------------------------

:- export(absolute_file_name/2).
absolute_file_name(Path, ExpandedPath) :-
    fixed_absolute_file_name(Path, '.', ExpandedPath).

% ---------------------------------------------------------------------------
% (Extended stream properties)

% Stream aliases (one stream per alias)
:- data '$alias_stream'/2.
% Inverse index (from stream to aliases)
:- data '$stream_alias'/2.

:- data '$stream_type'/2. % Stream type
:- data '$stream_eof_action'/2. % Stream eof_action
:- data '$stream_reposition'/2. % Stream reposition

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

set_stream_type(Stream, Type) :-
    ( Type = text -> true ; set_fact('$stream_type'(Stream, Type)) ).

get_stream_type(Stream, Type) :-
    ( '$stream_type'(Stream, Type0) -> true ; Type0 = text ),
    Type = Type0.

get_stream_eof_action(Stream, Action) :-
    ( '$stream_eof_action'(Stream, Action0) -> true ; Action0 = eof_code ),
    Action = Action0.

set_eof_action(Stream,EofAction) :-
    set_fact('$stream_eof_action'(Stream, EofAction)) .

set_reposition(Stream, Reposition) :-
    set_fact('$stream_reposition'(Stream, Reposition)).

remove_stream_data(S) :-
    remove_aliases(S),
    retractall_fact('$stream_type'(S,_)),
    retractall_fact('$stream_eof_action'(S,_)),
    retractall_fact('$stream_reposition'(S,_)).

% ---------------------------------------------------------------------------

:- export(open/3).
:- pred open(+sourcename, +io_mode, ?stream).

open(File, Mode, Stream) :-
    chk_domain_nonvar(io_mode, Mode, open/3),
    stream_basic:open(File, Mode, Stream).

% ---------------------------------------------------------------------------
% TODO: argument order changed!

:- export(open/4).
:- pred open(+sourcename, +io_mode, ?stream, +open_option_list).

open(File, Mode, Stream, Opts) :-
    chk_nonvar(File, open/4),
    chk_domain_nonvar(io_mode, Mode, open/4),
    get_open_opts(Opts, OtherOpts, text, Type, Aliases, error, EofAction, true, Reposition),
    stream_basic:open(File, Mode, Stream, OtherOpts),
    set_stream_type(Stream, Type),
    set_aliases(Aliases, Stream),
    set_eof_action(Stream, EofAction),
    set_reposition(Stream, Reposition).

% TODO:[JF] what should we do with repeated options?

% Get (and check) open options
get_open_opts(Opts, _, _, _, _, _, _, _, _) :- var(Opts), !, throw(error(instantiation_error, open/4)).
get_open_opts([], [], Type, Type, [], EofAction, EofAction, Reposition, Reposition) :- !.
get_open_opts(Opts0, OtherOpts, Type0, Type, Aliases, EofAction0, EofAction, Reposition0, Reposition) :-
    ( Opts0 = [Opt|Opts] -> true
    ; throw(error(type_error(list, Opts0), open/4))
    ),
    chk_nonvar(Opt, open/4),
    ( Opt = alias(Alias) ->
        check_alias(Alias),
        OtherOpts = OtherOpts0,
        Type1 = Type0,
        Aliases = [Alias|Aliases0],
        EofAction1 = EofAction0,
        Reposition1 = Reposition0
    ; Opt = type(Type1), nonvar(Type1), stream_type_t(Type1) ->
        OtherOpts = OtherOpts0,
        Aliases = Aliases0,
        EofAction1 = EofAction0,
        Reposition1 = Reposition0
    ; Opt = eof_action(EofAction1), nonvar(EofAction1), eof_action_t(EofAction1) ->
        OtherOpts = OtherOpts0,
        Type1 = Type0,
        Aliases = Aliases0,
        Reposition1 = Reposition0
    ; Opt = reposition(Reposition1) ->
        OtherOpts = OtherOpts0,
        Type1 = Type0,
        Aliases = Aliases0,
        EofAction1 = EofAction0
    ; ( open_option(Opt) -> true % other Ciao options % TODO: fix
      ; throw(error(domain_error(stream_option, Opt), open/4))
      ),
      OtherOpts = [Opt|OtherOpts0],
      Type1 = Type0,
      Aliases = Aliases0,
      EofAction1 = EofAction0,
      Reposition1 = Reposition0
    ),
    get_open_opts(Opts, OtherOpts0, Type1, Type, Aliases0, EofAction1, EofAction, Reposition1, Reposition).

:- import(stream_basic, [open_option/1]).

% Alias must be an atom and it cannot be reused
check_alias(Alias) :-
    chk_domain(atom, Alias, 'stream_basic:$open'/4-4),
    ( '$alias_stream'(Alias, _) ->
        % reusing alias not allowed
        throw(error(permission_error(open, source_sink, alias(Alias)), 'stream_basic:$open'/4-4))
    ; true
    ).

stream_type_t(binary).
stream_type_t(text).

eof_action_t(eof_code).
eof_action_t(reset).
eof_action_t(error).

% ---------------------------------------------------------------------------

:- prop stream_or_alias/1 + regtype.
stream_or_alias(X) :- atom(X). % TODO: complete
stream_or_alias(X) :- stream(X). % TODO: complete

:- export(close/1).
:- pred close(@stream_or_alias). 
close(S) :-
    chk_nonvar(S, close/1),
    iso_incomplete:close(S, []).

:- export(close/2).
:- pred close(@stream_or_alias,@close_options). 
close(S, Opts) :-
    get_close_opts(Opts, false, Force),
    ( Force == false -> true
    ; throw(bug(force_not_implemented)) % TODO: Force option is ignored! (JF)
    ),
    chk_resolve_stream_alias(S, S2, close/2),
    remove_stream_data(S2),
    stream_basic:close(S2).
    
:- export(close_options/1).
:- prop close_options(L) + regtype
   # "@var{L} is a list of @prop{close_option/1}.".

close_options([]).
close_options([O|Os]):-
    close_option(O),
    close_options(Os).

:- export(close_option/1).
:- prop close_option(O) + regtype 
   # "@var{O} is an option for close/2: @includedef{close_option/1}.".

close_option(force(true)).
close_option(force(false)).

% Get (and check) close options
get_close_opts(Opts, _, _) :- var(Opts), !, throw(error(instantiation_error, close/2)).
get_close_opts([], Force0, Force) :- !, Force=Force0.
get_close_opts(Opts0, _Force0, Force) :-
    ( Opts0 = [Opt|Opts] -> true
    ; throw(error(type_error(list, Opts0), close/2))
    ),
    chk_nonvar(Opt, open/4),
    ( Opt = force(Force1), atom(Force1), ( Force1 = true ; Force1 = false ) ->
        true
    ; throw(error(domain_error(close_option, Opt), close/2))
    ),
    get_close_opts(Opts, Force1, Force).

% ---------------------------------------------------------------------------

:- export(stream_property/2).
:- pred stream_property(?stream, ?stream_property).

:- doc(bug, "stream_property/2 not complete w.r.t. iso standard.").

stream_property(S, P) :- 
    nonvar(S),
    ( S=user_output,S1=S
    ; S=user_input,S1=S
    ; S=user_error,S1=S
    ; get_stream(user_output, S),S1=user_output
    ; get_stream(user_input, S),S1=user_input
    ; get_stream(user_error, S),S1=user_error
    ), !,
    usr_stream_property(S1, P).
stream_property(S, P) :- 
    var(S),
    ( get_stream(user_output, S)
    ; get_stream(user_input, S)
    ; get_stream(user_error, S)
    ),
    stream_property(S, P).
stream_property(S, P) :-
    chk_domain_if_nonvar(stream, S, stream_property/2),
    chk_domain_if_nonvar(stream_property, P, stream_property/2),
    current_stream(File, Mode, S), % TODO: segfault with get_stream(user_output, S), current_stream(A,B,S).
    ( P = file_name(File)
    ; P = mode(Mode)
    ; P = alias(Alias), alias_stream(Alias, S)
    ; P = type(Type), get_stream_type(S, Type)
    ; P = end_of_stream(EOS), stream_state(S, EOS)
    ; P = input, Mode = read
    ; P = output, \+ Mode = read
    ).

% TODO: special cases, fix
usr_stream_property(user_output, P) :- 
    % TODO: fix get_stream/2 segfault, make this unnecessary
    ( P = output
    ; P = alias(user_output)
    ; P = eof_action(reset)
    ; P = mode(append)
    ; P = type(text)
    ; P = reposition(false)
    ).
usr_stream_property(user_input, P) :- 
    ( P = input
    ; P = alias(user_input)
    ; P = eof_action(reset)
    ; P = mode(read)
    ; P = type(text)
    ; P = reposition(false)
    ).
usr_stream_property(user_error, P) :- 
    ( P = error
    ; P = alias(user_error)
    ; P = eof_action(reset)
    ; P = mode(append)
    ; P = type(text)
    ; P = reposition(false)
    ).

% TODO: check (JF)
stream_state(Stream, EOS) :-
    % TODO: if it is an output stream EOS = not    
    ( io_basic:at_end_of_stream(Stream) ->
      catch(io_basic:peek_code(Stream, Code),
            error(permission_error(access, past_end_of_stream, _), _),
            true), 
      ( Code == -1 ->
          EOS = at  
      ; EOS = past  
      )
    ; EOS = not
    ).

:- export(stream_property/1).
:- prop stream_property(P) + regtype
   # "@var{P} is a valid stream property: @includedef{stream_property/1}".  

stream_property(input).
stream_property(output).
stream_property(file_name(File)) :- atm(File).
stream_property(mode(Mode)) :- atm(Mode).
stream_property(type(Type)) :- atm(Type).
stream_property(alias(Alias)) :- atm(Alias).
stream_property(end_of_stream(EOS)) :- end_of_stream_t(EOS).

is_stream_property(input).
is_stream_property(output).
is_stream_property(file_name(_)).
is_stream_property(mode(_)).
is_stream_property(type(_)).
is_stream_property(alias(_)).
is_stream_property(end_of_stream(_)).

:- prop end_of_stream_t(Type) + regtype
   # "@var{Type} depends on the current position of the stream in the file:
      @begin{itemize}
      @item @tt{not}: not all the characters have been read
      @item @tt{at}: located just before the @tt{end_of_file} and
      @item @tt{past}: all the characters have already been read and @tt{end_of_file}
      @end{itemize}".

end_of_stream_t(at).
end_of_stream_t(past).
end_of_stream_t(not).

%----------------------------------------------------------------------------

% TODO: implement. This only covers some exception handling.
%
%   use a '$pos' term and encode all relevant info (see stream_basic.c)
%     last_nl_pos
%     nl_count
%     rune_count
%     and use fseek

:- export(set_stream_position/2).
set_stream_position(Stream, Pos) :-
    chk_resolve_stream_alias(Stream, _S2, set_stream_position/2),
    chk_domain_nonvar(stream_position, Pos, set_stream_position/2),
    throw(not_implemented).
%    ( iso_incomplete:current_input(Stream), var(Pos) ->
%        throw(error(instantiation_error, not_implemented)) % TODO: check (JF)
%    ; ( integer(Pos) ->
%          iso_incomplete:close(Stream)
%      ; throw(error(domain_error(stream_position, Pos), not_implemented)) % TODO: check (JF)
%      )
%    ).

:- export(set_input/1). 
set_input(S) :-
    chk_resolve_stream_alias(S, S2, set_input/1),
    fix_err(stream_basic:set_input(S2), set_input_err).

set_input_err(permission_error(access, stream, S), permission_error(input, stream, S)).

:- export(set_output/1).
set_output(S) :-
    chk_resolve_stream_alias(S, S2, set_output/1),
    fix_err(stream_basic:set_output(S2), set_output_err).

set_output_err(permission_error(modify, stream, S), permission_error(output, stream, S)).

:- export(flush_output/1).
flush_output(S) :-
    chk_resolve_stream_alias(S, S2, flush_output/1),
    fix_err(stream_basic:flush_output(S2), flush_output_err).

flush_output_err(permission_error(modify, stream, S), permission_error(output, stream, S)).
% TODO: check, must be raised when stream is an input stream (JF)
flush_output_err(domain_error(stream_or_alias, S), permission_error(output, stream, S)).

:- export(current_input/1).
current_input(S) :-
    chk_domain_if_nonvar(stream, S, current_input/1),
    stream_basic:current_input(S0),
    ensure_no_alias(S0, S1), % TODO: it should return a stream, not an alias
    unif_stream(S, S1).

% TODO:[JF] resolve S if output, so that we accept stream_alias on results
unif_stream(S, S1) :-
    ( atom(S), get_stream(S, Sa) -> Sa=S1
    ; S=S1
    ).

:- export(current_output/1).
current_output(S) :-
    chk_domain_if_nonvar(stream, S, current_output/1),
    stream_basic:current_output(S0),
    ensure_no_alias(S0, S1), % TODO: it should return a stream, not an alias
    unif_stream(S, S1).

ensure_no_alias(S0, S) :-
    ( atom(S0) -> get_stream(S0, S) ; S = S0 ).

:- export(at_end_of_stream/1).
at_end_of_stream(S) :-
    chk_resolve_stream_alias(S, S2, at_end_of_stream/1),
    io_basic:at_end_of_stream(S2).

:- export(get_code/1).
get_code(A) :-
    chk_domain_if_nonvar(in_character_code, A, get_code/1),
    iso_incomplete:current_input(S),
    iso_incomplete:get_code(S, A).

:- export(get_code/2).
get_code(S,A) :-
    chk_domain_if_nonvar(in_character_code, A, get_code/2),
    chk_resolve_stream_alias_type(input, S, S2, text, get_code/2),
    fix_eof(S2, -1, io_basic:get_code, A),
    postchk_character_code(A, get_code/2).
 
:- export(peek_code/1).
peek_code(A) :-
    chk_domain_if_nonvar(in_character_code, A, peek_code/1),
    iso_incomplete:current_input(S),
    iso_incomplete:peek_code(S, A).

:- export(peek_code/2). 
peek_code(S,A) :-
    chk_domain_if_nonvar(in_character_code, A, peek_code/2),
    chk_resolve_stream_alias_type(input, S, S2, text, peek_code/2),
    fix_eof(S2, -1, io_basic:peek_code, A),
    postchk_character_code(A, peek_code/2).

:- export(put_code/2). 
put_code(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, put_code/2),
    fix_err(io_basic:put_code(S2,A), out_err_fix).

% TODO: permission_error(modify, stream, S) is valid for set_stream_type/2, set_stream_eof_action/2
out_err_fix(permission_error(modify, stream, S), permission_error(output, stream, S)).

:- export(nl/1). 
nl(S) :-
    chk_resolve_stream_alias_type(output, S, S2, text, nl/1),
    fix_err(io_basic:nl(S2), out_err_fix).

:- export(tab/2).
tab(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, tab/2),
    io_basic:tab(S2,A).

:- export(get_byte/1).
get_byte(A) :-
    chk_domain_if_nonvar(in_byte, A, get_byte/1),
    iso_incomplete:current_input(S),
    iso_incomplete:get_byte(S, A).

:- export(get_byte/2).
get_byte(S,A) :-
    chk_domain_if_nonvar(in_byte, A, get_byte/2),
    chk_resolve_stream_alias_type(input, S, S2, binary, get_byte/2),
    fix_eof(S2, -1, io_basic:get_byte, A).

:- export(peek_byte/1).
peek_byte(A) :-
    chk_domain_if_nonvar(in_byte, A, peek_byte/1),
    iso_incomplete:current_input(S), iso_incomplete:peek_byte(S, A).

:- export(peek_byte/2).
peek_byte(S,A) :-
    chk_domain_if_nonvar(in_byte, A, peek_byte/2),
    chk_resolve_stream_alias_type(input, S, S2, binary, peek_byte/2),
    fix_eof(S2, -1, io_basic:peek_byte, A).

:- export(put_byte/1).
put_byte(A) :-
    iso_incomplete:current_output(S),
    iso_incomplete:put_byte(S, A).

:- export(put_byte/2). 
put_byte(S,A) :-
    chk_nonvar(A, put_byte/2),
    chk_domain_if_nonvar(byte, A, put_byte/2),
    chk_resolve_stream_alias_type(output, S, S2, binary, put_byte/2),
    fix_err(io_basic:put_byte(S2,A), out_err_fix).

:- export(display/2). 
display(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, display/2),
    io_basic:display(S2,A).

:- export(displayq/2). 
displayq(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, displayq/2),
    io_basic:displayq(S2,A).

:- export(get_char/1).
get_char(A) :-
    chk_domain_if_nonvar(in_character, A, get_char/1),
    iso_incomplete:current_input(S),
    iso_incomplete:get_char(S, A).

:- export(get_char/2).
get_char(S,A) :-
    chk_domain_if_nonvar(in_character, A, get_char/2),
    chk_resolve_stream_alias_type(input, S, S2, text, get_char/2),
    fix_eof(S2, end_of_file, iso_char:get_char, A),
    postchk_character(A, get_char/2).

:- export(peek_char/1).
peek_char(A) :-
    chk_domain_if_nonvar(in_character, A, peek_char/1),
    iso_incomplete:current_input(S),
    iso_incomplete:peek_char(S, A).

:- export(peek_char/2). 
peek_char(S,A) :-
    chk_domain_if_nonvar(in_character, A, peek_char/2),
    chk_resolve_stream_alias_type(input, S, S2, text, peek_char/2),
    fix_eof(S2, end_of_file, iso_char:peek_char, A),
    postchk_character(A, peek_char/2).
    
:- export(put_char/1).
put_char(A) :-
    iso_incomplete:current_output(S),
    iso_incomplete:put_char(S, A).

:- export(put_char/2).
put_char(S,A) :-
    chk_domain_nonvar(character, A, put_char/2),
    chk_resolve_stream_alias_type(output, S, S2, text, put_char/2),
    fix_err(iso_char:put_char(S2,A), out_err_fix).

:- export(read/1).
read(A) :-
    iso_incomplete:current_input(S), iso_incomplete:read_term(S, A, []).

:- export(read/2).
read(S,A) :-
    chk_resolve_stream_alias_type(input, S, S2, text, read/2),
    read:read(S2,A).

:- export(read_term/2).
read_term(A, B) :-
    iso_incomplete:current_input(S), iso_incomplete:read_term(S, A, B).

:- export(read_term/3).
read_term(S,A,B) :-
    chk_resolve_stream_alias_type(input, S, S2, text, read_term/3),
    fix_err(read:read_term(S2,A,B), read_term_fix(S2)).

% read_term_fix(permission_error(access, past_end_of_stream, S), permission_error(input, past_end_of_stream, S1)) :- ensure_no_alias(S, S1). % TODO: rename before?
% read_term_fix(permission_error(access, stream, S), permission_error(input, stream, S1))  :- ensure_no_alias(S, S1). % TODO: rename before?
read_term_fix(ActualS, permission_error(access, past_end_of_stream, S0), permission_error(input, past_end_of_stream, S)) :- fix_nil_stream(ActualS, S0, S).
read_term_fix(_, permission_error(access, stream, S), permission_error(input, stream, S)).

% TODO: read_term/3 uses set_input internally and forgets about the actual stream, thus the error is meaningless, resolve the alias instead
fix_nil_stream(ActualS, [], S) :- !, S = ActualS.
fix_nil_stream(_, S, S).

:- export(write_term/2).
write_term(A, B) :-
    iso_incomplete:current_output(S), iso_incomplete:write_term(S, A, B).

% TODO: type_error should report the whole list, or just the tail
:- export(write_term/3).
write_term(S,A,B) :-
    chk_resolve_stream_alias_type(output, S, S2, text, write_term/3),
    write:write_term(S2,A,B).

:- export(write/1).
write(A) :-
    iso_incomplete:current_output(S), iso_incomplete:write_term(S, A, [numbervars(true)]).

:- export(write/2). 
write(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, write/2),
    fix_err(write:write(S2,A), out_err_fix).

:- export(writeq/2). 
writeq(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, writeq/2),
    write:writeq(S2,A).

:- export(write_canonical/2). 
write_canonical(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, write_canonical/2),
    write:write_canonical(S2,A).

:- export(print/2). 
print(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, print/2),
    write:print(S2,A).

:- export(printq/2). 
printq(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, printq/2),
    write:printq(S2,A).

:- export(portray_clause/2).
portray_clause(S,A) :-
    chk_resolve_stream_alias_type(output, S, S2, text, portray_clause/2),
    write:portray_clause(S2,A).

:- export(current_op/3).
current_op(S,A,B) :- 
    chk_domain_if_nonvar(atom, B, current_op/3),
    chk_domain_if_nonvar(operator_specifier, A, current_op/3),
    chk_domain_if_nonvar(operator_priority, S, current_op/3),
    operators:current_op(S,A,B).

:- export(op/3).
op(S,A,B) :-
    chk_domain_nonvar(operator_priority, S, op/3),
    chk_ops(B),
    chk_domain_nonvar(operator_specifier, A, op/3),
    operators:op(S,A,B).

chk_ops(B) :-
    ( atom(B) -> true
    ; chk_ops_list(B)
    ).

chk_ops_list(Xs) :- var(Xs), !, throw(error(instantiation_error, op/3)).
chk_ops_list([]) :- !.
chk_ops_list([X|Xs]) :- !,
    chk_nonvar(X, op/3),
    ( X = ',' -> throw(error(permission_error(modify,operator,','), op/3))
    ; ( atom(X) -> true ; throw(error(type_error(atom,X), op/3)) )
    ),
    chk_ops_list(Xs).
chk_ops_list(Xs) :- throw(error(type_error(list, Xs), op/3)).

chk_resolve_stream_alias(S, S2, Where) :-
    chk_domain_nonvar(stream_or_alias, S, Where),
    resolve_stream_alias(S, S2, Where).

resolve_stream_alias(Alias, Stream, Where) :- atom(Alias), !,
    ( Alias = user_input -> Stream = Alias
    ; Alias = user_output -> Stream = Alias
    ; Alias = user_error -> Stream = Alias
    ; Alias = user -> Stream = Alias
    ; '$alias_stream'(Alias, Stream0) -> Stream = Stream0
    ; throw(error(existence_error(stream, Alias), Where))
    ).
resolve_stream_alias(Stream, Stream, _Where).

chk_resolve_stream_alias_type(Mode, S, S2, Type, Where) :-
    chk_resolve_stream_alias(S, S2, Where),
    chk_stream_type(Mode, S2, Type, Where).

chk_stream_type(Op, Stream, Type, Culprit) :- 
    stream_property(Stream, Op), !,
    get_stream_type(Stream, Type0),
    ( Type0 = Type -> true
    ; stream_type_to_perm(Type0, Perm),
      throw(error(permission_error(Op, Perm, Stream), Culprit))
    ).  
chk_stream_type(_Op, _Stream, _Type, _Culprit). % TODO: mode mismatch, checked later

stream_type_to_perm(text, text_stream).
stream_type_to_perm(binary, binary_stream).

% TODO: very slow! this must be implemented in stream_basic.c
:- meta_predicate fix_eof(?, ?, pred(2), ?).
fix_eof(Stream, Eof, Pred, Code) :-
    get_stream_eof_action(Stream, Action),
    catch(fix_err(Pred(Stream, Code), fix_access_err), 
          error(permission_error(access, past_end_of_stream, _ErrStream), ImplDef), % TODO: fixme, Ciao should throw 'input' instead of 'access'
          handle_fix_eof(Action, Eof, Pred, Stream, ImplDef, Code)).

fix_access_err(permission_error(access, stream, S), permission_error(input, stream, S)).

:- meta_predicate handle_fix_eof(?, ?, pred(2), ?, ?, ?).    
handle_fix_eof(Action, Eof, Pred, Stream, ImplDef, Code) :-
    ( Action = eof_code -> Code = Eof
    ; Action = reset -> clearerr(Stream), Pred(Stream, Code)
    ; Action = error -> throw(error(permission_error(input, past_end_of_stream, Stream), ImplDef))
    ).

postchk_character_code(Code, Where) :-
    % NOTE: the ISO standard says that the repr error here is
    %   'character' instead of 'character_code' (JF)
    ( Code = 0 -> throw(error(representation_error(character), Where))
    ; true
    ).

postchk_character(Char, Where) :-
    ( Char = '' -> throw(error(representation_error(character), Where)) % TODO: more cases? (JF)
    ; true
    ).
  
% % TODO: unused?
% fix_exception_peek_char_type_error(Stream, Pred) :-
%     catch(Pred(Stream),
%           error(permission_error(access, past_end_of_stream, _ErrStream), ImplDef),
%           throw(error(type_error(in_character,1),ImplDef))).

% ----------------------------------------------------------------
%! # checks

chk_nonvar(X, Where) :-
    ( var(X) -> throw(error(instantiation_error, Where)) ; true ).

chk_domain_if_nonvar(Ty, X, Where) :-
    ( var(X) -> true
    ; chk_domain(Ty, X, Where)
    ).

chk_domain_nonvar(Ty, X, Where) :-
    chk_nonvar(X, Where),
    chk_domain(Ty, X, Where).

chk_domain(true, _, _) :- !. % (trivial check, for is_dom/2)
chk_domain(Ty, X, Where) :- is_dom(Ty, TyBase), !,
    chk_domain(TyBase, X, Where), % Note: first check base type
    ( \+ check_ty(Ty, X) -> throw(error(domain_error(Ty, X), Where)) ; true ).
chk_domain(Ty, X, Where) :- is_rep(Ty, TyBase), !,
    chk_domain(TyBase, X, Where), % Note: first check base type
    ( \+ check_ty(Ty, X) -> throw(error(representation_error(Ty), Where)) ; true ).
chk_domain(Ty, X, Where) :-
    ( \+ check_ty(Ty, X) -> throw(error(type_error(Ty, X), Where)) ; true ).

% types representing domains
is_dom(stream, true).
is_dom(stream_property, true).
is_dom(stream_position, true).
is_dom(operator_specifier, atom).
is_dom(operator_priority, integer).
is_dom(io_mode, atom).

% representation checks
is_rep(character_code, integer).
is_rep(in_character_code, integer).

% (instantiation checks)
check_ty(stream, X) :- nonvar(X), stream(X).
check_ty(stream_or_alias, X) :- nonvar(X), stream_or_alias(X).
check_ty(integer, X) :- integer(X).
check_ty(atom, X) :- atom(X).
check_ty(byte, X) :- is_byte(X).
check_ty(in_byte, X) :- is_in_byte(X).
check_ty(character, X) :- is_character(X).
check_ty(in_character, X) :- is_in_character(X).
check_ty(character_code, X) :- is_character_code(X).
check_ty(in_character_code, X) :- is_in_character_code(X).
check_ty(operator_specifier, X) :- nonvar(X), is_operator_specifier(X).
check_ty(operator_priority, X) :- is_operator_priority(X).
check_ty(stream_property, X) :- nonvar(X), is_stream_property(X).
check_ty(stream_position, X) :- nonvar(X), X = '$pos'(_). % TODO: implement
check_ty(io_mode, X) :- nonvar(X), io_mode(X).

% Common types:
%
%   byte: integer in 0..255 interval
%   in_byte: integer in 0..255 interval or -1 for end-of-file
%   character: a character atom
%   in_character: a character atom or end_of_file
%   character_code: integer in 1..maxcode interval
%   in_character_code: integer in 1..maxcode interval or -1 for end-of-file

% Note: maxcode relaxed for unicode % TODO: add some value?

is_byte(Byte) :- integer(Byte), Byte >= 0, Byte =< 255.

is_in_byte(Byte) :- integer(Byte),
    ( Byte = -1 -> true
    ; Byte >= 0, Byte =< 255
    ).

is_character_code(Code) :- integer(Code), Code >= 1.

is_in_character_code(Code) :- integer(Code),
    ( Code = -1 -> true
    ; Code >= 1
    ).

is_character(Char) :-
    atom(Char),                   
    atom_length(Char, 1),         
    char_code(Char, Code),        
    Code >= 0.

is_in_character(Char) :-
    atom(Char),
    ( Char = end_of_file -> true
    ; atom_length(Char, 1),
      char_code(Char, Code),
      Code >= 0
    ).

is_operator_priority(X) :-
    integer(X),
    X >= 0,
    X =< 1200.

% TODO: It is duplicated in basic_props.pl
is_operator_specifier(X) :-
    ( X = fy ;
      X = fx ;        
      X = yfx ;
      X = xfy ;
      X = xfx ;
      X = yf ;
      X = xf ).

% Fix errors
:- meta_predicate fix_err(goal, pred(2)).
fix_err(G, ErrTr) :-
    catch(G, E, fix_err_(E, ErrTr)).

:- meta_predicate fix_err_(?, pred(2)).
fix_err_(E, ErrTr) :-
    ( E = error(E0, Where), ErrTr(E0, E1) -> throw(error(E1, Where))
    ; throw(E)
    ).

