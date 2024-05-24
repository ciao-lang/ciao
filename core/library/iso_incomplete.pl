:- module(iso_incomplete, [
    absolute_file_name/2,
    open/4,
    close/1,
    close/2,
    close_options/1,
    close_option/1,
    stream_property/2,
    stream_prop/1,
    set_stream_position/2,
    %
    set_input/1, 
    set_output/1,
    flush_output/1,
    current_input/1,
    current_output/1,
    %
    at_end_of_stream/1,
    get_code/1,
    get_code/2,
    peek_code/1,
    peek_code/2, 
    put_code/2, 
    nl/1, 
    tab/2,
    get_byte/1,
    get_byte/2,
    peek_byte/1,
    peek_byte/2,
    put_byte/1,
    put_byte/2, 
    display/2, 
    displayq/2, 
    %
    get_char/1,
    get_char/2,
    peek_char/1,
    peek_char/2, 
    put_char/2,
    put_char/1,
    %
    read/1,
    read/2,
    read_term/2,
    read_term/3,
    %
    write_term/2,
    write_term/3,
    write/1,
    write/2, 
    writeq/2, 
    write_canonical/2, 
    print/2, 
    printq/2, 
    portray_clause/2,
    current_op/3,
    op/3
], [assertions,isomodes,datafacts]).

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
:- use_module(library(iso_char)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(operators)).

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

% Stream eof_action
:- data '$stream_eof_action'/2.

% Stream position
:- data '$stream_position'/2.

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

get_stream_eof_action(Stream, Action) :-
    ( '$stream_eof_action'(Stream, Action0) -> true ; Action0 = eof_code ),
    Action = Action0.

set_eof_action(Stream,EofAction) :-
     set_fact('$stream_eof_action'(Stream, EofAction)) .

set_reposition(Stream, Reposition) :-
    set_fact('$stream_position'(Stream, Reposition)).

% ---------------------------------------------------------------------------
% TODO: argument order changed!
:- pred open(+sourcename, +io_mode, ?stream, +open_option_list).

open(File, Mode, Stream, Opts) :-
    ( var(File) ; var(Mode) ; var(Opts) -> fix_exception_open ;
    catch(
        (get_open_opts(Opts, OtherOpts, text, Type, Aliases, error, EofAction, Reposition, true),
        stream_basic:open(File, Mode, Stream, OtherOpts),
        set_stream_type(Stream, Type),
        set_aliases(Aliases, Stream),
        set_eof_action(Stream, EofAction),
        set_reposition(Stream, Reposition)),
        error(domain_error(open_option_list, [bar]), _),
        handle_exception
    )).

handle_exception :-
    throw(error(domain_error(stream_option, bar), _)).

% Get (and check) aliases options
get_open_opts([], [], Type, Type, [], EofAction, EofAction, Reposition, Reposition).
get_open_opts([Opt|Opts], OtherOpts, Type0, Type, Aliases, EofAction0, EofAction, Reposition0, Reposition) :-
    ( Opt = alias(Alias) ->
        check_alias(Alias),
        OtherOpts = OtherOpts0,
        Type1 = Type0,
        Aliases = [Alias|Aliases0],
        EofAction1 = EofAction0,
        Reposition1 = Reposition0
    ; Opt = type(Type1) ->
        check_stream_type(Type1),
        OtherOpts = OtherOpts0,
        Aliases = Aliases0,
        EofAction1 = EofAction0,
        Reposition1 = Reposition0
    ; Opt = eof_action(EofAction1) ->
        check_eof_action(EofAction1),
        OtherOpts = OtherOpts0,
        Type1 = Type0,
        Aliases = Aliases0,
        Reposition1 = Reposition0
    ; Opt = reposition(Reposition1) ->
        OtherOpts = OtherOpts0,
        Type1 = Type0,
        Aliases = Aliases0,
        EofAction1 = EofAction0
    ; OtherOpts = [Opt|OtherOpts0],
      Type1 = Type0,
      Aliases = Aliases0,
      EofAction1 = EofAction0
    ),
    get_open_opts(Opts, OtherOpts0, Type1, Type, Aliases0, EofAction1, EofAction, Reposition1, Reposition).

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

check_eof_action(Action) :-
    ( var(Action) ->
        throw(error(instantiation_error, 'stream_basic:$open'/4-4))
    ; Action = eof_code -> true
    ; Action = reset -> true
    ; Action = error -> true
    ; throw(error(domain_error(stream_option, eof_action(Action)), 'stream_basic:$open'/4-4))
    ).


% ---------------------------------------------------------------------------

:- pred close(@stream,@close_options). 

:- doc(bug, "close/2 not complete w.r.t. iso standard.").


close(S, _) :-
    resolve_stream_alias(S, S2),
    ( nonvar(S2) -> remove_aliases(S2) ; fix_exception_close ),
    retractall_fact('$stream_type'(S2,_)),
    retractall_fact('$stream_eof_action'(S2,_)),
    catch(catch(stream_basic:close(S2),
                error(domain_error(stream_or_alias, ErrStream), ImplDef),
                throw(error(existence_error(stream, ErrStream), ImplDef))),
          error(existence_error(stream,foo), ImplDef),
          throw(error(domain_error(stream_or_alias,foo), ImplDef))).
    
    
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

% TODO: warnings due to missing qualification in comp expansion!
:- pred close(@stream). 
close(S) :-
    (var(S)-> fix_exception_close; close(S, [])).

% ---------------------------------------------------------------------------

:- pred stream_property(?stream, ?stream_prop).

:- doc(bug, "stream_property/2 not complete w.r.t. iso standard.").

stream_property(S, P) :- 
    nonvar(S), S=user_output, !,
    ( P = output
    ; P = alias(user_output)
    ; P = mode(output)
    ; P = type(text)
    ).

stream_property(S, P) :- 
    nonvar(S),S = user_input, !,
    ( P = input
    ; P = alias(user_input)
    ; P = mode(input)
    ; P = type(text)
    ).

stream_property(S, P) :- 
    nonvar(S), S=user_error, !,
    ( P = error
    ; P = alias(user_error)
    ; P = type(text)
    ).

stream_property(S, P) :-
     current_stream(File, Mode, S), % TODO: segfault with get_stream(user_output, S), current_stream(A,B,S).
    ( P = file_name(File)
    ; P = mode(Mode)
    ; P = alias(Alias), alias_stream(Alias, S)
    ; P = type(Type), get_stream_type(S, Type)
    ; P = end_of_stream(EOS), stream_state(S, EOS)
     % (last case)
        % ( Mode = read -> P = input ; P = output )
    ; P = input, Mode = read
    ; P = output, \+ Mode = read
 %   )
    ).

stream_state(Stream, EOS) :-
 % TODO: if is an output stream EOS = not    
    (   io_basic:at_end_of_stream(Stream) ->
        catch(io_basic:peek_code(Stream, Code),
              error(permission_error(access, past_end_of_stream, _), _),
              true), 
        (   Code == -1 ->
            EOS = at  
        ;   EOS = past  
        )
    ; EOS = not
    ).

:- prop stream_prop(P) + regtype
   # "@var{P} is a valid stream property: @includedef{stream_prop/1}".  

stream_prop(input).
stream_prop(output).
stream_prop(file_name(File)) :- atm(File).
stream_prop(mode(Mode)) :- atm(Mode).
stream_prop(alias(Alias)) :- atm(Alias).
stream_prop(end_of_stream(EOS)) :- end_of_stream_t(EOS).

:- prop end_of_stream_t(Type) + regtype
   # "@var{Type} depends on the current position of the stream in the file.
 not if not all the characters have been read, at if it is located just before the
 end_of_file and past if all the characters have already been read and end_of_file".

end_of_stream_t(at).
end_of_stream_t(past).
end_of_stream_t(not).

%----------------------------------------------------------------------------
% TODO: Implement for other cases. There is only exception handling.
set_stream_position(Stream, Pos) :-
    (   iso_incomplete:current_input(Stream), var(Pos)
    ->  throw(error(instantiation_error, not_implemented))
    ;   (   integer(Pos)
        ->  iso_incomplete:close(Stream)
        ;   throw(error(domain_error(stream_position, Pos), not_implemented))
        )
    ).

set_input(S) :- resolve_stream_alias(S, S2), fix_exception_set_input(S2, stream_basic:set_input).
set_output(S) :- resolve_stream_alias(S, S2), fix_exception_set_output(S2, stream_basic:set_output).

flush_output(S) :- ( var(S) -> throw(error(instantiation_error, flush_output/1)) ; true),
    resolve_stream_alias(S, S2),
    fix_exception_flush_output(S2, stream_basic:flush_output).

current_input(S) :-
    (nonvar(S), \+stream(S) -> fix_exception_current
    ;resolve_stream_alias(S, S2),
     ( \+stream(S2) -> throw(error(domain_error(stream, foo), current_input/1))
     ;stream_basic:current_input(S2))).

current_output(S) :-
    (nonvar(S), \+stream(S) -> fix_exception_current
    ;resolve_stream_alias(S, S2),
     ( \+stream(S2) -> throw(error(domain_error(stream, foo), current_output/2))
      ;stream_basic:current_output(S2))).

at_end_of_stream(S) :- resolve_stream_alias(S, S2), io_basic:at_end_of_stream(S2).

get_code(A) :- (nonvar(A), \+ integer(A) -> fix_exception_type_error_code(A);
     (integer(A), \+ is_in_character(A) -> throw(error(representation_error(in_character_code), get_code/1));
    iso_incomplete:current_input(S), iso_incomplete:get_code(S, A))).
get_code(S,A) :- (nonvar(A), \+ integer(A) -> fix_exception_type_error_code(A);
    resolve_stream_alias(S, S2), fix_eof(S2, -1, io_basic:get_code, A),
    check_code(A, get_code/2)).
 
peek_code(A) :- (nonvar(A), \+ integer(A) -> fix_exception_type_error_code(A);
    (integer(A), \+ is_in_character(A) -> throw(error(representation_error(in_character_code), peek_code/1));
    iso_incomplete:current_input(S), iso_incomplete:peek_code(S, A))).
peek_code(S,A) :- (var(S) -> throw(error(instantiation_error,S));
    (nonvar(A), \+ integer(A) -> fix_exception_type_error_code(A); 
    resolve_stream_alias(S, S2),
    chk_type(input, S2,  text, peek_code/2),
    fix_eof(S2, -1, io_basic:peek_code, A),
    check_code(A, peek_code/2))).

put_code(S,A) :- (var(S) -> throw(error(instantiation_error,S));
    resolve_stream_alias(S, S2),
    chk_type(output, S2,  text, put_code/2),
    catch(io_basic:put_code(S2,A),
          error(permission_error(modify, stream, ErrStream), ImplDef),
          fix_exception_permission_error(ErrStream, ImplDef))).

nl(S) :- resolve_stream_alias(S, S2), fix_exception_nl(S2,io_basic:nl).
tab(S,A) :- resolve_stream_alias(S, S2), io_basic:tab(S2,A).
get_byte(A) :- (nonvar(A), \+is_byte(A) -> fix_exception_type_error_byte(A)
       ; iso_incomplete:current_input(S), iso_incomplete:get_byte(S, A)).
get_byte(S,A) :- resolve_stream_alias(S, S2), fix_eof(S2, -1, io_basic:get_byte, A).
peek_byte(A) :- (nonvar(A), \+is_byte(A) -> fix_exception_type_error_byte(A)
       ; iso_incomplete:current_input(S), iso_incomplete:peek_byte(S, A)).
peek_byte(S,A) :- resolve_stream_alias(S, S2), fix_eof(S2, -1, io_basic:peek_byte, A).

put_byte(A) :-
    iso_incomplete:current_output(S),
    iso_incomplete:put_byte(S, A).
put_byte(S,A) :- (var(S) -> throw(error(instantiation_error,S));
    (nonvar(A), \+is_byte(A) -> fix_exception_type_error_put_byte(A);                                     
    resolve_stream_alias(S, S2),
    (var(A) -> fix_exception_put_byte(S2, io_basic:put_byte)
    ;chk_type(output, S2,  binary, put_byte/2),
        catch(io_basic:put_byte(S2,A),
          error(permission_error(modify, stream, ErrStream), ImplDef),
          fix_exception_permission_error(ErrStream, ImplDef))))).
display(S,A) :- resolve_stream_alias(S, S2), io_basic:display(S2,A).
displayq(S,A) :- resolve_stream_alias(S, S2), io_basic:displayq(S2,A).

get_char(A) :- (nonvar(A), \+ is_in_character(A) -> fix_exception_type_error_char(A)
        ; iso_incomplete:current_input(S), iso_incomplete:get_char(S, A)).
get_char(S,A) :-(nonvar(A), \+ is_in_character(A) -> fix_exception_type_error_char(A);
    resolve_stream_alias(S, S2), fix_eof(S2, end_of_file, iso_char:get_char, A),
    check_char(A, get_char/2)).
peek_char(A) :- (nonvar(A), \+ is_in_character(A) -> fix_exception_type_error_char(A);
    iso_incomplete:current_input(S), iso_incomplete:peek_char(S, A)).
peek_char(S,A) :- (nonvar(A), \+ is_in_character(A), A \= end_of_file
                   -> fix_exception_type_error_char(A); true ),
     (var(S) -> throw(error(instantiation_error,S)); true ),
     resolve_stream_alias(S, S2),
     (nonvar(S2) -> fix_eof(S2, end_of_file, iso_char:peek_char, A) ; true ),
     check_char(A, peek_char/2).
    
put_char(A) :-  iso_incomplete:current_output(S),
    iso_incomplete:put_char(S, A).
put_char(S,A) :-(var(S) -> throw(error(instantiation_error,S));
    (nonvar(A), \+ is_in_character(A) -> fix_exception_type_error_putchar(A);
    resolve_stream_alias(S, S2),
    chk_type(output, S2,  text, put_char/2),
    catch(iso_char:put_char(S2,A),
          error(permission_error(modify, stream, ErrStream), ImplDef),
          fix_exception_permission_error(ErrStream, ImplDef)))).

read(A) :- iso_incomplete:current_input(S), iso_incomplete:read_term(S, A, []). 
read(S,A) :- resolve_stream_alias(S, S2), read:read(S2,A).
read_term(A, B) :- iso_incomplete:current_input(S), iso_incomplete:read_term(S, A, B).    
read_term(S,A,B) :- resolve_stream_alias(S, S2),
    chk_type(input, S2,  text, read_term/3),
    fix_exception_read_term(S2, A, B).

write_term(A, B) :- iso_incomplete:current_output(S), iso_incomplete:write_term(S, A, B). 
write_term(S,A,B) :- resolve_stream_alias(S, S2),
    chk_type(output, S2,  text, write_term/3),
    catch(write:write_term(S2,A,B),
          error(type_error(list,foo), ImplDef),
          throw(error(type_error(list,[quoted(true)|foo]), ImplDef))).

write(A) :- iso_incomplete:current_output(S), iso_incomplete:write_term(S, A, [numbervars(true)]).
write(S,A) :- resolve_stream_alias(S, S2),
    catch(write:write(S2,A),
          error(permission_error(modify, stream, ErrStream), ImplDef),
          fix_exception_permission_error(ErrStream, ImplDef)).
writeq(S,A) :- resolve_stream_alias(S, S2), write:writeq(S2,A).
write_canonical(S,A) :- resolve_stream_alias(S, S2), write:write_canonical(S2,A).
print(S,A) :- resolve_stream_alias(S, S2), write:print(S2,A).
printq(S,A) :- resolve_stream_alias(S, S2), write:printq(S2,A).
portray_clause(S,A) :- resolve_stream_alias(S, S2), write:portray_clause(S2,A).

current_op(S,A,B) :- 
    ( nonvar(B), \+atom(B) -> fix_exception_type_error_current_op(S, B) ; true ),
    ( nonvar(A), \+atom(A) -> fix_exception_type_error_current_op(S, A) ; true ),
    ( nonvar(S), \+is_operator_priority(S) -> throw(error(domain_error(operator_priority,S), current_op/3)); true ),
    ( nonvar(A), \+is_operator_specifier(A) -> throw(error(domain_error(operator_specifier,A), current_op/3)); true ),
    operators:current_op(S,A,B).

op(S,A,B) :-
    ( nonvar(S) , \+ integer(S) -> throw(error(type_error(integer, S), op/3 )); true ),
    ( var(S) -> throw(error(instantiation_error, op/3)); true ),
    ( nonvar(S), \+integer(S)-> throw(error(type_error(integer, S), op/3)); true ),
    ( var(A) -> throw(error(instantiation_error, op/3)); true ),
    ( \+ is_operator_specifier(A) -> throw(error(domain_error(operator_specifier, A), op/3)); true ),
    ( \+ atom(A) -> throw(error(type_error(atom, A), op/3 )); true ),
    ( var(B) -> throw(error(instantiation_error, op/3)); true ),
    ( \+ is_list(B), \+ atom(B)-> throw(error(type_error(list, B), op/3)); true ),
    ( atom(B) -> true; error_in_ops(B) ),
    operators:op(S,A,B).

:- use_package(hiord). % TODO: add if not included

chk_type(Op, Stream, Type, Culprit) :- 
    stream_property(Stream, Op), !,
    get_stream_type(Stream, Type0),
    ( Type0 = Type -> true
    ; stream_type_to_perm(Type0, Perm),
      throw(error(permission_error(Op, Perm, Stream), Culprit))
    ).  
chk_type(_Op, _Stream, _Type, _Culprit).

stream_type_to_perm(text, text_stream).
stream_type_to_perm(binary, binary_stream).

% TODO: very slow! this must be implemented in stream_basic.c
:- meta_predicate fix_eof(?, ?, pred(2), ?).
fix_eof(Stream, Eof, Pred, Code) :-
    get_stream_eof_action(Stream, Action),
    catch(catch(Pred(Stream, Code),
                error(permission_error(access, stream, ErrStream), ImplDef),
                throw(error(permission_error(input, stream, ErrStream), ImplDef))), 
          error(permission_error(access, past_end_of_stream, _ErrStream), ImplDef), % TODO: fixme, Ciao should throw 'input' instead of 'access'
          handle_fix_eof(Action, Eof, Pred, Stream, ImplDef, Code)).

:- meta_predicate handle_fix_eof(?, ?, pred(2), ?, ?, ?).    
handle_fix_eof(Action, Eof, Pred, Stream, ImplDef, Code) :-
    ( Action = eof_code -> Code = Eof
    ; Action = reset -> clearerr(Stream), Pred(Stream, Code)
    ; Action = error -> throw(error(permission_error(input, past_end_of_stream, Stream), ImplDef))
    ).

check_code(Code, ImplDef) :-
    ( Code = 0 -> throw(error(representation_error(character), ImplDef))
    ; true
    ).

check_char(Char, ImplDef) :-
    ( Char = '' -> throw(error(representation_error(character), ImplDef))
    ; true
    ).
                               
fix_exception_open :-
    throw(error(instantiation_error, open/4)).

fix_exception_set_input(Stream, Pred) :-
    catch(Pred(Stream), error(permission_error(access,stream, ErrStream), ImplDef),
          throw(error(permission_error(input, stream, ErrStream), ImplDef))).

fix_exception_set_output(Stream, Pred) :-
    catch(Pred(Stream), error(permission_error(modify,stream, ErrStream), ImplDef),
          throw(error(permission_error(output, stream, ErrStream), ImplDef))).

fix_exception_flush_output(Stream, Pred) :-
    catch(
        catch(Pred(Stream),
                 error(permission_error(modify,stream, ErrStream), ImplDef),
                 throw(error(permission_error(output, stream, ErrStream), ImplDef))),
        error(domain_error(stream_or_alias, st_o), ImplDef),
        throw(error(permission_error(output,stream, st_o), ImplDef))).

fix_exception_current :-
    throw(error(domain_error(stream, foo), current_input/1)).

fix_exception_peek_char_type_error(Stream, Pred) :-
    catch(Pred(Stream), error(permission_error(access , past_end_of_stream, _ErrStream), ImplDef),
          throw(error(type_error(in_character,1),ImplDef))).

fix_exception_put_byte(Stream, Pred) :-
        catch(Pred(Stream), error(type_error(byte, Stream),ImplDef),
                 throw(error(instantiation_error, ImplDef))).

fix_exception_nl(Stream, Pred) :-
    catch(Pred(Stream), error(permission_error(modify,stream, ErrStream), ImplDef),
          throw(error(permission_error(output, stream, ErrStream), ImplDef))).

fix_exception_close :- 
    throw(error(instantiation_error, close/1)).

fix_exception_type_error_byte(A) :-
    throw(error(type_error(in_byte,A), get_byte/2)).

fix_exception_type_error_put_byte(A) :-
    throw(error(type_error(byte,A), put_byte/1)).

fix_exception_type_error_char(A) :-
    throw(error(type_error(in_character, A), get_char/2)).

fix_exception_type_error_putchar(A) :-
    throw(error(type_error(character, A), put_char/1)).

fix_exception_type_error_code(A) :-
    throw(error(type_error(integer, A), get_code/1)).

fix_exception_read_term(S2, A, B) :-
    catch(catch( read:read_term(S2,A,B),
                 error(permission_error(access,past_end_of_stream, _ErrStream), ImplDef),
                 throw(error(permission_error(input, past_end_of_stream, S2), ImplDef))),
          error(permission_error(access, stream, user_output), ImplDef),
          throw(error(permission_error(input, stream, user_output), ImplDef))).

fix_exception_permission_error(ErrStream, ImplDef) :- 
    throw(error(permission_error(output, stream, ErrStream), ImplDef)).

fix_exception_type_error_current_op(S, Operator) :-
    throw(error(type_error(atom, Operator), S)).

is_byte(Byte) :-
    integer(Byte),   
    Byte >= 0,     
    Byte =< 255.

is_in_character(Char) :-
    atom(Char),                   
    atom_length(Char, 1),         
    char_code(Char, Code),        
    Code >= 0, Code =< 255.

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

error_in_ops([]).
error_in_ops([X|Xs]) :-
    ( var(X) -> throw(error(instantiation_error, op/3)); true ),
    ( var(Xs) -> throw(error(instantiation_error, op/3)); true ),
    ( X = ',' -> throw(error(permission_error(modify,operator,','), op/3))
    ; atom(X) ->
        error_in_ops(Xs)
    ;
        throw(error(type_error(atom,X), op/3))
    ).

is_list([]).
is_list([_|_]).