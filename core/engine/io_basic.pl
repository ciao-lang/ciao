:- module(io_basic, [], [noprelude, assertions, nortchecks, nativeprops, isomodes]).

:- doc(title, "Basic input/output stream operations").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Mats Carlsson").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides the basic input/output operations
   on streams (byte/code input/output and canonical term output). Most
   predicates are provided in two versions: one that specifies the
   input or output stream as the first argument and a second which
   omits this argument and uses the current input or output stream.

   From the @concept{ISO-Prolog} predicates for character
   input/output, only the @tt{_code} versions are provided, the
   @tt{_char} versions are given by @lib{library(iso_char)}.").

% NOTE: These are operations that new stream types should implement

:- use_module(engine(basiccontrol)).
:- use_module(engine(stream_basic)).
% TODO: the following use_module only for '$code_bytes'/3, remove if implemented in C
:- use_module(engine(arithmetic)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).

:- if(defined(optim_comp)).
:- '$native_include_c_source'(.(io_basic)).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section,"Code Input/Output").

:- export(get_code/2).
:- doc(get_code(Stream, Code), "Reads from @var{Stream} the next
   character and unifies @var{Code} with its character code.  At end of
   stream, unifies @var{Code} with the integer -1.").
:- trust pred get_code(+stream, ?int) + (iso, native).
:- if(defined(optim_comp)).
:- '$props'(get_code/2, [impnat=cbool(get2)]).
:- else.
:- impl_defined(get_code/2).
:- endif.

:- export(get_code/1).
:- doc(get_code(Code), "Behaves like @tt{current_input(S),
   get_code(S,Code)}.").
:- trust pred get_code(?int) + (iso, native).
:- if(defined(optim_comp)).
:- '$props'(get_code/1, [impnat=cbool(get)]).
:- else.
:- impl_defined(get_code/1).
:- endif.

:- export(get1_code/2).
:- doc(get1_code(Stream, Code), "Reads from @var{Stream} the next
   non-layout character (see @pred{code_class/2}) and unifies @var{Code}
   with its character code.  At end of stream, unifies @var{Code} with
   the integer -1.").
:- trust pred get1_code(+stream, ?int) + native.
:- if(defined(optim_comp)).
:- '$props'(get1_code/2, [impnat=cbool(get12)]).
:- else.
:- impl_defined(get1_code/2).
:- endif.

:- export(get1_code/1).
:- doc(get1_code(Code), "Behaves like @tt{current_input(S),
   get1_code(S,Code)}.").
:- trust pred get1_code(?int) + native.
:- if(defined(optim_comp)).
:- '$props'(get1_code/1, [impnat=cbool(get1)]).
:- else.
:- impl_defined(get1_code/1).
:- endif.

:- export(peek_code/2).
:- doc(peek_code(Stream, Code), "Unifies @var{Code} with the
   character code of the next character of @var{Stream}, leaving the
   stream position unaltered.  At end of stream, unifies @var{Code} with
   the integer -1.").
:- trust pred peek_code(+stream, ?int) + iso.
:- if(defined(optim_comp)).
:- '$props'(peek_code/2, [impnat=cbool(peek2)]).
:- else.
:- impl_defined(peek_code/2).
:- endif.

:- export(peek_code/1).
:- doc(peek_code(Code), "Behaves like @tt{current_input(S),
   peek_code(S,Code)}.").
:- trust pred peek_code(?int) + iso.
:- if(defined(optim_comp)).
:- '$props'(peek_code/1, [impnat=cbool(peek)]).
:- else.
:- impl_defined(peek_code/1).
:- endif.

:- export(skip_code/2).
:- doc(skip_code(Stream, Code), "Skips just past the next character
   code @var{Code} from @var{Stream}.").
:- trust pred skip_code(+stream, +int).
:- if(defined(optim_comp)).
:- '$props'(skip_code/2, [impnat=cbool(skip2)]).
:- else.
:- impl_defined(skip_code/2).
:- endif.

:- export(skip_code/1).
:- doc(skip_code(Code), "Behaves like @tt{current_input(S),
   skip_code(S,Code)}.").
:- trust pred skip_code(+int).
:- if(defined(optim_comp)).
:- '$props'(skip_code/1, [impnat=cbool(skip)]).
:- else.
:- impl_defined(skip_code/1).
:- endif.

:- export(skip_line/1).
:- doc(skip_line(Stream), "Skips from @var{Stream} the remaining
   input characters on the current line.  If the end of the stream is
   reached, the stream will stay at its end.  Portable among different
   operating systems.").
:- trust pred skip_line(+stream).
:- if(defined(optim_comp)).
:- '$props'(skip_line/1, [impnat=cbool(skip_line1)]).
:- else.
:- impl_defined(skip_line/1).
:- endif.

:- export(skip_line/0).
:- doc(skip_line, "Behaves like @tt{current_input(S), skip_line(S)}.").
:- trust pred skip_line.
:- if(defined(optim_comp)).
:- '$props'(skip_line/0, [impnat=cbool(skip_line)]).
:- else.
:- impl_defined(skip_line/0).
:- endif.

:- export(put_code/2).
:- doc(put_code(Stream, Code), "Outputs to @var{Stream} the
   character corresponding to character code @var{Code}.").
:- trust pred put_code(+stream, +int) + (iso, native, is_det).
:- if(defined(optim_comp)).
:- '$props'(put_code/2, [impnat=cbool(put2)]).
:- else.
:- impl_defined(put_code/2).
:- endif.

:- export(put_code/1).
:- doc(put_code(Code), "Behaves like @tt{current_output(S),
   put_code(S,Code)}.").
:- trust pred put_code(+int) + (iso, native, is_det).
:- if(defined(optim_comp)).
:- '$props'(put_code/1, [impnat=cbool(put)]).
:- else.
:- impl_defined(put_code/1).
:- endif.

:- export(nl/1).
:- doc(nl(Stream), "Outputs a newline character to @var{Stream}.
   Equivalent to @tt{put_code(Stream, 0'\\\\n)}.").
:- trust pred nl(+stream) + (iso, native, is_det, not_fails).
:- if(defined(optim_comp)).
:- '$props'(nl/1, [impnat=cbool(nl1)]).
:- else.
:- impl_defined(nl/1).
:- endif.

:- export(nl/0).
:- doc(nl, "Behaves like @tt{current_output(S), nl(S)}.").
:- trust pred nl + (iso, native, is_det, not_fails).
:- if(defined(optim_comp)).
:- '$props'(nl/0, [impnat=cbool(nl)]).
:- else.
:- impl_defined(nl/0).
:- endif.

:- export(tab/2).
:- doc(tab(Stream,Num), "Outputs @var{Num} spaces to @var{Stream}.").
:- trust pred tab(+stream,+int) + (native, is_det).
:- if(defined(optim_comp)).
:- '$props'(tab/2, [impnat=cbool(tab2)]).
:- else.
:- impl_defined(tab/2).
:- endif.

:- export(tab/1).
:- doc(tab(Num), "Behaves like @tt{current_output(S), tab(S,Num)}.").
:- trust pred tab(+int) + (native, is_det).
:- if(defined(optim_comp)).
:- '$props'(tab/1, [impnat=cbool(tab)]).
:- else.
:- impl_defined(tab/1).
:- endif.

:- export(code_class/2).
:- doc(code_class(Code,Class), "Unifies @var{Class} with an integer
   corresponding to the lexical class of the character whose code is
   @var{Code}, with the following correspondence:
   ```
    0 - layout (includes space, newline, tab)
    1 - small letter
    2 - capital letter (including '_')
    3 - digit
    4 - graphic (includes #$&*+-./:<=>?@^\\`~ )
    5 - punctuation (includes !;""'%(),[]{|} )
    6 - extended identifier continuation
    7 - invalid
   ```
   Note that in @concept{ISO-Prolog} the back quote @tt{`} is a punctuation
   character, whereas in Ciao it is a graphic character.  Thus, if
   compatibility with @concept{ISO-Prolog} is desired, the programmer should
   not use this character in unquoted names.").
:- trust pred code_class(+int,?int).
:- if(defined(optim_comp)).
:- '$props'(code_class/2, [impnat=cbool(code_class)]).
:- else.
:- impl_defined(code_class/2).
:- endif.

% TODO: implement in C
:- doc('$code_bytes'(C, Bs, Bs0), "Converts between the character code
   @var{C} and the difference list of bytes @var{Bs}-@var{Bs0} using
   UTF8 encoding. Incorrect byte sequences are decoded as 0xFFFD (rune
   error)").

'$code_bytes'(C, S, S0) :- var(C), !, rune_decode(S, S0, C).
'$code_bytes'(C, S, S0) :- rune_encode(C, S, S0).

rune_encode(C, S, S0) :- C =< 0x7F, !, S=[C|S0].
rune_encode(C, S, S0) :- C =< 0x7FF, !,
    B1 is 0xC0\/((C>>6)/\0x1F),
    B2 is (C/\0x3F)\/0x80,
    S = [B1,B2|S0].
rune_encode(C, S, S0) :- C =< 0xFFFF, !,
    B1 is 0xE0\/((C>>12)/\0xF),
    B2 is ((C>>6)/\0x3F)\/0x80,
    B3 is (C/\0x3F)\/0x80,
    S = [B1,B2,B3|S0].
rune_encode(C, S, S0) :- C =< 0x10FFFF, !,
    B1 is 0xF0\/((C>>18)/\0x7),
    B2 is ((C>>12)/\0x3F)\/0x80,
    B3 is ((C>>6)/\0x3F)\/0x80,
    B4 is (C/\0x3F)\/0x80,
    S = [B1,B2,B3,B4|S0].

rune_decode([B1|Bs], Bs0, C) :-
    ( rune_decode_(B1, Bs, Bs1, C0) -> C=C0, Bs0=Bs1
    ; C=0xFFFD, Bs=Bs0 % error rune (Unicode Replacement character)
    ).

rune_decode_(B1, Bs, Bs0, R) :- B1 =< 0x7F, !, R=B1, Bs=Bs0.
rune_decode_(B1, _, _, _) :- B1 =< 0xBF, !, fail. % invalid
rune_decode_(B1, Bs, Bs0, R) :- B1 =< 0xDF, !, % 2 bytes
    Bs = [B2|Bs0], B2/\0xC0 =:= 0x80,
    R is ((B1/\0x1F)<<6)\/(B2/\0x3F),
    R >= 0x80.
rune_decode_(B1, Bs, Bs0, R) :- B1 =< 0xEF, !, % 3 bytes
    Bs = [B2,B3|Bs0], B2/\0xC0 =:= 0x80, B3/\0xC0 =:= 0x80,
    R is ((B1/\0xF)<<12)\/((B2/\0x3F)<<6)\/(B3/\0x3F),
    R >= 0x800.
rune_decode_(B1, Bs, Bs0, R) :- B1 =< 0xF7, !, % 4 bytes
    Bs = [B2,B3,B4|Bs0], B2/\0xC0 =:= 0x80, B3/\0xC0 =:= 0x80, B4/\0xC0 =:= 0x80,
    R is ((B1/\0x7)<<18)\/((B2/\0x3F)<<12)\/((B3/\0x3F)<<6)\/(B4/\0x3F),
    R >= 0x10000.

% TODO: implement in C
:- export(string_bytes/2).
:- doc(string_bytes(String,Bytes), "@var{String} is the list of the
   character codes encoded by the list of bytes @var{Bytes}
   (equivalent to @tt{String=Bytes} if any of the lists is a list of
   ASCII codes).").

%:- trust pred string_bytes(+string,?bytelist).
%:- trust pred string_bytes(?string,+bytelist).

string_bytes(Cs, Bs) :- is_ascii(Cs), !, Bs=Cs. % fast case
string_bytes(Cs, Bs) :- is_ascii(Bs), !, Bs=Cs. % fast case
string_bytes(Cs, Bs) :- string_bytes_(Cs, Bs, []).

string_bytes_([C|Cs], Bs, Bs0) :- '$code_bytes'(C, Bs, Bs1), !, string_bytes_(Cs, Bs1, Bs0).
string_bytes_([], Bs, Bs).

is_ascii(Cs) :- var(Cs), !, fail.
is_ascii([]).
is_ascii([C|Cs]) :- integer(C), C>=0, C<0x80, is_ascii(Cs).

:- export(getct/2).
:- doc(getct(Code, Type), "Reads from the current input stream the
   next character, unifying @var{Code} with its character code, and
   @var{Type} with its lexical class.  At end of stream, unifies both
   @var{Code} and @var{Type} with the integer -1.  Equivalent to
   @begin{verbatim}
   get(Code), (Code = -1 -> Type = -1 ; code_class(Code,Type))
   @end{verbatim}").
:- trust pred getct(?int, ?int).
:- if(defined(optim_comp)).
:- '$props'(getct/2, [impnat=cbool(getct)]).
:- else.
:- impl_defined(getct/2).
:- endif.

:- export(getct1/2).
:- doc(getct1(Code, Type), "Reads from the current input stream the
   next non-layout character, unifying @var{Code} with its character
   code, and @var{Type} with its lexical class (which will be nonzero).
   At end of stream, unifies both @var{Code} and @var{Type} with the
   integer -1.  Equivalent to
   @begin{verbatim}
   get1(Code), (Code = -1 -> Type = -1 ; code_class(Code,Type))
   @end{verbatim}").
:- trust pred getct1(?int, ?int).
:- if(defined(optim_comp)).
:- '$props'(getct1/2, [impnat=cbool(getct1)]).
:- else.
:- impl_defined(getct1/2).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section,"Byte Input/Output").

:- export(get_byte/2).
:- doc(get_byte(Stream, Byte), "Reads from @var{Stream} the next byte
   and unifies it with @var{Byte}.  At end of stream, unifies
   @var{Byte} with the integer -1.").
:- trust pred get_byte(+stream, ?int) + (iso, native).
%:- trust pred get_byte(+stream, ?in_byte) + (iso, native).
:- if(defined(optim_comp)).
:- '$props'(get_byte/2, [impnat=cbool(get_byte2)]).
:- else.
:- impl_defined(get_byte/2).
:- endif.

:- export(get_byte/1).
:- doc(get_byte(Byte), "Behaves like @tt{current_input(S),
   get_byte(S,Byte)}.").
:- trust pred get_byte(?int) + (iso, native).
%:- trust pred get_byte(?in_byte) + (iso, native).
:- if(defined(optim_comp)).
:- '$props'(get_byte/1, [impnat=cbool(get_byte1)]).
:- else.
:- impl_defined(get_byte/1).
:- endif.

:- export(peek_byte/2).
:- doc(peek_byte(Stream, Byte), "Unifies @var{Byte} with the next byte
   of @var{Stream}, leaving the stream position unaltered.  At end of
   stream, unifies @var{Byte} with the integer -1.").
:- trust pred peek_byte(+stream, ?int) + iso.
:- if(defined(optim_comp)).
:- '$props'(peek_byte/2, [impnat=cbool(peek_byte2)]).
:- else.
:- impl_defined(peek_byte/2).
:- endif.

:- export(peek_byte/1).
:- doc(peek_byte(Byte), "Behaves like @tt{current_input(S),
   peek_byte(S,Byte)}.").
:- trust pred peek_byte(?int) + iso.
:- if(defined(optim_comp)).
:- '$props'(peek_byte/1, [impnat=cbool(peek_byte1)]).
:- else.
:- impl_defined(peek_byte/1).
:- endif.

:- export(put_byte/2).
:- doc(put_byte(Stream, Byte), "Outputs to @var{Stream} the byte
   @var{Byte}.").
:- trust pred put_byte(+stream, +int) + (iso, native).
%:- trust pred put_byte(+stream, +byte) + (iso, native).
:- if(defined(optim_comp)).
:- '$props'(put_byte/2, [impnat=cbool(put_byte2)]).
:- else.
:- impl_defined(put_byte/2).
:- endif.

:- export(put_byte/1).
:- doc(put_byte(Byte), "Behaves like @tt{current_output(S),
   put_byte(S,Byte)}.").
:- trust pred put_byte(+int) + (iso, native, is_det).
%:- trust pred put_byte(+byte) + (iso, native, is_det).
:- if(defined(optim_comp)).
:- '$props'(put_byte/1, [impnat=cbool(put_byte1)]).
:- else.
:- impl_defined(put_byte/1).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Stream properties").

:- export(at_end_of_stream/1).
:- doc(at_end_of_stream(Stream), "@var{Stream} has a stream position
   end-of-stream or past-end-of-stream. This predicate is provided for
   ISO compatibility, but its usage is discouraged, since it cannot
   reliably detect that no more data is available in the stream (e.g.,
   without blocking). Use @pred{peek_byte/2} or @pred{peek_code/2}
   instead.").
% NOTE: Not compatible with other ISO implementations what require
% implicitly peeking stream data
:- trust pred at_end_of_stream(+stream) + iso.
:- if(defined(optim_comp)).
:- '$props'(at_end_of_stream/1, [impnat=cbool(at_end_of_stream1)]).
:- else.
:- impl_defined(at_end_of_stream/1).
:- endif.

:- export(at_end_of_stream/0).
:- doc(at_end_of_stream, "Behaves like @tt{current_input(S),
   at_end_of_stream(S)}.").
:- trust pred at_end_of_stream + iso.
:- if(defined(optim_comp)).
:- '$props'(at_end_of_stream/0, [impnat=cbool(at_end_of_stream0)]).
:- else.
:- impl_defined(at_end_of_stream/0).
:- endif.

% ---------------------------------------------------------------------------
:- doc(section, "Terms Input/Output").

:- export(display/2).
:- doc(display(Stream, Term), "Displays @var{Term} onto
   @var{Stream}.  Lists are output using list notation, the other
   compound terms are output in functional notation.  Similar to
   @tt{write_term(Stream, Term, [ignore_ops(ops)])}, except that 
   curly bracketed notation is not used with @tt{@{@}/1}, and
   the @tt{write_strings} flag is not honored.").
:- trust pred display(+stream,@term) + native.
:- if(defined(optim_comp)).
:- '$props'(display/2, [impnat=cbool(prolog_display2)]).
:- else.
:- impl_defined(display/2).
:- endif.

:- export(display/1).
:- doc(display(Term), "Behaves like @tt{current_output(S),
   display(S,Term)}.").
:- trust pred display(@term) + native.
:- if(defined(optim_comp)).
:- '$props'(display/1, [impnat=cbool(prolog_display)]).
:- else.
:- impl_defined(display/1).
:- endif.

:- export(displayq/2).
:- doc(displayq(Stream, Term), "Similar to @tt{display(Stream, Term)},
   but atoms and functors that can't be read back by @pred{read_term/3}
   are quoted.  Thus, similar to
   @tt{write_term(Stream, Term, [quoted(true), ignore_ops(ops)])}, with the
   same exceptions as @pred{display/2}.").
:- trust pred displayq(+stream,@term).
:- if(defined(optim_comp)).
:- '$props'(displayq/2, [impnat=cbool(prolog_displayq2)]).
:- else.
:- impl_defined(displayq/2).
:- endif.

:- export(displayq/1).
:- doc(displayq(Term), "Behaves like @tt{current_output(S),
   displayq(S,Term)}.").
:- trust pred displayq(@term).
:- if(defined(optim_comp)).
:- '$props'(displayq/1, [impnat=cbool(prolog_displayq)]).
:- else.
:- impl_defined(displayq/1).
:- endif.

:- if(defined(optim_comp)).
:- export('$format_print_float'/3). % internal predicate
:- '$props'('$format_print_float'/3, [impnat=cbool(prolog_format_print_float)]).
%
:- export('$format_print_integer'/3). % internal predicate
:- '$props'('$format_print_integer'/3, [impnat=cbool(prolog_format_print_integer)]).
%
:- export('$display_string'/1). % internal predicate
:- '$props'('$display_string'/1, [impnat=cbool(prolog_display_string)]).
%
:- export('$copy_stdout'/1). % internal predicate
:- '$props'('$copy_stdout'/1, [impnat=cbool(prolog_copy_stdout)]).
:- endif.
