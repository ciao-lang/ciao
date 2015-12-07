:- module(write,[
        write_term/3, write_term/2,
        write_option/1,
        write/2, write/1,
        writeq/2, writeq/1,
	write_list1/1,
        write_canonical/2, write_canonical/1,
        print/2, print/1, printq/2, printq/1,
	portray_clause/2, portray_clause/1,
        numbervars/3, prettyvars/1,
        printable_char/1
        ], 
        [dcg, assertions, nortchecks, nativeprops, isomodes, define_flag]).

:- use_module(library(operators)).
:- use_module(library(sort)).
:- use_module(engine(internals), ['$atom_mode'/2]).
:- use_module(engine(attributes)).

:- doc(title,"Term output").  

:- doc(author,"Richard A. O'Keefe (original version)").
:- doc(author,"Mats Carlsson (changes)").
:- doc(author,"Daniel Cabeza (changes)").
:- doc(author,"Manuel Hermenegildo (changes)").
:- doc(author,"Manuel Carro (changes)").

:- doc(module,"This library provides different predicates for term
           output, additional to the kernel predicates
           @pred{display/1}-@pred{display/2} and
           @tt{displayq/1}-@tt{displayq/2}. All the predicates defined
           in ISO-Prolog are included, plus other traditionally
           provided by Prolog Implementations. Output predicates are
           provided in two versions: one that uses the current output
           stream and other in which the stream is specified
           explicitly, as an additional first argument.").

:- doc(define_flag/3,"Defines flags as follows:
        @includedef{define_flag/3}
        (See @ref{Changing system behaviour and various flags}).

        If flag is @tt{on}, lists which may be written as strings are.").

define_flag(write_strings, [on,off], off).

writeq_quick(Term) :- var(Term), displayq(Term).
writeq_quick(Term) :- atomic(Term), displayq(Term).

write_quick(Term) :- var(Term), display(Term).
write_quick(Term) :- atomic(Term), display(Term).


:- pred write_term(@Stream, ?Term, +OptList) :
   stream * term * list(write_option) +  iso

   # "Outputs the term @var{Term} to the stream @var{Stream}, with the
      list of write-options @var{OptList}. See @pred{write_option/1}
      type for default options.".

write_term(Stream, Term, OptList) :-
        current_output(Curr),
        catch(set_output(Stream),
              error(ErrT, _), throw(error(ErrT,write_term/3-1))),
        write_term_internal(Term, OptList, 3),
        set_output(Curr).


:- pred write_term(?Term, +OptList): term * list(write_option) + iso

   # "Behaves like @tt{current_output(S),
      write_term(S,Term,OptList)}.".

write_term(Term, OptList) :-
        write_term_internal(Term, OptList, 2).

write_term_internal(Term, OptList, N) :-
        Default = options(false,false,false,false,1000000),
        compute_options(OptList, N, Default, 1200, Options, Priority),
        write_out(Term, Options, Priority, 0, 0, '(', 2'100, _).


:- prop write_option(Opt) 
        # "@var{Opt} is a valid write option.".

:- doc(write_option/1, "@var{Opt} is a valid write option which
 affects the predicate @tt{write_term/3} and similar ones. Possible
 write_options are: @begin{itemize} @item @bf{quoted(}@em{bool}@bf{):}
 If @em{bool} is @tt{true}, atoms and functors that can't be read back
 by @pred{read_term/3} are quoted, if it is @tt{false}, each atom and
 functor is written as its name. Default value is @tt{false}.

 @item @bf{ignore_ops(}@em{flag}@bf{):} If @em{flag} is @tt{true},
 each compound term is output in functional notation, if it is
 @tt{ops}, curly bracketed notation and list notation is enabled when
 outputing compound terms, if it is @tt{false}, also operator notation
 is enabled when outputing compound terms. Default value is
 @tt{false}.

 @item @bf{numbervars(}@em{bool}@bf{):} If @em{bool} is
 @tt{true}, a term of the form @tt{'$VAR'(N)} where @tt{N} is an
 integer, is output as a variable name consisting of a capital letter
 possibly followed by an integer, a term of the form @tt{'$VAR'(Atom)}
 where @tt{Atom} is an atom, as this atom (without quotes), and a term
 of the form @tt{'$VAR'(String)} where @tt{String} is a character
 string, as the atom corresponding to this character string.  See
 predicates @tt{numbervars/3} and @tt{prettyvars/1}.  If @em{bool} is
 @tt{false} this cases are not treated in any special way.  Default
 value is @tt{false}.
 
 @item @bf{portrayed(}@em{bool}@bf{):} If @em{bool} is @tt{true}, then 
 call multifile predicates @pred{portray/1} and @pred{portray_attribute/2},
 to provide the user handlers for pretty printing some terms.
 @tt{portray_attribute/2} is called whenever an attributed variable is to be
 printed, @tt{portray/1} is called whenever a non-variable term is to be
 printed.  If either call succeeds, then it is assumed that the term has been
 output, else it is printed as usual.  If @em{bool} is @tt{false}, these
 predicates are not called. Default value is @tt{false}.  This option is 
 set by the toplevel when writing the final values of variables, and by the
 debugging package when writing the goals in the tracing messages.  Thus you
 can vary the forms of these messages if you wish.

 @item @bf{max_depth(}@em{depth}@bf{):} @em{depth} is a positive integer or
 cero. If it is positive, it denotes the depth limit on printing compound
 terms. If it is cero, there is no limit. Default value is @tt{0} (no limit).

 @item @bf{priority(}@em{prio}@bf{):} @em{prio} is an integer between 1 and
 1200. If the term to be printed has higher priority than @em{prio}, it will be
 printed parenthesized.  Default value is 1200 (no term parenthesized).

@end{itemize}.").

write_option(quoted(Qt)) :- boolean(Qt).
write_option(ignore_ops(IO)) :- ignore_ops_flag(IO).
write_option(numbervars(NV)) :- boolean(NV).
write_option(portrayed(Pr)) :- boolean(Pr).
write_option(max_depth(MD)) :- integer(MD), MD >= 0.
write_option(priority(Prio)) :- integer(Prio), Prio >= 1, Prio =< 1200.

compute_options(V, N, _, _, _, _) :- var(V), !,
        throw(error(instantiation_error, write_term/N-N)).
compute_options([], _, Options, Prio, Options, Prio) :- !.
compute_options([Opt|Opts], N, Options1, Prio1, Options, Prio) :- !,
        one_opt(Opt, N, Options1, Prio1, Options2, Prio2),
        compute_options(Opts, N, Options2, Prio2, Options, Prio).
compute_options(Opts, N, _, _, _, _) :-
        throw(error(type_error(list, Opts), write_term/N-N)).

one_opt(V, N, _, _, _, _) :- var(V), !,
        throw(error(instantiation_error, write_term/N-N)).
one_opt(quoted(Qt),     _, options(_ ,IO,NV,Pr,Li), Prio,
                           options(Qt,IO,NV,Pr,Li), Prio) :- boolean(Qt), !.
one_opt(ignore_ops(IO), _, options(Qt,_ ,NV,Pr,Li), Prio,
                           options(Qt,IO,NV,Pr,Li), Prio) :- ignore_ops_flag(IO), !.
one_opt(numbervars(NV), _, options(Qt,IO,_, Pr,Li), Prio,
                           options(Qt,IO,NV,Pr,Li), Prio) :- boolean(NV), !.
one_opt(portrayed(Pr),  _, options(Qt,IO,NV,_, Li), Prio,
                           options(Qt,IO,NV,Pr,Li), Prio) :- boolean(Pr), !.
one_opt(max_depth(MD),  _, options(Qt,IO,NV,Pr,_ ), Prio,
                           options(Qt,IO,NV,Pr,Li), Prio) :-
                           integer(MD), MD >= 0, !,
                           (MD = 0 -> Li = 1000000; Li = MD).
one_opt(priority(Prio), _, Opts, _, Opts, Prio) :-
        integer(Prio), Prio >= 1, Prio =< 1200, !.
one_opt(Opt, N, _, _, _, _) :-
        throw(error(domain_error(write_option, Opt), write_term/N-N)).

:- prop boolean(Bool)
        # "@var{Bool} is either the atom @tt{true} or the atom @tt{false}.".

boolean(true).
boolean(false).

:- prop ignore_ops_flag(Flag)
        # "@var{Flag} is either the atom @tt{true}, the atom
          @tt{false} or the atom @tt{ops}.".

ignore_ops_flag(true).
ignore_ops_flag(ops).
ignore_ops_flag(false).


:- pred write_canonical(@Stream, ?Term) : stream * term + iso
       # "Behaves like @tt{write_term(Stream, Term, [quoted(true),
          ignore_ops(true)])}.  The output of this predicate can
          always be parsed by @pred{read_term/2} even if the term
          contains special characters or if operator declarations have
          changed.".

write_canonical(Stream, Term) :-
        current_output(Curr),
        catch(set_output(Stream),
              error(ErrT, _), throw(error(ErrT,write_canonical/2-1))),
        write_canonical(Term),
        set_output(Curr).


:- pred write_canonical(?Term) : term + iso
        # "Behaves like @tt{current_output(S), write_canonical(S,Term)}.".

write_canonical(Term) :-
        writeq_quick(Term), !.
write_canonical(Term) :-
        Options = options(true,true,false,false,1000000),
        write_out(Term, Options, 1200, 0, 0, '(', 2'100, _).


:- pred print(@Stream, ?Term): stream * term
        # "Behaves like @tt{write_term(Stream, Term,
           [numbervars(true), portrayed(true)])}.".

print(Stream, Term) :-
        current_output(Curr),
        catch(set_output(Stream),
              error(ErrT, _), throw(error(ErrT,print/2-1))),
        print(Term),
        set_output(Curr).


:- pred print(?Term): term
        # "Behaves like @tt{current_output(S), print(S,Term)}.".

print(Term) :-
        Options = options(false,false,true,true,1000000),
        write_out(Term, Options, 1200, 0, 0, '(', 2'100, _).


:- pred printq(@Stream, ?Term): stream * term
        # "Behaves like @tt{write_term(Stream, Term,
           [quoted(true), numbervars(true), portrayed(true)])}.".

printq(Stream, Term) :-
        current_output(Curr),
        catch(set_output(Stream),
              error(ErrT, _), throw(error(ErrT,printq/2-1))),
        printq(Term),
        set_output(Curr).


:- pred printq(?Term): term
        # "Behaves like @tt{current_output(S), printq(S,Term)}.".

printq(Term) :-
        Options = options(true,false,true,true,1000000),
        write_out(Term, Options, 1200, 0, 0, '(', 2'100, _).


:- trust pred write(@Stream, ?Term): stream * term + (iso, is_det)
        # "Behaves like @tt{write_term(Stream, Term, [numbervars(true)])}.".

write(Stream, Term) :-
        current_output(Curr),
        catch(set_output(Stream),
              error(ErrT, _), throw(error(ErrT,write/2-1))),
        write(Term),
        set_output(Curr).


:- trust pred write(?Term): term + (iso, is_det)
        # "Behaves like @tt{current_output(S), write(S,Term)}.".

write(Term) :-
        write_quick(Term), !.
write(Term) :-
        Options = options(false,false,true,false,1000000),
        write_out(Term, Options, 1200, 0, 0, '(', 2'100, _).


:- pred write_list1(+list)
        # "Writes a list to current output one element in each line.".

write_list1([]).
write_list1([H|L]) :- writeq(H), nl, write_list1(L).


:- trust pred writeq(@Stream, ?Term): stream * term + (iso, is_det)
        # "Behaves like @tt{write_term(Stream, Term, [quoted(true),
          numbervars(true)])}.".

writeq(Stream, Term) :-
        current_output(Curr),
        catch(set_output(Stream),
              error(ErrT, _), throw(error(ErrT,writeq/2-1))),
        writeq(Term),
        set_output(Curr).


:- trust pred writeq(?Term): term + (iso, is_det)
        # "Behaves like @tt{current_output(S), writeq(S,Term)}.".

writeq(Term) :-
        writeq_quick(Term), !.
writeq(Term) :-
        Options = options(true,false,true,false,1000000),
        write_out(Term, Options, 1200, 0, 0, '(', 2'100, _).

%   writes a parenthesis if the context demands it.
%   Context = 2'000 for alpha
%   Context = 2'001 for quote
%   Context = 2'010 for other
%   Context = 2'100 for punct

maybe_open_paren(P, Prio, Lpar, '(', _, 2'100) :-
        P > Prio, !,
        display(Lpar).
maybe_open_paren(_, _, Lpar, Lpar, C, C).

maybe_close_paren(P, Prio, _, 2'100) :-
        P > Prio, !,
        display(')').
maybe_close_paren(_, _, C, C).



%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(Ci, Co) :-
        (   Ci\/Co<2'100, Ci#Co<2'010 -> put_code(0' )
        ;   true
        ).

/*
sticky_contexts(alpha, alpha).
sticky_contexts(quote, quote).
sticky_contexts(other, other).
sticky_contexts(alpha, quote).
sticky_contexts(quote, alpha).
*/


:- pred printable_char(+Char): character_code # 
"@var{Char} is the code of a character which can be printed.".

printable_char(V) :- var(V), !, fail.
printable_char(9).  % TAB
printable_char(10). % NEWLINE
printable_char(32). % SPACE
printable_char(B) :-
        integer(B), B > 32, B < 256, code_class(B, C), C > 0. % Not layout

%   write_out(Term, Options, Prio, PrePrio, Depth, Lpar, Ci, Co)
%   writes out a Term given Options
%   at nesting depth Depth
%   in a context of priority Priority (that is, expressions with
%   greater priority must be parenthesized),
%   and prefix operators =< PrePrio must be parenthesized,
%   where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

%% Print hooks

:- multifile portray_attribute/2.

:- pred portray_attribute(Attr, Var) : nonvar * var # "@em{A user
defined predicate.} When an attributed variable @var{Var} is about to
be printed, this predicate receives the variable and its attribute
@var{Attr}.  The predicate should either print something based on
@var{Attr} or @var{Var}, or do nothing and fail. In the latter case,
the default printer (@pred{write/1}) will print the attributed
variable like an unbound variable, e.g. @tt{_673}.".

:- multifile portray/1.


:- trust pred portray(T) : term(T) => term(T)
   # "@em{A user defined predicate.} This should either print the @var{Term}
      and succeed, or do nothing and fail.  In the latter case, the default
      printer (@tt{write/1}) will print the @var{Term}.".

% this clause is for attributed variables -- DMCAI -- ATTRVARS
%
write_out(Term, Options,  _, _, _, _, _, 2'000) :-
        get_attribute(Term,M),
        Options = options(_,_,_,true,_),
        ( \+ portray_attribute(M,Term) ->
              fail               % portray_attribute might bind variables
        ; true
        ),
        !.
write_out(Term, _, _, _, _, _, Ci, 2'000) :-
        var(Term), !,
        maybe_space(Ci, 2'000),
        displayq(Term).
write_out(_, Options, _, _, Depth, _, Ci, 2'010) :-
        Options = options(_,_,_,_,Limit),
        Depth >= Limit, !,
        maybe_space(Ci, 2'010),
        display(...).
write_out('$VAR'(N), Options, _, _, _, _, Ci, Co) :-
        Options = options(_,_,true,_,_),
        write_VAR(N, Ci, Co), !.
write_out(Term, Options, _, _, _, _, _, 2'000) :-
        Options = options(_,_,_,true,_),
        (   \+ portray(Term) ->
            fail                 % portray might bind variables
        ;   true
        ), !.
write_out(Atom, Options, _, PrePrio, _, Lpar, _, 2'100) :-
        atom(Atom),
        current_prefixop(Atom, P, _),
        P =< PrePrio, !,
        display(Lpar),
        Options = options(Quote,_,_,_,_),
        write_atom(Quote, Atom, 2'100, _),
        put_code(0')).
write_out(Atom, Options, _, _, _, _, Ci, Co) :-
        atom(Atom), !,
        Options = options(Quote,_,_,_,_),
        write_atom(Quote, Atom, Ci, Co).
write_out(N, _, _, _, _, _, Ci, 2'000) :-
        number(N), !,
        ( ( N < 0 ; N == -0.0 ) -> maybe_space(Ci, 2'010)
			% We are using -0.0 because such number
			% exists in IEEE 754 specification
        ;   maybe_space(Ci, 2'000)
        ),
        displayq(N).
write_out(Term, Options, _, _, Depth, _, Ci, 2'100) :-
        Options = options(Quote,true,_,_,_), % Ignore lists and operators
        functor(Term, Atom, Arity), !,
        write_atom(Quote, Atom, Ci, _),
        Depth1 is Depth+1,
        write_args(0, Arity, Term, Options, Depth1).
% Handle {...}, lists and operators
write_out({Term}, Options, _, _, Depth, _, _, 2'100) :- !,
        put_code(0'{),
        Depth1 is Depth+1,
        write_out(Term, Options, 1200, 0, Depth1, '(', 2'100, _),
        put_code(0'}).
write_out([Char|Tail], Options, _, _, Depth, _, _, Co) :-
        current_prolog_flag(write_strings, on),
        printable_char(Char), !,
        put_code(0'"),  % print characters after '"'
        put_string_code(Char),
        Depth1 is Depth+1,
        write_string_tail(Tail, Options, Depth1, Co).
write_out([Head|Tail], Options, _, _, Depth, _, _, 2'100) :- !,
        put_code(0'[),
        Depth1 is Depth+1,
        write_out(Head, Options, 999, 0, Depth1, '(', 2'100, _),
        write_tail(Tail, Options, Depth1).
write_out(Term, Options, _, _, Depth, _, Ci, 2'100) :-
        Options = options(Quote,ops,_,_,_), % Ignore operators
        functor(Term, Atom, Arity), !,
        write_atom(Quote, Atom, Ci, _),
        Depth1 is Depth+1,
        write_args(0, Arity, Term, Options, Depth1).
write_out((A,B), Options, Prio, _, Depth, Lpar, Ci, Co) :- !,
        %  This clause stops writeq quoting commas.
        Depth1 is Depth+1,
        maybe_open_paren(1000, Prio, Lpar, Lpar1, Ci, C1),
        write_out(A, Options, 999, 0, Depth1, Lpar1, C1, _),
        put_code(0',),
        write_out(B, Options, 1000, 1000, Depth1, '(', 2'100, C2),
        maybe_close_paren(1000, Prio, C2, Co).
write_out(Term, Options, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
        functor(Term, F, N),
        Depth1 is Depth+1,
        Options = options(Quote,_,_,_,_),
        write_out_(N, F, Term, Quote, Options, Prio, PrePrio, Depth1, Lpar, Ci, Co).

write_out_(1, F, Term, Quote, Options, Prio, _, Depth, Lpar, Ci, Co) :-
        current_postfixop(F, P, O), !,
        (current_infixop(F, _, _, _) -> O1=1200; O1=O),
        maybe_open_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
        arg(1, Term, A),
        write_out(A, Options, P, 1200, Depth, Lpar1, C1, C2),
        write_atom(Quote, F, C2, C3),
        maybe_close_paren(O1, Prio, C3, Co).
write_out_(1, F, Term, Quote, Options, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
        current_prefixop(F, O, P),
        arg(1, Term, A),
        (number(A) -> F \== - ; true), !,
        (PrePrio=1200 -> O1 is P+1; O1=O),      % for "fy X yf" etc. cases
        maybe_open_paren(O1, Prio, Lpar, _, Ci, C1),
        write_atom(Quote, F, C1, C2),
        write_out(A, Options, P, P, Depth, ' (', C2, C3),
        maybe_close_paren(O1, Prio, C3, Co).
write_out_(2, F, Term, Quote, Options, Prio, PrePrio, Depth, Lpar, Ci, Co) :-
        current_infixop(F, P, O, Q), !,
        (PrePrio=1200 -> O1 is Q+1; O1=O),      % for "U xfy X yf" etc. cases
        maybe_open_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
        arg(1, Term, A),
        write_out(A, Options, P, 1200, Depth, Lpar1, C1, C2),
        ( F = '|' ->
          write_atom(false, '|', C2, C3)
        ;
          write_atom(Quote, F, C2, C3)
        ),
        arg(2, Term, B),
        write_out(B, Options, Q, Q, Depth, '(', C3, C4),
        maybe_close_paren(O1, Prio, C4, Co).
write_out_(N, F, Term, Quote, Options, _, _, Depth, _, Ci, 2'100) :-
        write_atom(Quote, F, Ci, _),
        write_args(0, N, Term, Options, Depth).

write_VAR(N, Ci, 2'000) :-
        integer(N), N >= 0, !,
        maybe_space(Ci, 2'000),
        Letter is N mod 26 + 0'A,
        put_code(Letter),
        (   N>=26 ->
            Rest is N//26, displayq(Rest)
        ;   true
        ).
write_VAR(Atom, Ci, Co) :-
        atom(Atom), !,
        '$atom_mode'(Atom, Co),
        maybe_space(Ci, Co),
        display(Atom).
%% Added the case in which the name is a string. MH
write_VAR(String, Ci, Co) :-
        nonvar(String),
        % This type test is incomplete!
        String = [X|_],
        integer(X),
        atom_codes(Atom,String), !,
        '$atom_mode'(Atom, Co),
        maybe_space(Ci, Co),
        display(Atom).

write_atom(false, Atom, Ci, Co) :-
        '$atom_mode'(Atom, Co),
        maybe_space(Ci, Co),
        display(Atom).
write_atom(true, Atom, Ci, Co) :-
        '$atom_mode'(Atom, Co),
        maybe_space(Ci, Co),
        displayq(Atom).


%   write_args(DoneSoFar, Arity, Term, Options, Depth)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in SynStyle, LexStyle, given that DoneSoFar have already been written.

write_args(0, _, _, Options, Depth) :-
        Options = options(_,_,_,_,Limit),
        Depth >= Limit, !,
        put_code(0'(), display(...),    put_code(0')).
write_args(N, N, _, _, _) :- !,
        put_code(0')).
write_args(I, N, Term, Options, Depth) :-
        write_sep(I),
        J is I+1,
        arg(J, Term, A),
        write_out(A, Options, 999, 0, Depth, '(', 2'100, _),
        write_args(J, N, Term, Options, Depth).

write_sep(0) :- !, put_code(0'().
write_sep(_) :- put_code(0',).



%   write_tail(Tail, Options, Depth)
%   writes the tail of a list given Options, Depth.

write_tail(Var, _, _) :-                        %  |var]
        var(Var), !,
        put_code(0'|),
        displayq(Var),
        put_code(0']).
write_tail([], _, _) :- !,                      %  ]
        put_code(0']).
write_tail(_, Options, Depth) :-
        Options = options(_,_,_,_,Limit),
        Depth >= Limit, !,
        put_code(0'|),
        display(...),
        put_code(0']).
write_tail([Head|Tail], Options, Depth) :- !, %  ,Head tail
        put_code(0',),
        write_out(Head, Options, 999, 0, Depth, '(', 2'100, _),
        Depth1 is Depth+1,
        write_tail(Tail, Options, Depth1).
write_tail(Other, Options, Depth) :-    %  |junk]
        put_code(0'|),
        write_out(Other, Options, 999, 0, Depth, '(', 2'100, _),
        put_code(0']).

write_string_tail(Var, _, _, 2'000) :-
        var(Var), !,
        put_code(0'"),
        put_code(0'|),
        put_code(0'|),
        displayq(Var).
write_string_tail([], _, _, 2'100) :- !,
        put_code(0'").
write_string_tail(_, Options, Depth, 2'010) :-
        Options = options(_,_,_,_,Limit),
        Depth >= Limit, !,
        put_code(0'"),  % end string with '"'
        put_code(0'|),
        put_code(0'|),
        display(...).
write_string_tail([Char|Tail], Options, Depth, Co) :-
        printable_char(Char), !,
        put_string_code(Char),
        Depth1 is Depth+1,
        write_string_tail(Tail, Options, Depth1, Co).
write_string_tail(Other, Options, Depth, Co) :-
        put_code(0'"),  % end string with '"'
        put_code(0'|),
        put_code(0'|),
        write_out(Other, Options, 999, 0, Depth, '(', 2'100, Co).

put_string_code(0'") :- !, display('""').
put_string_code(0'\\) :- !, display('\\\\').
put_string_code(C) :- put_code(C).

:- export(write_attribute/1).

%% --- DTM: Note: if portray_attribute use some indexing (==) with 
%% the variable, then portray_attribute could misbehave when printing on toplevel.

write_attribute( Var ) :-
	get_attribute( Var , Attr ),
        ( \+ portray_attribute( Attr, Var ) ->
          % portray_attribute might bind variables
	  displayq( Attr )
        ; true
        ).



/* portraying clauses */


:- pred prettyvars(?Term): term
        # "Similar to @tt{numbervars(Term,0,_)}, except that singleton
 variables in @var{Term} are unified with @tt{'$VAR'('_')}, so that when the
 resulting term is output with a write option @tt{numbervars(true)}, in the
 place of singleton variables @tt{_} is written. This predicate is used by
 @pred{portray_clause/2}.".

prettyvars(Term) :-
        collect_vars(Term, Vars0, []),
        keysort(Vars0, Vars),
        pretty_vars(Vars, 0).

collect_vars(Var) -->
        {var(Var)}, !, [Var-[]].
collect_vars([X|Xs]) --> !,
        collect_vars(X),
        collect_vars(Xs).
collect_vars(X) -->
        {functor(X, _, A)},
        collect_vars_(0, A, X).

collect_vars_(A, A, _) --> !.
collect_vars_(A0, A, X) -->
        {A1 is A0+1},
        {arg(A1, X, X1)},
        collect_vars(X1),
        collect_vars_(A1, A, X).

pretty_vars([], _).
pretty_vars([X,Y|Xs], N0) :-
        X==Y, !,
        X='$VAR'(N0)-[],
        N is N0+1,
        pretty_vars_(Xs, X, N).
pretty_vars(['$VAR'('_')-[]|Xs], N0) :-
        pretty_vars(Xs, N0).

pretty_vars_([X|Xs], Y, N0) :-
        X==Y, !,
        pretty_vars_(Xs, Y, N0).
pretty_vars_(Xs, _, N0) :-
        pretty_vars(Xs, N0).


:- pred portray_clause(?Clause): term
        # "Behaves like @tt{current_output(S), portray_clause(S,Term)}.". 

% This must be careful not to bind any variables in Clause.
portray_clause(Clause) :-
        prettyvars(Clause),
        portray_clause1(Clause),
        fail.
portray_clause(_).


:- pred portray_clause(@Stream, ?Clause): stream * term
        # "Outputs the clause @var{Clause} onto @var{Stream}, pretty printing
 its variables and using indentation, including a period at the end. This
 predicate is used by @tt{listing/0}.". 


portray_clause(Stream, Clause) :-
        current_output(Curr),
        set_output(Stream),
        portray_clause(Clause),
        set_output(Curr).

portray_clause1(:-(Command)) :-
        functor(Command, Key, 1),
        current_op(_, fx, Key), !,
        arg(1, Command, Body),
        'list clauses'(Body, :-(Key), 8, Co),
        write_fullstop(Co).
portray_clause1((Pred:-Body)) :- !,
        write_out(Pred, options(true,false,true,false,1000000), 1199, 1200, -1, '(', 2'100, Ci), % writeq
        (   Body=true -> write_fullstop(Ci)
        ;   'list clauses'(Body, 0, 8, Co),
            write_fullstop(Co)
        ).
portray_clause1((Pred-->Body)) :- !,
        write_out(Pred, options(true,false,true,false,1000000), 1199, 1200, -1, '(', 2'100, _), % writeq
        'list clauses'(Body, 2, 8, Co),
        write_fullstop(Co).
portray_clause1(Pred) :-
        write_out(Pred, options(true,false,true,false,1000000), 1200, 0, -1, '(', 2'100, Ci), % writeq
        write_fullstop(Ci).

write_fullstop(Ci) :-
        maybe_space(Ci, 2'010),
        put_code(0'.), nl.


'list clauses'((A,B), L, D, Co) :- !,
        'list clauses'(A, L, D, _),
        'list clauses'(B, 1, D, Co).
'list clauses'((A;B), L, D, 2'100) :- !,
        'list magic'(L, D),
        'list disj 1'(A, 3, D),
        'list disj 2'(B, D).
'list clauses'((A->B), L, D, 2'100) :- !,
        'list magic'(L, D),
        E is D+4,
        'list clauses'(A, 3, E, _),
        'list clauses'(B, 5, E, _),
        nl, tab(D),
        put_code(0')).
'list clauses'(!, 0, _, 2'100) :- !,
        display(' :- !').
'list clauses'(!, 1, _, 2'100) :- !,
        display(', !').
'list clauses'(!, 2, _, 2'100) :- !,
        display(' --> !').
'list clauses'(Goal, L, D, Co) :-
        'list magic'(L, D),
        write_out(Goal, options(true,false,true,false,1000000), 999, 0, -1, '(', 2'100, Co). % writeq


'list magic'(0, D) :-
        display(' :-'),
        nl, tab(D).
'list magic'(1, D) :-
        put_code(0',),
        nl, tab(D).
'list magic'(2, D) :-
        display(' -->'),
        nl, tab(D).
'list magic'(3, _) :-
        display('(   ').
'list magic'(4, _) :-
        display(';   ').
'list magic'(5, D) :-
        display(' ->'),
        nl, tab(D).
'list magic'(:-(Key), D) :-
        display(':- '),
        displayq(Key),
        nl, tab(D).

'list disj 2'((A;B), D) :- !,
        'list disj 1'(A, 4, D),
        'list disj 2'(B, D).
'list disj 2'(Conj, D) :-
        'list disj 1'(Conj, 4, D),
        put_code(0')).

'list disj 1'((A->B), L, D) :- !,
        E is D+4,
        'list clauses'(A, L, E, _),
        'list clauses'(B, 5, E, _),
        nl, tab(D).
'list disj 1'(A, L, D) :-
        E is D+4,
        'list clauses'(A, L, E, _),
        nl, tab(D).


:- pred numbervars(?Term, +N, ?M): term * int * term => term * int * int 
        # "Unifies each of the variables in term @var{Term} with a term
 of the form @tt{'$VAR'(I)} where @tt{I} is an integer from @var{N}
 onwards. @var{M} is unified with the last integer used plus 1. If the
 resulting term is output with a write option @tt{numbervars(true)}, in
 the place of the variables in the original term will be printed a
 variable name consisting of a capital letter possibly followed by an
 integer. When @var{N} is 0 you will get the variable names A, B, ...,
 Z, A1, B1, etc.".

% It's too expensive to support cyclic structures.
numbervars(X, N0, N) :-
        ( integer(N0) ->
                numbervars1(X, N0, N)
        ; var(N0) ->
                throw(error(instantiation_error, numbervars/3-2))
        ; throw(error(type_error(integer, N0), numbervars/3-2))
        ).

numbervars1(X, N0, N) :- var(X), !, X='$VAR'(N0), N is N0+1.
numbervars1(X, N0, N) :- atomic(X), !, N0=N.
numbervars1([X|Xs], N0, N) :- !,
        numbervars1(X, N0, N1),
        numbervars1(Xs, N1, N).
numbervars1(X, N0, N) :-
        functor(X, _, A),
        numbervars1_(0, A, X, N0, N).

numbervars1_(A, A, _, N0, N) :- !, N0=N.
numbervars1_(A0, A, X, N0, N) :-
        A1 is A0+1,
        arg(A1, X, X1),
        numbervars1(X1, N0, N1),
        numbervars1_(A1, A, X, N1, N).
