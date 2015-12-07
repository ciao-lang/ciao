:- module(format_to_string, 
	[format_to_string/3],
	[dcg,assertions,isomodes]).

% Naive implementation of format_to_string that uses strings instead
% of writing codes to a file and reading the file.
%
% This code was written to check how fast can a good implementation of
% format_to_string could be. Do not use for serious purposes (see TODO
% list).
%
% Author: Jose F. Morales

% TODO:
%   Use string streams or other way to share the implementation
%   of format/2 and write/1.
%
%   This code rewrites part of write/1 to output to a string:
%     - no variable names are given
%     - ~k ~p not implemented right
%     - atoms with ' are not quoted properly

:- use_module(library(lists)).
:- use_module(library(write), [printable_char/1]).

:- use_module(engine(internals), ['$atom_mode'/2]).
:- use_module(library(operators)).

%% FOR TEMPORARILY PARTIALLY DOCUMENTING:
:- use_module(library(assertions/doc_props)).

% ------------------------------------------------------------------------

:- prop format_control(C) + (regtype, doc_incomplete) 
   # "@var{C} is an atom or string describing how the arguments should
      be formatted. If it is an atom it will be converted into a
      string with @tt{name/2}.".

format_control(C) :- string(C).
format_control(C) :- atm(C).

% format(+Control, +Arguments)
% format(+Stream, +Control, +Arguments)
% Stream Stream
% atom or list of chars Control corresponds roughly to the first argument of
%				the C stdio function printf().
% list or atom Arguments	corresponds to the rest of the printf()
%				arguments
%

:- pred format_to_string(Format, Arguments, String) 
   : (format_control(Format), list(Arguments)) => string(String)
   # "Print @var{Arguments} onto current string @var{String} according
      to format @var{Format}. This predicate is similar to the format/2,
      but the result is stored in a string.".

% :- use_module(library(format), [format/3]).
% format_to_string(A, B, C) :-
% %	format(user_error, "Frmtting: [[~s ~w ~w]]~n", [A, B, C]),
% 	format_to_string_(A, B, C),
% 	format:format_to_string(A, B, C2),
% 	( C == C2 -> true
% 	; format(user_error, "Frmt: [[~s]]~nMine: [[~s]]~nOthr: [[~s]]~n", [A, C, C2])
% 	).
% %	format(user_error, "done: [[~s ~w ~w]]~n", [A, B, C]).

% :- use_module(library(format), [format_to_string/3]).
% format_to_string(A,B,C) :- format:format_to_string(A,B,C).

format_to_string(Control, _, []) :-
        var(Control), !,
        throw(error(instantiation_error, format/2-1)).
format_to_string(Control, Arguments, Os) :- format1(Control, Arguments, Os), !.
format_to_string(Control, Arguments, _Os) :-
	throw(error(invalid_arguments(format(Control, Arguments)), format_to_string/3)).

format1(Control, Arguments, Os) :-
	(   atom(Control) -> atom_codes(Control, ControlList)
	;   ControlList=Control
	),
	(   ArgumentList=Arguments
	;   ArgumentList=[Arguments]
	),
	fmt_parse(ArgumentList, SpecList, ControlList, []), !,
	fmt_print(SpecList, 0, 0' , Os).

:- push_prolog_flag(multi_arity_warnings, off).
% exactly like format:fmt_parse/4
fmt_parse([], []) --> [].
fmt_parse(Args, Specs) --> [0'~, C1], !,
	fmt_parse(C1, Args, Specs, 0, D, D).
fmt_parse(Args, Specs) --> [0'\\, 0'c, 0'\n], !,
	fmt_parse(Args, Specs).
fmt_parse(Args, [I|Specs]) --> [I],
	{integer(I)},
	fmt_parse(Args, Specs).

fmt_parse(C, Args, Specs, Sofar, _, D) --> {C>=0'0, C=<0'9}, !,
	{N is 10*Sofar+C-0'0},
	[C1], fmt_parse(C1, Args, Specs, N, N, D).
fmt_parse(0'*, [N|Args], Specs, _, _, D) -->
	{integer(N)},
	[C1], fmt_parse(C1, Args, Specs, 0, N, D).
fmt_parse(0'~, Args, [0'~|Specs], _, 1, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'n, Args, [spec(0'c, 0'\n, N)|Specs], _, N, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'N, Args, [settab(0,_,_,0)|Specs], _, 1, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'|, Args, [Spec|Specs], _, N, current) -->
	(   {current=N} ->
	    {Spec=settab(0,_,Tab,Tab)}
	;   {Spec=settab(N,_,_,N)}
	),
	fmt_parse(Args, Specs).
fmt_parse(0'+, Args, [settab(N,Tab,_,Tab)|Specs], _, N, 8) -->
	fmt_parse(Args, Specs).
fmt_parse(0't, Args, [fill(N)|Specs], _, N, 0' ) --> % faking
	fmt_parse(Args, Specs).
fmt_parse(0'`, Args, [fill(Fill)|Specs], 0, _, _) -->
	[Fill, 0't],
	fmt_parse(Args, Specs).
fmt_parse(0'i, [_|Args], Specs, _, 1, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'a, [A|Args], [spec(0'a, A, 1)|Specs], _, 1, 1) -->
	{atom(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'c, [A|Args], [spec(0'c, A, N)|Specs], _, N, 1) -->
	{integer(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'k, [A|Args], [spec(0'k, A, 1)|Specs], _, 1, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'p, [A|Args], [spec(0'p, A, 1)|Specs], _, 1, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'q, [A|Args], [spec(0'q, A, 1)|Specs], _, 1, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'w, [A|Args], [spec(0'w, A, 1)|Specs], _, 1, 1) -->
	fmt_parse(Args, Specs).
fmt_parse(0'e, [A|Args], [spec(0'e, V, N)|Specs], _, N, 6) -->
	{V is float(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'E, [A|Args], [spec(0'E, V, N)|Specs], _, N, 6) -->
	{V is float(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'f, [A|Args], [spec(0'f, V, N)|Specs], _, N, 6) -->
	{V is float(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'g, [A|Args], [spec(0'g, V, N)|Specs], _, N, 6) -->
	{V is float(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'G, [A|Args], [spec(0'G, V, N)|Specs], _, N, 6) -->
	{V is float(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'd, [A|Args], [spec(0'd, V, N)|Specs], _, N, 0) -->
	{V is integer(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'D, [A|Args], [spec(0'D, V, N)|Specs], _, N, 0) -->
	{V is integer(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'r, [A|Args], [spec(0'r, V, N)|Specs], _, N, 8) -->
	{V is integer(A)},
	fmt_parse(Args, Specs).
fmt_parse(0'R, [A|Args], [spec(0'R, V, N)|Specs], _, N, 8) -->
	{V is integer(A)},
	fmt_parse(Args, Specs).
fmt_parse(0's, [A|Args], [spec(0's, A, N)|Specs], _, N, Len) -->
	{is_ascii_list(A, 0, Len)},
	fmt_parse(Args, Specs).
:- pop_prolog_flag(multi_arity_warnings).

is_ascii_list(X, _, _) :- var(X), !, fail.
is_ascii_list([], N, N).
is_ascii_list([X|Xs], N0, N) :-
	N1 is N0+1,
	integer(X),
	is_ascii_list(Xs, N1, N).

:- push_prolog_flag(multi_arity_warnings, off).
% like format:fmt_print/4 but uses a string instead of a stream
fmt_print([], _, _, []).
fmt_print([X|Xs], Tab, Fill, Os) :- fmt_print(X, Xs, Tab, Fill, Os).

fmt_print(settab(Arg,Tab0PlusArg,Pos,Tab), Xs, Tab0, Fill, Os) :- !,
	Tab0PlusArg is Tab0+Arg,
	fake_line_position(Os, Pos),
	(   Pos>Tab ->
	    write_nl(Os, Os1),
	    putn(Tab, Fill, Os1, Os2)
	;   Skip is Tab-Pos,
	    putn(Skip, Fill, Os, Os2)
	),
	fmt_print(Xs, Tab, Fill, Os2).
fmt_print(fill(Fill), Xs, Tab, _, Os) :- !,
	fmt_print(Xs, Tab, Fill, Os).
fmt_print(spec(X,A,N), Xs, Tab, Fill, Os) :- !,
	fake_line_count(Os, Lc0),
	fmt_pr(X, A, N, Os, Os0),
	fake_line_count(Os0, Lc),
	fmt_print(Lc0, Lc, Xs, Tab, Fill, Os0).
fmt_print(0'\n, Xs, _, _, Os) :- !,
	write_nl(Os, Os1),
	fmt_print(Xs, 0, 0' , Os1).
fmt_print(C, Xs, Tab, Fill, Os) :-
	Char is integer(C),
	write_char(Char, Os, Os0),
	fmt_print(Xs, Tab, Fill, Os0).

write_nl(Os, Os0) :- write_char(0'\n, Os, Os0).

write_char(C, [C|Os], Os).

% We do not have this in the string
fake_line_position(_Os, 0).
fake_line_count(_Os, 0).

fmt_print(Lc, Lc, Xs, Tab, Fill, Os) :- !,
	fmt_print(Xs, Tab, Fill, Os).
fmt_print(_, _, Xs, _, _, Os) :- !,
	fmt_print(Xs, 0, 0' , Os).
:- pop_prolog_flag(multi_arity_warnings).

% atom
fmt_pr(0'a, Arg, _, Os, Os0) :- str_write(Arg, Os, Os0).
% term
fmt_pr(0'k, Arg, _, Os, Os0) :- str_write(Arg, Os, Os0).
% term
fmt_pr(0'p, Arg, _, Os, Os0) :- str_write(Arg, Os, Os0).
% term
fmt_pr(0'q, Arg, _, Os, Os0) :- str_writeq(Arg, Os, Os0).
% term
fmt_pr(0'w, Arg, _, Os, Os0) :- str_write(Arg, Os, Os0).
% character
fmt_pr(0'c, Arg, Number, Os, Os0) :-
	putn(Number, Arg, Os, Os0).
fmt_pr(0'e, Arg, _Number, Os, Os0) :- 
	write_number(Arg, Os, Os0).
fmt_pr(0'E, Arg, _Number, Os, Os0) :-
	write_number(Arg, Os, Os0).
fmt_pr(0'f, Arg, _Number, Os, Os0) :-
	write_number(Arg, Os, Os0).
fmt_pr(0'g, Arg, _Number, Os, Os0) :-
	write_number(Arg, Os, Os0).
fmt_pr(0'G, Arg, _Number, Os, Os0) :-
	write_number(Arg, Os, Os0).
fmt_pr(0'd, Arg, _Number, Os, Os0) :-
	write_number(Arg, Os, Os0).
fmt_pr(0'D, Arg, _Number, Os, Os0) :-
	write_number(Arg, Os, Os0).
fmt_pr(0'r, Arg, _Number, Os, Os0) :-
	write_number(Arg, 16, Os, Os0).
fmt_pr(0'R, Arg, _Number, Os, Os0) :-
	write_number(Arg, -16, Os, Os0).
% string
fmt_pr(0's, Arg, Number, Os, Os0) :-
	putn_list(Number, Arg, Os, Os0).

putn(0, _, Os, Os) :- !.
putn(N, C, Os, Os0) :-
	N>0, N1 is N-1,
	Char is integer(C),
	write_char(Char, Os, Os1),
	putn(N1, C, Os1, Os0).

putn_list(0, _, Os, Os) :- !.
putn_list(N, [], Os, Os0) :- !,
	N1 is N-1,
	write_char(0' , Os, Os1),
	putn_list(N1, [], Os1, Os0).
putn_list(N, [C|Chars], Os, Os0) :-
	N1 is N-1,
	Char is integer(C),
	write_char(Char, Os, Os1),
	putn_list(N1, Chars, Os1, Os0).

str_write(Term, Os, Os0) :-
        Options = options(false,false,true,false,1000000),
        str_write_out(Term, Options, 1200, 0, 0, '(', 2'100, _, Os, Os0).

str_writeq(Term, Os, Os0) :-
        Options = options(true,false,true,false,1000000),
        str_write_out(Term, Options, 1200, 0, 0, '(', 2'100, _, Os, Os0).

str_write_out(Term, _, _, _, _, _, Ci, 2'000) -->
        { var(Term) }, !,
        str_maybe_space(Ci, 2'000),
        str_displayq(Term).
str_write_out(Atom, Options, _, PrePrio, _, Lpar, _, 2'100) -->
        { atom(Atom) },
        { current_prefixop(Atom, P, _) },
        { P =< PrePrio }, !,
        str_display(Lpar),
        { Options = options(Quote,_,_,_,_) },
        str_write_atom(Quote, Atom, 2'100, _),
        [0')].
str_write_out(Atom, Options, _, _, _, _, Ci, Co) -->
        { atom(Atom) }, !,
        { Options = options(Quote,_,_,_,_) },
        str_write_atom(Quote, Atom, Ci, Co).
str_write_out(N, _, _, _, _, _, Ci, 2'000) -->
        { number(N) }, !,
        ( { ( N < 0 ; N == -0.0 ) } -> str_maybe_space(Ci, 2'010)
			% We are using -0.0 because such number
			% exists in IEEE 754 specification
        ;   str_maybe_space(Ci, 2'000)
        ),
        str_displayq(N).
str_write_out(Term, Options, _, _, Depth, _, Ci, 2'100) -->
        { Options = options(Quote,true,_,_,_) }, % Ignore lists and operators
        { functor(Term, Atom, Arity) }, !,
        str_write_atom(Quote, Atom, Ci, _),
        { Depth1 is Depth+1 },
        str_write_args(0, Arity, Term, Options, Depth1).
% Handle {...}, lists and operators
str_write_out({Term}, Options, _, _, Depth, _, _, 2'100) --> !,
        [0'{],
        { Depth1 is Depth+1 },
        str_write_out(Term, Options, 1200, 0, Depth1, '(', 2'100, _),
        [0'}].
str_write_out([Char|Tail], Options, _, _, Depth, _, _, Co) -->
        { current_prolog_flag(write_strings, on) },
        { printable_char(Char) }, !,
        [0'"],  % print characters after '"'
        str_put_string_code(Char),
        { Depth1 is Depth+1 },
        str_write_string_tail(Tail, Options, Depth1, Co).
str_write_out([Head|Tail], Options, _, _, Depth, _, _, 2'100) --> !,
        [0'[],
        { Depth1 is Depth+1 },
        str_write_out(Head, Options, 999, 0, Depth1, '(', 2'100, _),
        str_write_tail(Tail, Options, Depth1).
str_write_out(Term, Options, _, _, Depth, _, Ci, 2'100) -->
        { Options = options(Quote,ops,_,_,_) }, % Ignore operators
        { functor(Term, Atom, Arity) }, !,
        str_write_atom(Quote, Atom, Ci, _),
        { Depth1 is Depth+1 },
        str_write_args(0, Arity, Term, Options, Depth1).
str_write_out((A,B), Options, Prio, _, Depth, Lpar, Ci, Co) --> !,
        %  This clause stops writeq quoting commas.
        { Depth1 is Depth+1 },
        str_maybe_open_paren(1000, Prio, Lpar, Lpar1, Ci, C1),
        str_write_out(A, Options, 999, 0, Depth1, Lpar1, C1, _),
        [0',],
        str_write_out(B, Options, 1000, 1000, Depth1, '(', 2'100, C2),
        str_maybe_close_paren(1000, Prio, C2, Co).
str_write_out(Term, Options, Prio, PrePrio, Depth, Lpar, Ci, Co) -->
        { functor(Term, F, N) },
        { Depth1 is Depth+1 },
        { Options = options(Quote,_,_,_,_) },
        str_write_out_(N, F, Term, Quote, Options, Prio, PrePrio, Depth1, Lpar, Ci, Co).

str_write_out_(1, F, Term, Quote, Options, Prio, _, Depth, Lpar, Ci, Co) -->
        { current_postfixop(F, P, O) }, !,
        { (current_infixop(F, _, _, _) -> O1=1200; O1=O) },
        str_maybe_open_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
        { arg(1, Term, A) },
        str_write_out(A, Options, P, 1200, Depth, Lpar1, C1, C2),
        str_write_atom(Quote, F, C2, C3),
        str_maybe_close_paren(O1, Prio, C3, Co).
str_write_out_(1, F, Term, Quote, Options, Prio, PrePrio, Depth, Lpar, Ci, Co) -->
        { current_prefixop(F, O, P) },
        { arg(1, Term, A) },
        { (number(A) -> F \== - ; true) }, !,
        { (PrePrio=1200 -> O1 is P+1; O1=O) },      % for "fy X yf" etc. cases
        str_maybe_open_paren(O1, Prio, Lpar, _, Ci, C1),
        str_write_atom(Quote, F, C1, C2),
        str_write_out(A, Options, P, P, Depth, ' (', C2, C3),
        str_maybe_close_paren(O1, Prio, C3, Co).
str_write_out_(2, F, Term, Quote, Options, Prio, PrePrio, Depth, Lpar, Ci, Co) -->
        { current_infixop(F, P, O, Q) }, !,
        { (PrePrio=1200 -> O1 is Q+1; O1=O) },      % for "U xfy X yf" etc. cases
        str_maybe_open_paren(O1, Prio, Lpar, Lpar1, Ci, C1),
        { arg(1, Term, A) },
        str_write_out(A, Options, P, 1200, Depth, Lpar1, C1, C2),
        ( { F = '|' } ->
          str_write_atom(false, '|', C2, C3)
        ;
          str_write_atom(Quote, F, C2, C3)
        ),
        { arg(2, Term, B) },
        str_write_out(B, Options, Q, Q, Depth, '(', C3, C4),
        str_maybe_close_paren(O1, Prio, C4, Co).
str_write_out_(N, F, Term, Quote, Options, _, _, Depth, _, Ci, 2'100) -->
        str_write_atom(Quote, F, Ci, _),
        str_write_args(0, N, Term, Options, Depth).

str_write_atom(false, Atom, Ci, Co) -->
        { '$atom_mode'(Atom, Co) },
        str_maybe_space(Ci, Co),
        str_display(Atom).
str_write_atom(true, Atom, Ci, Co) -->
        { '$atom_mode'(Atom, Co) },
        str_maybe_space(Ci, Co),
        str_displayq(Atom).

str_write_args(0, _, _, Options, Depth) -->
        { Options = options(_,_,_,_,Limit) },
        { Depth >= Limit }, !,
        [0'(], str_display(...), [0')].
str_write_args(N, N, _, _, _) --> !,
        [0')].
str_write_args(I, N, Term, Options, Depth) -->
        str_write_sep(I),
        { J is I+1 },
        { arg(J, Term, A) },
        str_write_out(A, Options, 999, 0, Depth, '(', 2'100, _),
        str_write_args(J, N, Term, Options, Depth).

str_write_sep(0) --> !, [0'(].
str_write_sep(_) --> [0',].

str_write_tail(Var, _, _) -->                        %  |var]
        { var(Var) }, !,
        [0'|],
        str_displayq(Var),
        [0']].
str_write_tail([], _, _) --> !,                      %  ]
        [0']].
str_write_tail(_, Options, Depth) -->
        { Options = options(_,_,_,_,Limit) },
        { Depth >= Limit }, !,
        [0'|],
        str_display(...),
        [0']].
str_write_tail([Head|Tail], Options, Depth) --> !, %  ,Head tail
        [0',],
        str_write_out(Head, Options, 999, 0, Depth, '(', 2'100, _),
        { Depth1 is Depth+1 },
        str_write_tail(Tail, Options, Depth1).
str_write_tail(Other, Options, Depth) -->    %  |junk]
        [0'|],
        str_write_out(Other, Options, 999, 0, Depth, '(', 2'100, _),
        [0']].

str_write_string_tail(Var, _, _, 2'000) -->
        { var(Var) }, !,
        [0'"],
        [0'|],
        [0'|],
        str_displayq(Var).
str_write_string_tail([], _, _, 2'100) -->
        [0'"].
str_write_string_tail(_, Options, Depth, 2'010) -->
        { Options = options(_,_,_,_,Limit) },
        { Depth >= Limit }, !,
        [0'"],  % end string with '"'
        [0'|],
        [0'|],
        "...".
str_write_string_tail([Char|Tail], Options, Depth, Co) -->
        { printable_char(Char) }, !,
        str_put_string_code(Char),
        { Depth1 is Depth+1 },
        str_write_string_tail(Tail, Options, Depth1, Co).
str_write_string_tail(Other, Options, Depth, Co) -->
        [0'"],  % end string with '"'
        [0'|],
        [0'|],
        str_write_out(Other, Options, 999, 0, Depth, '(', 2'100, Co).

% incomplete
str_displayq(Var) --> { var(Var) }, !, "<VAR>".
str_displayq(X) --> { atom(X) }, !,
        { atom_codes(X, Codes) },
	( { '$atom_mode'(X, 1) } ->
	    "'", emit(Codes), "'"
	; emit(Codes)
	).
str_displayq(X) --> { number(X) }, !, { number_codes(X, Codes) }, emit(Codes).

% incomplete
str_display(Var) --> { var(Var) }, !, "<VAR>".
str_display(X) --> { atom(X) }, !, { atom_codes(X, Codes) }, emit(Codes).
str_display(X) --> { number(X) }, !, { number_codes(X, Codes) }, emit(Codes).

emit([]) --> [].
emit([X|Xs]) --> [X], emit(Xs).

str_put_string_code(0'") --> !, [0'", 0'"].
str_put_string_code(0'\\) --> !, [0'\\, 0'\\].
str_put_string_code(C) --> [C].

str_maybe_open_paren(P, Prio, Lpar, '(', _, 2'100) -->
        { P > Prio }, !,
        str_display(Lpar).
str_maybe_open_paren(_, _, Lpar, Lpar, C, C) --> [].

str_maybe_close_paren(P, Prio, _, 2'100) -->
        { P > Prio }, !,
        str_display(')').
str_maybe_close_paren(_, _, C, C) --> [].

str_maybe_space(Ci, Co) -->
        ( { Ci\/Co<2'100, Ci#Co<2'010 } -> [0' ]
        ; []
        ).

:- push_prolog_flag(multi_arity_warnings, off).
write_number(A, Os, Os0) :-
	number_codes(A, Codes),
	append(Codes, Os0, Os).

write_number(A, Radix, Os, Os0) :-
	( Radix > 0 ->
	    number_codes(A, Radix, Codes)
	; Radix1 is -Radix,
	  number_codes(A, Radix1, Codes0),
	  uppercase(Codes0, Codes)
	),
	append(Codes, Os0, Os).
:- pop_prolog_flag(multi_arity_warnings).

uppercase([], []).
uppercase([X|Xs], [Y|Ys]) :- uppercase_2(X, Y), uppercase(Xs, Ys).

uppercase_2(X0, X) :- X0 >= 0'a, X0 =< 0'z, !,
	X is X0 + 0'A - 0'a.
uppercase_2(X, X).
