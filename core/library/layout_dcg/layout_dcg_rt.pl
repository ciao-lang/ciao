:- module(layout_dcg_rt, [], [assertions, hiord, dcg]).

:- doc(title, "Layout-sensitive parsing with DCGs").
% (for parsing layout-sensitive text)
:- doc(author, "Jose F. Morales").

:- doc(bug, "Tabs are not handled correctly. Use `untabify/2` before
   parsing text.").

:- use_module(library(lists), [append/3, length/2]).

:- export(sc_lift/3).
:- meta_predicate sc_lift(pred(2), ?, ?).
% Lift a DCG predicate to a 'sc' layout-DCG predicate. This allow the
% encoding of strings with column numbers. We assume that the starting
% column number is 0.

sc_lift(Goal, Xs, Xs0) :-
    Goal(sc(Xs, 0), sc(Xs0, _)).

:- export(sc_empty/2).
% The stream is empty
sc_empty(X,X) :- X = sc([], _).

:- export(sc_col/3).
% The current column number is `Col`
sc_col(Col,X,X) :- X = sc(_, Col).

:- export(sc_str/3).
% Match the `Str` string (`Str` cannot contain newlines)
sc_str(Str, sc(Cs, Col), sc(Rest, RestCol)) :-
    % (we assume that the spine of the Str list is instantiated
    % and that it does not contain newlines)
    append(Str, Rest, Cs), !, 
    length(Str, L),
    RestCol is Col + L.

% TODO: Tabs are not really correctly handled.

:- export(sc_char/3).
% Pick a character `C`.
% If a newline is found, the column number is reset to 0.
% Else, the column number is update accordingly.

sc_char(C, sc([C|Cs], Col0), sc(Cs, Col)) :-
    sc_next_col(C, Col0, Col).

sc_next_col(0'\n, _, Col) :- !, % new line
    Col = 0.
sc_next_col(0'\t, Col0, Col) :- !, % tab % TODO: Incorrect
    tabsize(Tab), Col is Col0 + Tab.
sc_next_col(_, Col0, Col) :- !, % space or normal char
    Col is Col0 + 1.

:- export(sc_nl/2).
%! sc_nl:
%    Pick a new line character.
sc_nl(S0, S) :-
    sc_char(0'\n, S0, S).

:- export(sc_blank_nonl/2).
%! sc_blank_nonl:
%    Pick a character, that must be a blank character.
sc_blank_nonl(S0, S) :-
    sc_char(C, S0, S),
    ( C = 0'   -> true
    ; C = 0'\t -> true
    ).

:- export(sc_blank/4).
%! sc_blank(Row0, Row):
%    Pick a character, that must be a blank character.
sc_blank(Row0, Row, S0, S) :-
    sc_char(C, S0, S),
    ( C = 0'\n -> % new line
        Row is Row0 + 1
    ; C = 0'  -> Row = Row0 % space
    ; C = 0'\t -> Row = Row0 % tab
    ).

:- export(sc_nonblank_loc/5).
%! nonblank_loc(Row0, Col, Row):
%    Assuming that the current row is `Row0`, peek the location of
%    the next non-blank character (column `Col` and row `Row`).
sc_nonblank_loc(Row0, Col, Row, S, S) :-
    % (do not update the stream)
    sc_nonblank_loc_(Row0, Col, Row, S, _).

sc_nonblank_loc_(Row0, Col, Row) -->
    % A blank
    sc_blank(Row0, Row1),
    !,
    sc_nonblank_loc_(Row1, Col, Row).
sc_nonblank_loc_(Row, Col, Row) -->
    % A non-blank
    sc_col(Col), sc_char(_).

% ---------------------------------------------------------------------------

:- export(tabsize/1).
% TODO: We assume 8 characters per tab, this may not be always true
tabsize(8). % Size of tab

% A filter that removes all tabs in the correct way. Otherwise column
%       numbers could be incorrect. That is, if this line starts at
%       column 0, 'foo' is at column 8 (and not 10).  "aa\tfoo" ===>
%       "aa foo" This can be implemented in blank_inc, but it will
%       make matching with blanks much harder (think about it).

:- export(untabify/3).
%! untabify(Column, Cs0, Cs): Cs is the Cs0 where tabs are replaced by
%    spaces. We assume that the starting column of Cs0 is Column.
untabify(Col, Cs0, Cs) :-
    untabify_(Cs0, Col, Cs).

untabify_([], _, []).
untabify_([C|Cs0], Col0, Cs) :-
    ( C = 0'\t ->
        Col0i is Col0 + 1, Cs = [0' |Cs0i],
        fill_tabstop(Col0i, Col1, Cs0i, Cs1)
    ; C = 0'\n ->
        Col1 = 0, Cs = [C|Cs1]
    ; Col1 is Col0 + 1, Cs = [C|Cs1]
    ),
    untabify_(Cs0, Col1, Cs1).

fill_tabstop(Col0, Col, Cs, Cs0) :-
    % We are in a tabstop, do nothing
    tabsize(N),
    0 is Col0 mod N,
    !,
    Col = Col0, Cs = Cs0.
fill_tabstop(Col0, Col, Cs, Cs0) :-
    Cs = [0' |Cs1],
    Col1 is Col0 + 1,
    fill_tabstop(Col1, Col, Cs1, Cs0).

% ---------------------------------------------------------------------------

:- export(tidy_blanks/3).
% Tidy blanks (replace tabs by whitespaces and remove left margin)
tidy_blanks(Col, Cs0, Cs) :-
    untabify(Col, Cs0, Cs1),
    remove_margin(Cs1, Cs).

% Identify the number of unnecessary blank characters on the left
% margin and remove them (up to `~max_margin` characters).
remove_margin(Cs0, Cs) :-
    detect_margin_size(Cs0, M),
    cut_left(Cs0, M, Cs).

% (string must be always at the begin of line)
detect_margin_size(Cs, M) :-
    Max = 0xffffff, % infinite for this purpose
    detect_margin_size_(Cs, Max, M).

detect_margin_size_([], M, M) :- !.
detect_margin_size_(Cs, M0, M) :-
    blanks_number(Cs, N, Cs2), \+ match_eol_or_end(Cs2), !,
    ( N < M0 -> M1 = N ; M1 = M0 ),
    skip_line(Cs2, Cs3),
    detect_margin_size_(Cs3, M1, M).
detect_margin_size_(Cs, M0, M) :-
    skip_line(Cs, Cs1),
    detect_margin_size_(Cs1, M0, M).

skip_line([], []).
skip_line([0'\n|Cs0], Cs) :- !, Cs = Cs0.
skip_line([_|Cs0], Cs) :- skip_line(Cs0, Cs).

% Remove the largest sequence of consecutive blanks from `Cs`, obtain
% the rest in `Cs2`. `N` is the number of blanks removed.
blanks_number(Cs, N, Cs2) :-
    blanks_number_(Cs, 0, N, Cs2).

blanks_number_([C|Cs], I, N, Cs2) :- blank_col_inc(C, I, I1), !,
    blanks_number_(Cs, I1, N, Cs2).
blanks_number_(Cs, I, I, Cs).

blank_col_inc(0'\t, Col0, Col) :-
    tabsize(Tab),
    Col is Col0 + Tab.
blank_col_inc(0' , Col0, Col) :- Col is Col0 + 1. % space

% Cut `M` characters from the left of all lines of `Cs`
cut_left(Cs0, M, Cs) :-
    skip_n(M, Cs0, Cs1),
    cut_left_(Cs1, M, Cs).

cut_left_([], _M, []).
cut_left_([0'\n|Cs0], M, [0'\n|Cs]) :- !,
    cut_left(Cs0, M, Cs).
cut_left_([C|Cs0], M, [C|Cs]) :-
    cut_left_(Cs0, M, Cs).

% Remove N elements (or the whole line, if it is shorter) from the
% first line of `Cs`. Obtain the rest in `Cs2`.
skip_n(_, Cs0, Cs) :- match_eol_or_end(Cs0), !,
    Cs = Cs0.
skip_n(I, [_|Cs0], Cs) :- I > 0, !,
    I1 is I - 1,
    skip_n(I1, Cs0, Cs).
skip_n(_, Cs, Cs).

match_eol_or_end([]).
match_eol_or_end([0'\n|_]).

%% :- use_module(library(lists), [reverse/2]).
%% 
%% % Remove surrounding blanks (spaces and newlines)
%% tidy_comment(Text0, Text) :-
%%      remove_blank(Text0, Text1),
%%      reverse(Text1, Text2),
%%      remove_blank(Text2, Text3),
%%      reverse(Text3, Text).
%% 
%% % remove spaces (newlines and blanks)
%% remove_blank([X|Xs0], Xs) :- blank(X), !, remove_blank(Xs0, Xs).
%% remove_blank(Xs, Xs).

%! detect_code_cmd(Text, Cmd):
%    Detect the appropriate documentation command `Cmd` (variable,
%    predicate, other code, etc.) for the code `Text`.

