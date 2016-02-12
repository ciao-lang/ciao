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
