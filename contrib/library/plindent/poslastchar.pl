:- module(poslastchar, [pos_last_char/3], [assertions]).

:- doc(author, "Edison Mera").

:- doc(module, "Predicate to get the position of the last char in
	a line.").

pos_last_char(Chars, Pos, Pos) :-
	var(Chars), % this is approximated, because Chars could change later
	!.
pos_last_char([],           Pos,  Pos).
pos_last_char([Char|Chars], Pos0, Pos) :-
	pos_last_char_each(Char, Pos0, Pos1),
	!,
	pos_last_char(Chars, Pos1, Pos).

pos_last_char_each(0'\n, pos(_, Line0), pos(0, Line1)) :-
	Line1 is Line0 + 1.
pos_last_char_each(0'\t, pos(Col0, Line0), pos(Col1, Line0)) :-
	Col1 is (Col0 // 8 + 1) * 8.
pos_last_char_each(_, pos(Col0, Line0), pos(Col1, Line0)) :-
	Col1 is Col0 + 1.
