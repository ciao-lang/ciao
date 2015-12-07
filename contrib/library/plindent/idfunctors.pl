:- module(idfunctors, [
		remove_nl/2,
		identify_functors/5,
		auto_space_argdescs/2],
	    [assertions, regtypes, hiord, dcg, fsyntax, nativeprops]).

:- use_package(plindent(plindent_decl)).

:- use_module(library(lists)).
:- use_module(plindent(poslastchar)).
:- use_module(plindent(plisettings)).

:- doc(author, "Edison Mera").
:- doc(module, "Functors formatter.").


update_functors_openpar(
	    [functorproc${name => Name, tokentype => TokenType,
		    indentlevel => IndentLevel, arg => Arg, argt => ArgT,
		    pos => Pos1, argdescs => Args, argdescst => ArgsT,
		    parlevel => ParLevel0}|Functors],
	    [functorproc${name => Name, tokentype => TokenType,
		    indentlevel => IndentLevel, arg => Arg, argt => ArgT,
		    pos => Pos1, argdescs => Args, argdescst => ArgsT,
		    parlevel => ParLevel1}|Functors]) -->
	!,
	[],
	{ParLevel1 is ParLevel0 + 1}.

do_update_functors_closepar(% Used when parlevel is 0
	    [
		functorproc${name => Name, tokentype => TokenType,
		    indentlevel => IndentLevel,
		    arg => Arg, argt => [], pos => PosF,
		    argdescs => Argdescs, argdescst => [], parlevel => 1},
		functorproc${name => Name2, tokentype => TokenType2,
		    indentlevel => IndentLevel2, arg => Arg2,
		    argt => [B|Arg2T], pos => Pos2, argdescs => Args2,
		    argdescst => Args2T, parlevel => ParLevel}
		|Functors],
	    [
		functorproc${name => Name2, tokentype => TokenType2,
		    indentlevel => IndentLevel2, arg => Arg2, argt => Arg2T,
		    pos => Pos2, argdescs => Args2, argdescst => Args2T,
		    parlevel => ParLevel}
		|Functors]) -->
	{
	    B = functordesc${name => Name, tokentype => TokenType,
		indentlevel => IndentLevel, arg => Arg, pos => PosF,
		argdescs => Argdescs, parlevel => 0}
	},
	!,
	[].
do_update_functors_closepar(% Used when parlevel is not 0
	    [functorproc${name => Name, tokentype => TokenType,
		    indentlevel => IndentLevel, arg => Arg, argt => ArgT,
		    pos => Pos, argdescs => Args, argdescst => ArgdescsT,
		    parlevel => ParLevel0}|Functors],
	    [functorproc${name => Name, tokentype => TokenType,
		    indentlevel => IndentLevel, arg => Arg, argt => ArgT,
		    pos => Pos, argdescs => Args, argdescst => ArgdescsT,
		    parlevel => ParLevel1}|Functors]) -->
	{ParLevel0 > 0},
	!,
	[],
	{ParLevel1 is ParLevel0 - 1}.

update_functors_par(TokenType, Functors0, Functors) -->
	{is_opener(TokenType)},
	!,
	update_functors_openpar(Functors0, Functors), !.
update_functors_par(closepar, Functors0, Functors) -->
	do_update_functors_closepar(Functors0, Functors),
	!.
update_functors_par(_, Functors, Functors) -->
	[].

update_functors_(TokenType0, Value0, IndentationStyle, Functors0, Functors,
	    Tokens0, Tokens, Pos0, Pos) -->
	update_functors_par(TokenType0, Functors0, Functors1),
	update_functors_end_argument(TokenType0, Value0, IndentationStyle,
	    Functors1, Functors, Tokens0, Tokens, Pos0, Pos).

identify_functors_([Token0|Tokens0], [Token0|Values0], IndentationStyle,
	    IndentLevel0, Pos0, Functors0, Functors) :-
	Token0 = token(TokenType0, Value0),
	pos_last_char(Value0, Pos0, Pos1),
	update_functors_(TokenType0, Value0, IndentationStyle, Functors0,
	    Functors1, Tokens0, Tokens1, Pos1, Pos2, Values0, Values1),
	update_indent(TokenType0, IndentLevel0, IndentLevel1),
	proc_functor_begin(Tokens1, TokenType0, Value0, IndentLevel1, Pos2,
	    Functors2, Functors1),
	identify_functors_(Tokens1, Values1, IndentationStyle, IndentLevel1,
	    Pos2, Functors2, Functors),
	!.
identify_functors_([], [], _, _, _, Functors, Functors) :- !.
identify_functors_([], Values1, IndentationStyle, IndentLevel, Pos0,
	    Functors0, Functors) :-
	do_update_functors_closepar(Functors0, Functors1, Values1, Values),
	identify_functors_([], Values, IndentationStyle, IndentLevel, Pos0,
	    Functors1, Functors).

:- pred identify_functors(Tokens, Values, PliConfig, IndentLevel, Argdescs)
	: (list(Tokens), pliconfig_t(PliConfig), list(IndentLevel))
	=> (list(Values), list(Argdescs)).

identify_functors(Tokens0, Values, PliConfig, IndentLevel, Argdescs) :-
	PliConfig = pliconfig${indentation_style => IndentationStyle},
	identify_functors_(Tokens0, Values, IndentationStyle,
	    IndentLevel, pos(0, 1),
	    [functorproc${
		    name => "program(",
		    tokentype => openfunc,
		    indentlevel => [],
		    arg => Arg,
		    argt => Arg,
		    pos => pos(0, 1),
		    argdescs => Argdescs,
		    argdescst => Argdescs,
		    parlevel => 0}], % _ ).
	    [functorproc${
		    name => "program(",
		    tokentype => openfunc,
		    indentlevel => _,
		    arg => _,
		    argt => [],
		    pos => _,
		    argdescs => _,
		    argdescst => [],
		    parlevel => _}]).

proc_functor_begin([token(TokenType1, Value1)|_], _TokenType0, _Value0,
	    IndentLevel0, Pos0) -->
	{
	    is_opener(TokenType1),
	    !,
	    Name = Value1
	},
	[functorproc${
		name => Name,
		tokentype => TokenType1,
		indentlevel => IndentLevel0,
		arg => Arg,
		argt => Arg,
		pos => Pos0,
		argdescs => Args,
		argdescst => Args,
		parlevel => 0}].
proc_functor_begin(_, _, _, _, _) --> [].

update_functors_end_argument(TokenType0, Value0, IndentationStyle,
	    Functors0, Functors, Tokens0, Tokens, Pos0, Pos) -->
	{
	    bookmark_begin(TokenType0, Value0)
	;
	    TokenType0 \== spaces,
	    Tokens0 = [token(spaces, _),
		token(TokenType2, _)|_],
	    is_end_argument(TokenType2),
	    ( not_auto_tabuled(IndentationStyle, TokenType2) ->
		Bookmark = "" ; true )
	},
	!,
	push_args_and_bookmark(Functors0, Functors, Bookmark, Tokens0, Tokens,
	    Pos0, Pos).
update_functors_end_argument(_, _, _, Functors, Functors, Tokens, Tokens, Pos,
	    Pos) -->
	[].

push_args_and_bookmark([Functor0|Functors], [Functor1|Functors],
	    Bookmark, Tokens0, Tokens, Pos0, Pos) -->
	{push_arg_in_args(Functor0, Functor1, Bookmark, PosB)},
	put_bookmark(Tokens0, Tokens, Bookmark, Pos0, Pos, PosB).

push_arg_in_args(
	    functorproc${name => Name,
		tokentype => TokenType,
		indentlevel => IndentLevel,
		pos => PosF,
		arg => B,
		argt => [],
		argdescs => Args0,
		argdescst => [argdesc${arg => B, pos => PosB,
			bookmark => Bookmark}|Args],
		parlevel => ParLevel},
	    functorproc${name => Name,
		tokentype => TokenType,
		indentlevel => IndentLevel,
		pos => PosF,
		arg => X,
		argt => X,
		argdescs => Args0,
		argdescst => Args,
		parlevel => ParLevel},
	    Bookmark, PosB).

:- pred remove_nl/2 :: list *term +is_det # "This predicate remove the
	new line character from a string. It closes the first argument
	if it is a list not totally instantiated.".

remove_nl([],          []) :- !.
remove_nl([C|String0], String1) :-
	remove_nl_each(C, String1, String),
	remove_nl(String0, String).

remove_nl_each(0'\n, String,     String) :- !.
remove_nl_each(C,    [C|String], String).

put_bookmark([token(spaces, Spaces)|Tokens], Tokens, Bookmark,
	    Pos0, Pos, PosB) -->
	!,
	{
	    append(Spaces, Bookmark, Value),
	    pos_last_char(Spaces, Pos0, Pos),
	    remove_nl(Spaces, Spaces0),
	    pos_last_char(Spaces0, Pos0, PosB)
	},
	[token(spaces, Value)].
put_bookmark(Tokens, Tokens, "", Pos, Pos, Pos) --> [].

auto_space_argdescs([],                   []).
auto_space_argdescs([Argdesc1|Argdescs0], GArgdescs0) :-
	Argdesc1 = argdesc${arg => Functors1, pos => pos(_, LineA1)},
	(
	    Functors1 = % Consider when arg is only one functor
	    [functordesc${name => Name, tokentype => TokenType,
		    indentlevel => IndentLevel, argdescs => Argdescs1,
		    pos => pos(Col, LineF1)}] ->
	    ( LineA1 =:= LineF1 -> true
	    ; list(Functors1, auto_space_functordesc) ),
	    ( select(Argdesc2, Argdescs0, Argdescs01),
		Argdesc2 = argdesc${arg => Functors2, pos => pos(_, LineA2)},
		Functors2 = % Consider when arg is only one functor
		[functordesc${name => Name, tokentype => TokenType,
			indentlevel => IndentLevel, argdescs => Argdescs2,
			pos => pos(Col, LineF2)}],
		% format("LineF1=~w\n", [LineF1]),
		% format("LineA1=~w\n", [LineA1]),
		% format("LineF2=~w\n", [LineF2]),
		% format("LineA2=~w\n", [LineA2]),
		LineF1 \== LineF2, % Not in the same line
		LineA1 + 1 =:= LineF2, % Second clause begins in the next line
		LineA1 - LineF1 =:= LineA2 - LineF2, % Same number of lines
		length(Argdescs1, N1),
		length(Argdescs2, N2),
		N1 =:= N2 % Same number of arguments
	    ->
		Argdescs = [Argdesc2|Argdescs01],
		GArgdescs = [Argdesc1|GArgdescs0]
	    ;
		Argdescs = Argdescs0,
		GArgdescs = [],
		instantiate_bookmarks([Argdesc1|GArgdescs0])
	    )
	;
	    Argdescs = Argdescs0,
	    GArgdescs = [],
	    instantiate_bookmarks([Argdesc1|GArgdescs0])
	),
	auto_space_argdescs(Argdescs, GArgdescs).

auto_space_functordesc(functordesc${argdescs => Argdescs}) :-
	auto_space_argdescs(Argdescs, []).

instantiate_bookmarks(GArgdescs0) :-
	pos_bookmarks(GArgdescs0, _, PosBookmarks),
	!,
	instantiate_pos_bookmarks_each(PosBookmarks).

instantiate_pos_bookmarks_each(PosBookmarks) :-
	initialize_offsets(PosBookmarks, Offsets),
	instantiate_pos_bookmarks_each_(Offsets, PosBookmarks),
	!.
instantiate_pos_bookmarks_each(_).

instantiate_pos_bookmarks_each_(Offsets0, PosBookmarks) :-
	update_offsets(PosBookmarks, Offsets0, Offsets1),
	pos_bookmarks_min_max(PosBookmarks, Offsets1, 0.Inf, Min, 0, Max),
	instantiate_pos_bookmarks(Offsets1, PosBookmarks, Min, Max,
	    Offsets2, PosBookmarks1),
	!,
	instantiate_pos_bookmarks_each_(Offsets2, PosBookmarks1).
instantiate_pos_bookmarks_each_(_, _).

initialize_offsets([], []).
initialize_offsets([[PosBookmark|_]|PosBookmarkss],
	    [offs(0, Pos)|Offsets]) :-
	PosBookmark = pb(Pos, _),
	initialize_offsets(PosBookmarkss, Offsets).

update_offsets([], [], []).
update_offsets(
	    [[PosBookmark|_]|PosBookmarkss],
	    [offs(Offset0, pos(Col0, LastLine0))|Offsets0],
	    [offs(Offset, Pos)|Offsets]) :-
	PosBookmark = pb(Pos, _),
	Pos = pos(_, LastLine),
	(
	    LastLine0 \== LastLine ->
	    % display(pos(Col0-LastLine0) -Pos), nl,
	    Offset is Offset0 + Col0 - 1
	;
	    Offset = Offset0
	),
	update_offsets(PosBookmarkss, Offsets0, Offsets).

pos_bookmarks_min_max([], [], Min, Min, Max, Max).
pos_bookmarks_min_max([[PosBookmark|_]|PosBookmarkss],
	    [offs(OffsetChar, _)|Offsets], Min0, Min, Max0, Max) :-
	PosBookmark = pb(Pos, _),
	Pos = pos(Col0, _),
	Col is OffsetChar + Col0,
	(Col < Min0 -> Min1 = Col ; Min1 = Min0),
	(Col > Max0 -> Max1 = Col ; Max1 = Max0),
	pos_bookmarks_min_max(PosBookmarkss, Offsets, Min1, Min, Max1, Max).

instantiate_pos_bookmarks([], [], _Min, _Max, [], []).
instantiate_pos_bookmarks([Offset0|Offsets0],
	    [[PosBookmark|PosBookmarks]|PosBookmarkss0], Min,
	    Max, [offs(OffsetChar, pos(LastCol0, LastLine0))|Offsets],
	    [PosBookmarks|PosBookmarkss]) :-
	Offset0 = offs(OffsetChar0, pos(LastCol0, LastLine0)),
	PosBookmark = pb(Pos, Bookmark),
	Pos = pos(Col0, _Line0),
	(
	    var(Bookmark) ->
	    Col is OffsetChar0 + Col0,
	    SpaceLength is Max - Col,
	    gen_spaces(SpaceLength, Bookmark, ""),
	    Delta is SpaceLength
	;
	    length(Bookmark, SpaceLength),
	    Delta is SpaceLength
	    % ( LastLine0 =\= Line0 -> Delta is SpaceLength
	    % ; Delta is Max - (OffsetChar0 + Col0) )
	),
	OffsetChar is OffsetChar0 + Delta,
	instantiate_pos_bookmarks(Offsets0, PosBookmarkss0, Min, Max,
	    Offsets, PosBookmarkss).

gen_spaces(0,  Spaces,       Spaces) :- !.
gen_spaces(N0, " "||Spaces0, Spaces) :-
	N0 > 0,
	N is N0 - 1,
	gen_spaces(N, Spaces0, Spaces).

% The use of Scheme is to create the same spaced structure in several
% composed functors, for example:

% funca(aaa(b(c, ddd),    d(e)), e).
% funca(aaa(b(c, ddd, e), d(e)), e).

:- use_module(library(hiordlib), [map/3]).

:- meta_predicate map(?, pred(4), ?, ?, ?).

map([],     _,    []) --> [].
map([A|As], Meta, [B|Bs]) -->
	Meta(A, B),
	map(As, Meta, Bs).

% This works using backtracking but could be optimized

pos_bookmarks(Argdescs, Scheme, PosBookmarkss) :-
	map(Argdescs, pos_bookmark_f(Scheme), PosBookmarkss).

pos_bookmark_f(argdesc${arg => Functors}, Scheme, PosBookmarks) :-
	map(Functors, pos_bookmark_f_, Scheme, PosBookmarks, []).

pos_bookmark_f_(functordesc${argdescs => Argdescs}, Scheme) -->
	map(Argdescs, pos_bookmark_f__, Scheme).

pos_bookmark_f__(argdesc${arg => Functors, pos => Pos,
		bookmark => Bookmark}, Scheme) -->
	(
	    {var(Scheme), Functors \== []} ->
	    (map(Functors, pos_bookmark_f_, Scheme) ; {Scheme = []})
	;
	    (map(Functors, pos_bookmark_f_, Scheme) -> [] ; {Scheme = []})
	),
	[pb(Pos, Bookmark)].
