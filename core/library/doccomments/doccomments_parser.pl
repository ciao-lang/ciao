:- module(doccomments_parser, [], [assertions, isomodes, hiord, layout_dcg]).

:- doc(title, "Lightweight documentation parser").
:- doc(subtitle, "Parser of a (wiki) lightweight mark-up language for
   documentation").

:- doc(authors, "Jose F. Morales").

:- doc(module, "This module implements a parser for lightweight
   mark-up syntax for documentation.").

:- use_module(library(read_from_string),
	[read_from_string/2,
	 read_from_string_opts/4]).

:- use_module(library(doccomments/doccomments_syntax)).

% This parser is layout-sensitive. Along with the input character
% string, it also carries the column (and sometimes row) line.

%! @bug Make sure that variables in head are indeed parsed as
%    variables
%
%  @bug Cannot relate the variables in the head and body.  Some
%    possible solutions:
%
%       - a special sentence translation where we can manipulate the
%         mapping between variables and their names.
%       - a low-level 'doc' assertion (e.g., comp property) to specify the
%         variable names (this may be simpler to implement)
%         e.g.
%             %! nat(X): @var{X} is a natural number
%         ===>
%             :- pred nat(X) # '$varstr'("@var{X} is a natural number", ['X'-X])
%
%         It'd require support for '$varstr' in the assertion library / lpdoc.

%! wiki_parse(Cs, Envs):
%    Partially parse the documentation string `Cs` which contains
%    lightweight markup.

:- export(wiki_parse/2).
wiki_parse(Cs, Envs) :-
	layout_new(0, Layout),
	% TODO: Fix Ciao argument order for hiord!
	%sc_lift(parse_front(Layout, Envs0), Cs, Rest),
	sc_lift(([Layout, Envs0] -> 
                 ''(A, B) :- parse_front(Layout, Envs0, A, B)), Cs, Rest),
	( Rest = [] ->
	    true
	; % Could not parse the whole text
	  throw(bug(wiki_parse_leftovers(Rest)))
	),
	rectify_envs(Envs0, Envs).

% Parse a substring (used in the parser itself)
% TODO: This should not be necessary with a good grammar
parse_substring(Layout, Str, Envs) :-
	sc_lift(([Layout, Envs] ->
	         ''(A, B) :- parse_text(Layout, Envs, A, B)), Str, Rest),
	( Rest = [] ->
	    true
	; % Could not parse the whole text
	  throw(bug(parse_substring_leftovers(Rest)))
	).

% ---------------------------------------------------------------------------
% Rectify the environments after parsing. 
%
% This includes some heuristics to group commands and create missing
% environments (e.g., group 'item' into 'itemize' environments).

rectify_envs(Envs, Envs3) :-
	itemize_envs(Envs, Envs2),
	sectionize_envs(Envs2, Envs3).

% Put together contiguous 'item' environments.
itemize_envs([], []).
itemize_envs(Envs, [Itemize|PEnvs]) :-
	take_item_envs(Envs, IEnvs, Rest), \+ IEnvs = [], !,
	( member(env('item_num'(_), _), IEnvs) ->
	    Itemize = env('enumerate', IEnvs)
	; member(env('item'(_), _), IEnvs) ->
	    Itemize = env('description', IEnvs)
	; Itemize = env('itemize', IEnvs)
	),
	itemize_envs(Rest, PEnvs).
itemize_envs([Env0|Envs], [Env|PEnvs]) :-
	itemize_env(Env0, Env),
	itemize_envs(Envs, PEnvs).

itemize_env(env(Cmd, Envs0), env(Cmd, Envs)) :-
	cmd_type(Cmd, docstring),
	Envs0 = [_|_],
	\+ is_string(Envs0),
	!, % The item may contain items
	( itemize_envs(Envs0, Envs) -> true
	; throw(failed_itemize_envs)
	).
itemize_env(Env, Env).

:- use_module(library(lists), [append/3]).

% Put together contiguous text inside 'section-like' environments.
sectionize_envs([], []).
sectionize_envs([env(Cmd,Envs0)|Envs], [Env|PEnvs]) :-
	section_like_env(Cmd),
%
% TODO: This `blank_envs/1` check was introduced to allow section-like
%       and non-section-like behaviour, at the same time. It may not
%       be a good idea after all.
%
%%	blank_envs(Envs0),
	take_text_envs(Envs, SEnvs1, Rest), \+ SEnvs1 = [], !,
	append(Envs0, [env('text', "\n")|SEnvs1], SEnvs),
	Env = env(Cmd, SEnvs),
	sectionize_envs(Rest, PEnvs).
sectionize_envs([Env|Envs], [Env|PEnvs]) :-
	sectionize_envs(Envs, PEnvs).

% Get all 'item' envs
take_item_envs([Env0|Envs0], [Env|Items], Rest) :-
	Env0 = env(Cmd, _),
	is_item_cmd(Cmd),
	!,
	itemize_env(Env0, Env),
	skip_blank_envs(Envs0, Envs),
	take_item_envs(Envs, Items, Rest).
take_item_envs(Envs, [], Envs).

is_item_cmd('item').
is_item_cmd('item'(_)).
is_item_cmd('item_num'(_)).

% Get all 'text' envs (those that are not section_like_env)
take_text_envs([Env|Envs], [Env|SEnvs], Rest) :-
	Env = env(Cmd,_),
	\+ section_like_env(Cmd),
	!,
	take_text_envs(Envs, SEnvs, Rest).
take_text_envs(Envs, [], Envs).

% Skip blank envs
skip_blank_envs([Env|Envs], Rest) :-
	blank_env(Env),
	!,
	skip_blank_envs(Envs, Rest).
skip_blank_envs(Envs, Envs).

blank_envs([]).
blank_envs([Env|Envs]) :- blank_env(Env), blank_envs(Envs).

blank_env(env('text', "\n")) :- !.
blank_env(env('text', Envs)) :- blank_envs(Envs).

% ---------------------------------------------------------------------------
% Layout status for parsing

% layout(Base,     % Base column number
%        LineCol,  % This line begin column number
%        PrevCol)  % Previous non-blank line column number

layout_new(Col, layout(Col, Col, Col)).

% Trick, set a fake, very large BaseCol so that parsing is always
% limited to one line.
layout_oneline(layout(_, LineCol, PrevCol),
	       layout(BaseCol, LineCol, PrevCol)) :-
	infcol(BaseCol).

% Annotate the current start column of the line as the start column of
% the previous line.
layout_mark_prevcol(layout(BaseCol, LineCol, _),
	            layout(BaseCol, LineCol, LineCol)).

% Annotate the current column as a the current start column of the line.
layout_mark_linecol(Col, layout(BaseCol, _, PrevCol),
	                 layout(BaseCol, Col, PrevCol)).

layout_inner_basecol(Col, layout(_, LineCol, PrevCol),
	                  layout(BaseCol, LineCol, PrevCol)) :-
        inner_basecol(Col, BaseCol).

layout_basecol(layout(BaseCol, _, _), BaseCol).
layout_prevcol(layout(_, _, PrevCol), PrevCol).

% ---------------------------------------------------------------------------

% A practically infinite column number.
infcol(0xffffff).

% A base that accepts characeters beyond column `Col`
% (at least 1 character)
inner_basecol(Col, BaseCol) :-
	BaseCol is Col + 1.

outside(Col, BaseCol) :- Col < BaseCol.

% ---------------------------------------------------------------------------

% Given BaseCol, compute the column where code may start
codebase(BaseCol, CodeCol) :-
	code_env_indent(D), CodeCol is BaseCol + D.

% ---------------------------------------------------------------------------
% The real parsing code

% Parse arguments associated to command `Cmd`
% TODO: At his moment only one argument is retrieved (Body)
parse_arg(term, Layout, Term) -->
	parse_indented_term(Layout, Term).
parse_arg(docstring, Layout, Envs) -->
	parse_indented_docstring(Layout, Envs).
parse_arg(docstring_oneline, Layout0, Envs) -->
	{ layout_oneline(Layout0, Layout) },
	parse_indented_docstring(Layout, Envs).

% Parse a docstring whose indentation is greater than `BaseCol`.
parse_indented_docstring(Layout, Envs) -->
	% Blank, no newline
	sc_blank_nonl,
	!,
	% We skip it
	parse_indented_docstring(Layout, Envs).
parse_indented_docstring(_Layout, Envs) -->
	sc_nonblank_loc(0, _Col, Row),
	{ Row >= 2 }, % two or more newlines, exit % TODO: check, not always!
	!,
	% Stop here (do not consume the newlines yet)
	{ Envs = [] }.
parse_indented_docstring(Layout0, Envs) -->
	% Newline
	sc_nl,
	!,
	{ layout_mark_prevcol(Layout0, Layout) },
 	% Allow front commands now
	parse_front(Layout, Envs).
parse_indented_docstring(Layout, Envs) -->
	parse_text(Layout, Envs).

% Parsing text (non-front commands or normal text).
parse_text(Layout, Envs) -->
	% Initialize the accumulator
	parse_text_(Layout, StrHead, StrHead, Envs).

% TODO: Join lines together and build paragraphs?
% TODO: First get the paragraph and then postprocess? or not?
%       at least define... match_paragraph_until (it works in Org)

% parse_text_(Layout,
%             StrHead,     % Accumulator (head)
%             StrTail,     % Accumulator (tail)
%             Envs, Envs0) % Envs

parse_text_(Layout, StrHead, StrTail, Envs) -->
	sc_nonblank_loc(0, Col, Row),
	% next non-blank outside the base BaseCol, exit
	{ Row >= 1,
	  layout_basecol(Layout, BaseCol),
	  outside(Col, BaseCol) },
	!,
	% Stop here (do not consume the newlines yet)
	{ flush_text(StrHead, StrTail, Envs, []) }.
parse_text_(Layout0, StrHead, StrTail, Envs) -->
	% A new line
	sc_nl,
	!,
	% Flush the accumulated text
	{ flush_text(StrHead, StrTail, Envs, Envs1) },
	( % TODO: This should be done in a more simple way...
	  % At least two newlines
	  sc_nonblank_loc(0, _, Row),
	  { Row >= 1 } ->
	    % Flush and parse the next line front.
	    { Envs1 = [env('text', "\n")|Envs0] }
	; % Flush and parse the next line front.
	  { Envs1 = [env('text', " ")|Envs0] } % just a blank
	),
	{ layout_mark_prevcol(Layout0, Layout) },
	parse_front(Layout, Envs0).
parse_text_(Layout, StrHead, StrTail, Envs) -->
	% Hack to avoid parsing of text between {...}
	% TODO: With integration in the LPdoc parser, this would not be necessary
	% TODO: Text inside {...} is not parsed (it may be, depending on the command).
	sc_char(0'{), match_until_nonl(Text, "}"),
	!,
	{ flush_text(StrHead, StrTail, Envs, Envs1) },
	{ Envs1 = [env('text', "{"), env('text', Text), env('text', "}")|Envs0] },
	parse_text(Layout, Envs0).
parse_text_(Layout, StrHead, StrTail, Envs) -->
	% Some binary text decorator (e.g., [[...][...]])
	% TODO: Precompute to make it more efficient
	{ decorator_fxfxf(Begin, Mid, End, Cmd0) },
	sc_str(Begin), match_until_nonl(Text1, Mid), match_until_nonl(Text2, End),
	!,
	{ flush_text(StrHead, StrTail, Envs, Envs1) },
	%
	{ cmd_type(Cmd0, Type0) },
	{ parse_substring_arg(Type0, Layout, Text1, EnvsText1) },
	{ Cmd =.. [Cmd0, EnvsText1] },
	%
	{ cmd_type(Cmd, Type) },
	{ parse_substring_arg(Type, Layout, Text2, EnvsText2) },
	{ Envs1 = [env(Cmd, EnvsText2)|Envs0] },
	%
	parse_text(Layout, Envs0).
parse_text_(Layout, StrHead, StrTail, Envs) -->
	% Some unary text decorator (e.g., *...*)
	% TODO: Precompute to make it more efficient
	{ decorator_fxf(Begin, End, Cmd0) },
	sc_str(Begin), match_until_nonl(Text, End),
	!,
	{ flush_text(StrHead, StrTail, Envs, Envs1) },
	%
	{ ( Cmd0 = code ->
	    % This does not exist yet in LPdoc
	    detect_code_cmd(Text, Cmd)
	; Cmd = Cmd0
	) },
	{ cmd_type(Cmd, Type) },
	{ parse_substring_arg(Type, Layout, Text, EnvsText) },
	{ Envs1 = [env(Cmd, EnvsText)|Envs0] },
	%
	parse_text(Layout, Envs0).
parse_text_(Layout, StrHead, [C|StrTail], Envs) -->
	% A normal character code
	sc_char(C),
	!,
	parse_text_(Layout, StrHead, StrTail, Envs).
parse_text_(_Layout, StrHead, StrTail, Envs) -->
	% Nothing else, stop here
	{ flush_text(StrHead, StrTail, Envs, []) }.

% Parse a command argument that has been already read as raw text.
parse_substring_arg(string, _Layout, Text, Envs) :- !,
	Envs = [env('text', Text)].
parse_substring_arg(docstring, Layout, Text, Envs) :- !,
	parse_substring(Layout, Text, Envs).
parse_substring_arg(Type, _, _, _) :-
	throw(bug(parse_substring_arg(Type))).

% parse_front(Layout, Envs):
%   Parsing the front of the line (no character has been treated
%   before in this line, except blanks).
parse_front(Layout, Envs) -->
	skip_blanks(EmptyLines),
	parse_front_(Layout, EmptyLines, Envs).

% Skip blanks input. EmptyLines is true iff there are empty lines.
skip_blanks(EmptyLines) -->
	% Blank, no newline
	sc_blank_nonl, !, skip_blanks(EmptyLines).
skip_blanks(yes) -->
	% Newline
	sc_nl, !, skip_blanks(_).
skip_blanks(no) --> [].

% (all blanks have been skipped)
parse_front_(Layout, _EmptyLines, Envs) -->
	% Non-blank outside the base BaseCol
	sc_col(Col), { layout_basecol(Layout, BaseCol), outside(Col, BaseCol) },
	!,
	% Stop here
	{ Envs = [] }.
parse_front_(Layout, EmptyLines, Envs) -->
	% Check if this is a code block (based on indentation
	% w.r.t. the previous non-blank line)
	{ EmptyLines = yes },
	sc_col(Col),
	{ layout_prevcol(Layout, PrevCol) },
	{ codebase(PrevCol, CodeCol) },
	{ \+ outside(Col, CodeCol) },
	!,
	{ CodeBaseCol = Col },
	parse_indented_block(CodeBaseCol, CodeBlock),
	{ Envs = [env('verbatim', [env('text', CodeBlock)])|Envs0] },
	%
	parse_front(Layout, Envs0).
parse_front_(Layout0, _EmptyLines, Envs) -->
	% First, try front commands
	sc_col(Col), match_front_cmd(Cmd),
	!,
	{ cmd_type(Cmd, Type) },
	{ Envs = [env(Cmd, NBody)|Envs0] },
	{ layout_mark_linecol(Col, Layout0, Layout1) },
	{ layout_inner_basecol(Col, Layout1, Layout) },
	parse_arg(Type, Layout, NBody),
	%
	parse_front(Layout0, Envs0).
parse_front_(Layout0, _EmptyLines, Envs) -->
	% Else, try non-front commands or normal text
	sc_col(Col),
	{ layout_mark_linecol(Col, Layout0, Layout) },
	parse_text(Layout, Envs).

% Flush the accumulated text (if there is any)
flush_text(StrHead, StrTail, Envs, Envs0) :-
	StrTail = [],
	( StrHead = [] ->
	    Envs = Envs0
	; Envs = [env('text', StrHead)|Envs0]
	).

% Detect the right command for the given program code expression
% TODO: This should be part of LPdoc.
% TODO: Beware of the buggy read_from_string/2!
detect_code_cmd(Text, Cmd) :-
	( read_from_string(Text, TextTerm) ->
	    ( var(TextTerm) -> Cmd = 'var'
	    ; TextTerm = (_/_) -> Cmd = 'pred'
	    ; Cmd = 'tt' % TODO: This should be 'code' (not tt)
	    )
        ; Cmd = 'tt'
        ).

%! match_front_cmd(?Cmd):
%    Match a valid front command `Cmd`.

% A predicate head followed by ':', at column 0
match_front_cmd(Cmd) --> match_docpred(Head, VarNames), !,
	{ Cmd = docpred(Head, VarNames) }.
% A front command
match_front_cmd(Cmd) -->
	sc_char(C), { cmdchar(C) },
	match_cmdname(Cmd0), { atom_codes(Cmd, Cmd0) }, { front_cmd(Cmd) },
	!.
% An item with description (- Desc ::)
match_front_cmd(Cmd) --> sc_str("- "), sc_col(Col), match_until_nonl(Desc, " :: "), !,
	{ layout_new(Col, Layout) } ,
	{ parse_substring_arg(docstring, Layout, Desc, Envs) },
	{ Cmd = 'item'(Envs) }.
% An item (-)
match_front_cmd(Cmd) --> sc_str("- "), !,
	{ Cmd = 'item' }.
% An automatically numbered item (-# )
match_front_cmd(Cmd) --> sc_str("-# "), !, % "Fix font-lock
	{ Cmd = 'item_num'("") }.
% A numbered item (Num.)
match_front_cmd(Cmd) --> match_num(N), sc_char(0'.), !,
	{ Cmd = 'item_num'(N) }.
% A section name
match_front_cmd(Cmd) --> sc_str("* "), !,
	{ Cmd = 'section' }.
% A subsection name
match_front_cmd(Cmd) --> sc_str("** "), !,
	{ Cmd = 'subsection' }.
% A subsubsection name
match_front_cmd(Cmd) --> sc_str("*** "), !,
	{ Cmd = 'subsubsection' }.

%! match_docpred(?Head, ?VarNames):
%    Match a predicate documentation head (@tt{Pred:} or
%    @tt{Pred.}). Fails if no documentation head is recognized.
%
% TODO: This does not allow predicate heads which take more than one line
% TODO: Make this work:
%
%         %! foo(3.4, a:m). Many problems. That is: colon and period.
%
%       Other solution: parse the term and compare the rest (it it is
%       0': or 0'., you are done).

match_docpred(Head, VarNames) -->
	sc_col(Col), { Col = 0 }, % no left margin allowed at all
	match_docpred_(Head0),
	{ read_from_string_opts(Head0, Head, [], variable_names(VarNames)) }.

match_docpred_([]) --> sc_char(0':), !.
match_docpred_([]) --> sc_char(0'.), !.
match_docpred_(_) -->
	% Newline
	sc_nl,
 	!,
	{ fail }.
match_docpred_([C|Head]) -->
	sc_char(C),
	match_docpred_(Head).
	
% Parse an indented block `Block`. The whole block indentation must be
% greater than `BaseCol`. Blank characters on the left of `BaseCol`
% are just ignored.

% TODO: Work on BaseCol (change semantics)
% TODO: implement combination of assertions
% TODO: merge with LPdoc to allow it in .lpdoc

parse_indented_block(BaseCol, Block) -->
	% Newline
	sc_nl,
	!,
	( % Find non-blank characters
	  sc_nonblank_loc(0, Col1, _Row) ->
	    ( { outside(Col1, BaseCol) } ->
		% Non-blanks before base, stop here
		{ Block = [] }
	    ; % All characters before base are blank
	      % Continue with the block
	      { Block = [0'\n|Block0] },
	      parse_indented_block(BaseCol, Block0)
            )
        ; % No more non-blank characters, stop here
	  { Block = [] }
        ).
parse_indented_block(BaseCol, Block) -->	
	sc_col(Col), sc_char(C),
	!,
	( { outside(Col, BaseCol) } ->
	    % Character before base, ignore
	    { Block = Block0 }
	; { Block = [C|Block0] }
	),
	parse_indented_block(BaseCol, Block0).
parse_indented_block(_BaseCol, []) -->
	% Nothing else, finish
	[].

% Parse an indented term
parse_indented_term(Layout, Term) -->
	{ layout_basecol(Layout, BaseCol) },
	parse_indented_block(BaseCol, Block),
	% TODO: Do something with Rest (no garbage should be left)
	( { read_from_string_opts(Block, Term, _Rest, variable_names(_)) } ->
	    []
	; { throw(could_not_parse_term(Block)) }
	).

% ---------------------------------------------------------------------------
% Match a valid identifier for LPdoc command names

% Starting character for commands
cmdchar(0'\\).
cmdchar(0'@).

% Valid characters for names
cmdvalid(C) :- C >= 0'a, C =< 0'z, !.
cmdvalid(C) :- C >= 0'A, C =< 0'Z, !.
cmdvalid(C) :- C >= 0'0, C =< 0'9, !.
cmdvalid(0'_).

%! match_cmdname(?Cmd):
%    Capture from `Cs` a command name. `Rest` is the rest of
%    `Cs`. `Col` and `RestCol` indicate the column numbers.
match_cmdname([C|Cmd]) --> sc_char(C), { cmdvalid(C) }, !, match_cmdname(Cmd).
match_cmdname([]) --> [].

% ---------------------------------------------------------------------------
% Some auxiliary matchers

% Match a number
match_num([C|Num]) --> sc_char(C), { is_digit(C) }, !,
	match_num(Num).
match_num([]) --> [].

% Read in `Str` the text before `End` is found. Fails if the line
% finishes before.
match_until_nonl(_Str, _End) --> sc_char(0'\n), !,
	% Not found in the line
	{ fail }.
match_until_nonl(Str, End) --> sc_str(End), !,
	% Found `Str`, stop here
	{ Str = [] }.
match_until_nonl([C|Str], End) --> sc_char(C), !,
	match_until_nonl(Str, End).
match_until_nonl(_Str, _End) -->
	% Nothing else, so it has been not found
	{ fail }.

% ---------------------------------------------------------------------------

is_digit(C) :- C >= 0'0, C =< 0'9.

% ---------------------------------------------------------------------------

:- export(is_string/1).
% A cheap (but incomplete) check for strings
is_string([]).
is_string([C|_]) :- integer(C).


