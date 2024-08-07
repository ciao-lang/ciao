:- module(markdown_parser, [], [assertions, isomodes, hiord, layout_dcg]).

:- doc(title, "Parser for LPdoc-flavored markdown").

:- doc(authors, "Jose F. Morales").

:- doc(module, "This module implements a parser for the LPdoc-flavored
   markdown syntax (a lightweight mark-up syntax for
   documentation).").

:- use_module(library(read_from_string),
    [read_from_string/2,
     read_from_string_opts/4]).
:- use_module(library(lists), [member/2, append/3]).

:- use_module(library(markdown/markdown_syntax)).

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
%
%  @bug Make sure that verbatim/codeblocks work fine in lists items
%
%  @bug Treat language in ```lang ... ``` blocks

%! markdown_parse(Cs, Envs):
%    Partially parse the documentation string `Cs` which contains
%    lightweight markup.

:- export(markdown_parse/2).
markdown_parse(Cs, Envs) :-
    layout_new(0, Layout),
    sc_lift(parse_start(Layout, Envs0), Cs, Rest),
    ( Rest = [] ->
        true
    ; % Could not parse the whole text
      throw(bug(parse_leftovers(Rest), markdown_parse/2))
    ),
    rectify_envs(Envs0, Envs).

% Parse a substring (used in the parser itself)
% TODO: This should not be necessary with a good grammar
parse_substring(Layout, Str, Envs) :-
    sc_lift(parse_text(Layout, Envs), Str, Rest),
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

% Put together contiguous text inside 'section-like' environments.
sectionize_envs([], []).
sectionize_envs([env(Cmd,Envs0)|Envs], [Env|PEnvs]) :-
    section_like_env(Cmd),
%
% TODO: This `blank_envs/1` check was introduced to allow section-like
%       and non-section-like behaviour, at the same time. It may not
%       be a good idea after all.
%
%%      blank_envs(Envs0),
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
    parse_front2(Layout, Envs).
parse_indented_docstring(Layout, Envs) -->
    parse_text(Layout, Envs).

% Parsing text (non-front commands or normal text).
parse_text(Layout, Envs) -->
    % Initialize the accumulator
    parse_text_(Layout, 0, StrHead, StrHead, Envs).

% TODO: Join lines together and build paragraphs?
% TODO: First get the paragraph and then postprocess? or not?
%       at least define... match_paragraph_until (it works in Org)

% parse_text_(Layout,
%             PrevC,       % Previous char (0 if none)
%             StrHead,     % Accumulator (head)
%             StrTail,     % Accumulator (tail)
%             Envs, Envs0) % Envs

parse_text_(Layout, _PrevC, StrHead, StrTail, Envs) -->
    sc_nonblank_loc(0, Col, Row),
    % next non-blank outside the base BaseCol, exit
    { Row >= 1,
      layout_basecol(Layout, BaseCol),
      outside(Col, BaseCol) },
    !,
    % Stop here (do not consume the newlines yet)
    { flush_text(StrHead, StrTail, Envs, []) }.
parse_text_(Layout0, _PrevC, StrHead, StrTail, Envs) -->
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
    ; skip_blanks(_), sc_empty -> % No more characters
        { Envs1 = Envs0 }
    ; % Flush and parse the next line front.
      { Envs1 = [env('text', " ")|Envs0] } % just a blank
    ),
    { layout_mark_prevcol(Layout0, Layout) },
    parse_front2(Layout, Envs0).
parse_text_(Layout, _PrevC, StrHead, StrTail, Envs) -->
    % Hack to avoid parsing of text between {...}
    % TODO: With integration in the LPdoc parser, this would not be necessary
    % TODO: Text inside {...} is not parsed (it may be, depending on the command).
    sc_char(0'{), match_until_nonl(Text, "}"),
    !,
    { flush_text(StrHead, StrTail, Envs, Envs1) },
    { Envs1 = [env('text', "{"), env('text', Text), env('text', "}")|Envs0] },
    parse_text(Layout, Envs0).
parse_text_(Layout, _PrevC, StrHead, [C,C2,C3|StrTail], Envs) -->
    % Keep @`{ as a command
    sc_char(C), { cmdchar(C) },
    sc_char(C2), { C2 = 0'` },
    sc_char(C3), { C3 = 0'{ },
    !,
    parse_text_(Layout, C3, StrHead, StrTail, Envs).
parse_text_(Layout, _PrevC, StrHead, StrTail, Envs) -->
    % Escape markdown
    sc_char(C), { cmdchar(C) },
    sc_char(C2), { reserved_char(C2) },
    !,
    { flush_text(StrHead, StrTail, Envs, Envs1) },
    { Envs1 = [env('text', [C2])|Envs0] },
    parse_text(Layout, Envs0).
parse_text_(Layout, _PrevC, StrHead, [C,C|StrTail], Envs) -->
    % Two cmdchar, keep them
    sc_char(C), { cmdchar(C) },
    sc_char(C),
    !,
    parse_text_(Layout, C, StrHead, StrTail, Envs).
parse_text_(Layout, _PrevC, StrHead, StrTail, Envs) -->
    % Markup verbatim
    % TODO: better support for codeblock in markup? (not markdown) note that verbatim still allows commands inside!
    sc_char(C), { cmdchar(C) },
    sc_str("begin{verbatim}"),
    !,
    { flush_text(StrHead, StrTail, Envs, Envs1) },
    pick_until_endcmd(CodeBlock, "verbatim"), % TODO: Naive; merge with autodoc_parser automata instead
    skip_blanks(_),
    sc_char(C),
    sc_str("end{verbatim}"),
    { Envs1 = [env(codeblock("text"), CodeBlock)|Envs0] },
    parse_text(Layout, Envs0).
parse_text_(Layout, _PrevC, StrHead, StrTail, Envs) -->
    % Some binary text decorator (e.g., [[...][...]])
    % TODO: Precompute to make it more efficient
    { decorator_fxfxf(Begin, Mid, End, Cmd0) },
    sc_str(Begin),
    { cmd_type(Cmd0, Type0) },
    { Cmd1 =.. [Cmd0, EnvsText1] },
    { cmd_type(Cmd1, Type) },
    match_substring_arg(Type0, Text1, Mid),
    match_substring_arg(Type, Text2, End),
    !,
    { flush_text(StrHead, StrTail, Envs, Envs1) },
    %
    { parse_substring_arg(Type0, Layout, Text1, EnvsText1) },
    { parse_substring_arg(Type, Layout, Text2, EnvsText2) },
    { Env0 = env(Cmd1, EnvsText2) },
    %
    { fix_env(Env0, Env) },
    { Envs1 = [Env|Envs0] },
    parse_text(Layout, Envs0).
parse_text_(Layout, PrevC, StrHead, StrTail, Envs) -->
    % Some unary text decorator (e.g., *...*)
    % TODO: Precompute to make it more efficient
    { decorator_fxf(Begin, End, Cmd, Edge) },
    { valid_decorated_edge(Edge, PrevC) },
    { cmd_type(Cmd, Type) },
    sc_str(Begin), match_substring_arg(Type, Text, End),
    sc_peek(NextC), { valid_decorated_edge(Edge, NextC) },
    { valid_decorated_text(Text) }, % not empty
    !,
    { flush_text(StrHead, StrTail, Envs, Envs1) },
    %
    { parse_substring_arg(Type, Layout, Text, EnvsText) },
    { Env0 = env(Cmd, EnvsText) },
    %
    { fix_env(Env0, Env) },
    { Envs1 = [Env|Envs0] },
    parse_text(Layout, Envs0).
parse_text_(Layout, _PrevC, StrHead, [C|StrTail], Envs) -->
    % Other character code
    sc_char(C),
    !,
    parse_text_(Layout, C, StrHead, StrTail, Envs).
parse_text_(_Layout, _PrevC, StrHead, StrTail, Envs) -->
    % Nothing else, stop here
    { flush_text(StrHead, StrTail, Envs, []) }.

% Edges: 'any' for anything, 'noalpha' to avoid alphanum/1
valid_decorated_edge(any, _).
valid_decorated_edge(noalpha, C) :- \+ alphanum(C).

% Decorated text cannot be empty or be surrounded by blanks.
valid_decorated_text(Text) :-
    Text = [A|_],
    \+ is_blank_or_nl(A),
    append(_,[B],Text), !,
    \+ is_blank_or_nl(B).

% Alphanumeric % TODO: extend with valid unicode for code identifiers?
alphanum(C) :- C >= 0'a, C >= 0'z, !.
alphanum(C) :- C >= 0'A, C >= 0'Z, !.
alphanum(C) :- C >= 0'0, C >= 0'9, !.
alphanum(0'_).

is_blank_or_nl(0' ).
is_blank_or_nl(0'\t).
is_blank_or_nl(0'\n).
                                                      
% Match command argument (finishing at End)
match_substring_arg(multiline, Text, End) -->
    match_multiline(Text, End).
match_substring_arg(string, Text, End) -->
    match_until_nonl(Text, End).
match_substring_arg(docstring, Text, End) -->
%    match_until_nonl(Text, End).
    match_multiline(Text, End).

% Parse a command argument that has been already read as raw text.
parse_substring_arg(multiline, _Layout, Text, Envs) :- !,
    Envs = [env('text', Text)].
parse_substring_arg(string, _Layout, Text, Envs) :- !,
    Envs = [env('text', Text)].
parse_substring_arg(docstring, Layout, Text, Envs) :- !,
    parse_substring(Layout, Text, Envs).
parse_substring_arg(Type, _, _, _) :-
    throw(bug(parse_substring_arg(Type))).

% parse_start(Layout, Envs):
%   Start parsing (assumes blanks or nothing above)
parse_start(Layout, Envs) -->
    skip_blanks(_),
    parse_front1(Layout, yes, Envs).

% Skip blanks input. EmptyLines is true iff there are empty lines.
skip_blanks(EmptyLines) -->
    % Blank, no newline
    sc_blank_nonl, !, skip_blanks(EmptyLines).
skip_blanks(yes) -->
    % Newline
    sc_nl, !, skip_blanks(_).
skip_blanks(no) --> [].

% Skip blanks, no newline
skip_blanks_nonl --> sc_blank_nonl, !, skip_blanks_nonl.
skip_blanks_nonl --> [].

% (all blanks have been skipped)
parse_front1(Layout, _EmptyAbove, Envs) -->
    % Non-blank outside the base BaseCol
    sc_col(Col), { layout_basecol(Layout, BaseCol), outside(Col, BaseCol) },
    !,
    % Stop here
    { Envs = [] }.
parse_front1(Layout, EmptyAbove, Envs) -->
    % (Indented-based code block)
    % Check if this is a code block (based on indentation
    % w.r.t. the previous non-blank line)
    { EmptyAbove = yes },
    sc_col(Col),
    { layout_prevcol(Layout, PrevCol) },
    { codebase(PrevCol, CodeCol) },
    { \+ outside(Col, CodeCol) },
    !,
    { CodeBaseCol = Col },
    parse_indented_block(CodeBaseCol, CodeBlock),
    { Envs = [env(codeblock("text"), CodeBlock)|Envs0] },
    %
    parse_front2(Layout, Envs0).
parse_front1(Layout, _EmptyAbove, Envs) -->
    % (backtick-based code block)
    sc_col(Col),
    { Fence = "````"
    ; Fence = "```"
    ; Fence = "~~~"
    },
    sc_str(Fence),
    skip_blanks_nonl,
    match_lang_id(LangStr0), % ('' if none)
    skip_blanks_nonl,
    sc_nl,
    !,
    { LangStr0 = "" -> LangStr = "text" ; LangStr = LangStr0 },
    parse_backtick_block(CodeBlock0, Fence),
    { tidy_blanks(Col, CodeBlock0, CodeBlock) },
    { Envs = [env(codeblock(LangStr), CodeBlock)|Envs0] },
    %
    parse_front2(Layout, Envs0).
parse_front1(Layout0, EmptyAbove, Envs) -->
    % Try docpred (a predicate head followed by ':')
    { EmptyAbove = yes },
    sc_col(Col), match_docpred(Head, VarNames),
    !,
    { Cmd = docpred(Head, VarNames) },
    { cmd_type(Cmd, Type) },
    { Envs = [env(Cmd, NBody)|Envs0] },
    { layout_mark_linecol(Col, Layout0, Layout1) },
    { layout_inner_basecol(Col, Layout1, Layout) },
    parse_arg(Type, Layout, NBody),
    %
    parse_front2(Layout0, Envs0).
parse_front1(Layout0, _EmptyAbove, Envs) -->
    % Try front commands
    sc_col(Col), match_front_cmd(Cmd),
    !,
    { cmd_type(Cmd, Type) },
    { Envs = [env(Cmd, NBody)|Envs0] },
    { layout_mark_linecol(Col, Layout0, Layout1) },
    { layout_inner_basecol(Col, Layout1, Layout) },
    parse_arg(Type, Layout, NBody),
    %
    parse_front2(Layout0, Envs0).
parse_front1(Layout0, _EmptyAbove, Envs) -->
    % Else, try non-front commands or normal text
    sc_col(Col),
    { layout_mark_linecol(Col, Layout0, Layout) },
    parse_text(Layout, Envs).

% parse_front2(Layout, Envs):
%   Parsing the front of the line (no character has been treated
%   before in this line, except blanks).
parse_front2(Layout, Envs) -->
    skip_blanks(EmptyLines),
    parse_front1(Layout, EmptyLines, Envs).

% Flush the accumulated text (if there is any)
flush_text(StrHead, StrTail, Envs, Envs0) :-
    StrTail = [],
    ( StrHead = [] ->
        Envs = Envs0
    ; Envs = [env('text', StrHead)|Envs0]
    ).

% Fix commands that do not exist in LPdoc yet
% TODO: add them?
fix_env(env(code, EnvsText), Env) :-
    EnvsText = [env('text', Text)],
    !,
    detect_code_cmd(Text, Cmd),
    Env = env(Cmd, EnvsText).
fix_env(env(link(Text), Ref), Env) :- !,
    Env = env(href(Ref), Text).
fix_env(Env, Env).

% Detect the right command for the given program code expression
% TODO: This should be part of LPdoc.
% TODO: Beware of the buggy read_from_string/2!
detect_code_cmd(Text, Cmd) :-
    ( read_from_string(Text, TextTerm) ->
        ( var(TextTerm) -> Cmd = 'var'
        ; TextTerm = (_/A), integer(A), A >= 0 -> Cmd = 'pred' % TODO: read_from_string/2 is buggy and parses "a/" as (a/'')
        ; Cmd = 'tt' % TODO: This should be 'code' (not tt)
        )
    ; Cmd = 'tt'
    ).

%! match_front_cmd(?Cmd):
%    Match a valid front command `Cmd`.

% Hack to avoid parsing @author{...} as a front command
% TODO: better solution!
match_front_cmd(_Cmd) -->
    sc_char(C), { cmdchar(C) },
    sc_str("author{"),
    !,
    { fail }.
% A front command
match_front_cmd(Cmd) -->
    sc_char(C), { cmdchar(C) },
    match_cmdname(Cmd0), { atom_codes(Cmd1, Cmd0) }, { front_cmd(Cmd1) },
    !,
    { Cmd = Cmd1 }.
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
match_front_cmd(Cmd) --> sc_str("# "), !,
    { Cmd = 'section' }.
% A subsection name
match_front_cmd(Cmd) --> sc_str("## "), !,
    { Cmd = 'subsection' }.
% A subsubsection name
match_front_cmd(Cmd) --> sc_str("### "), !,
    { Cmd = 'subsubsection' }.

%! match_docpred(?Head, ?VarNames):
%    Match a predicate documentation head (@tt{Pred}) followed by 
%    `:`. Fails if no documentation head is recognized.
%
% TODO: This does not allow predicate heads which take more than one line
% TODO: Allow '.' too? Make this work?
%
%         %! foo(3.4, a:m). Many problems. That is: colon and period.
%
%       Other solution: parse the term and compare the rest (it it is
%       0': or 0'., you are done).

match_docpred(Head, VarNames) -->
    sc_col(Col), { Col = 0 }, % no left margin allowed at all
    match_docpred_(Head0),
    { valid_docpred_string(Head0) },
    { read_from_string_opts(Head0, Head, [], variable_names(VarNames)) },
    { nonvar(Head), \+ number(Head), \+ atom(Head) }.

match_docpred_([]) --> sc_char(0':), !.
% match_docpred_([]) --> sc_char(0'.), !. % TODO: disabled by now
match_docpred_(_) -->
    % Newline
    sc_nl,
    !,
    { fail }.
match_docpred_([C|Head]) -->
    sc_char(C),
    match_docpred_(Head).

valid_docpred_string([X|_]) :-
    ( X = 0'- ; X = 0'` ),
    % forbid strings that begin with '`' or '-' (reserved for items,
    % use parenthesis if you wish to document the predicate "-/1")
    !, fail.
valid_docpred_string(_).
    
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

parse_backtick_block(Block, Fence) -->
    % End
    ( sc_nl ; [] ),
    sc_col(Col), { Col = 0 }, % match ^\s*``` (or other Fence)
    skip_blanks_nonl,
    sc_str(Fence),
    !,
    { Block = [] }.
parse_backtick_block(Block, Fence) -->
    sc_char(C),
    !,
    { Block = [C|Block0] },
    parse_backtick_block(Block0, Fence).
parse_backtick_block([], _) -->
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

% Match language identifier in fenced code block
match_lang_id([C|Cs]) --> sc_char(C), { \+ is_blank_or_nl(C) }, !,
    match_lang_id(Cs).
match_lang_id([]) --> [].

% ---------------------------------------------------------------------------
% Match a valid identifier for LPdoc command names

% Starting character for commands
cmdchar(0'\\).
cmdchar(0'@). % TODO: Do not allow 0'@? (better compatibility)

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

% Read in `Str` the text before `End` is found in consecutive
% lines. Fails if an empty line is found before. Collapse all lines
% preserving blanks.
match_multiline(Str, End) --> sc_char(0'\n), !,
    ( skip_blanks(EmptyLines),
      { EmptyLines = yes } -> % Fail if empty line found
        { fail }
    ; % Otherwise continue (including same blanks)
      match_multiline_nl(Str, End)
    ).
match_multiline(Str, End) --> sc_str(End), !,
    % Found `Str`, stop here
    { Str = [] }.
match_multiline([C|Str], End) --> sc_char(C), !,
    match_multiline(Str, End).
match_multiline(_Str, _End) -->
    % Nothing else, so it has been not found
    { fail }.

% (nl was accumulated)
match_multiline_nl(Str, End) --> sc_str(End), !,
    % Found `Str`, stop here
    { Str = [] }.
match_multiline_nl(Str, End) -->
    { Str = [0' |Str0] },
    match_multiline(Str0, End).

% TODO: almost duplicated from autodoc_parse, but using layout_dcg
pick_until_endcmd([], CmdStr) -->
    % Peek <some blanks> @end{...}
    \+ \+ (skip_blanks(_), sc_char(C), { cmdchar(C) }, sc_str("end{"), sc_str(CmdStr), sc_str("}")), !.
pick_until_endcmd([X|Xs], CmdStr) --> sc_char(X), !, pick_until_endcmd(Xs, CmdStr).
pick_until_endcmd([], _CmdStr) --> !.

% ---------------------------------------------------------------------------

is_digit(C) :- C >= 0'0, C =< 0'9.

% ---------------------------------------------------------------------------

:- export(is_string/1).
% A cheap (but incomplete) check for strings
is_string([]).
is_string([C|_]) :- integer(C).


