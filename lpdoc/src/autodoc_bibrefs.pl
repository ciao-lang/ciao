:- module(autodoc_bibrefs,
	[resolve_bibliography/1, 
         parse_commands/3 % TODO: a hack
	 ],
	[dcg, assertions, regtypes]). 

:- doc(title, "Resolution of Bibliographical References").

:- doc(author, "Manuel Hermenegildo (original version)").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides a predicate to resolve the
	bibliographical references found during the generation of
	documentation.").

:- doc(bug, "Using external BibTeX command to resolve references. We
can merge with @apl{bibutils} in order to get rid of this
dependency.").

:- use_module(library(dict)).
:- use_module(library(aggregates)).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(format)).
:- use_module(library(pathnames), [path_concat/3]).

:- use_module(lpdoc(autodoc_state)).
:- use_module(lpdoc(autodoc_doctree)).
:- use_module(lpdoc(autodoc_refsdb)).
:- use_module(lpdoc(autodoc_settings)).
:- use_module(lpdoc(autodoc_aux), [autodoc_process_call/3]).
:- use_module(lpdoc(autodoc_aux), [verbose_message/1, verbose_message/2]).

:- pred resolve_bibliography(DocSt) : docstate #
"This predicate resolves bibliographical references. The algorithm is as follows:
 @begin{itemize}
 @item Write all the bibliographical references to a @tt{.aux} file.
 @item Invoke BibTeX with a customized @tt{.bst} file that generates a pseudo-docstring.
 @item Load the docstring and fix its syntax.
 @item Parse the docstring as a doctree.
 @item Extract @tt{(Label,Ref)} pairs from @tt{bibitem} commands.
 @end{itemize}

 Both the docstring and label/reference pairs are kept in the
 @var{DocSt}, and used later to map symbolic references to textual
 labels.".
         
% - Parse the references and extract RefPairs
%   ((Label,Ref) pairs from bibitem commands, used later to map symbolic
%   refs to textual labels).
% - Store the results (in the docstate)

resolve_bibliography(DocSt) :-
	docst_currmod(DocSt, Name),
	verbose_message("{Resolving bibliographical references", []),	
	( no_citations(DocSt) ->
	    % We had no citations, do nothing
	    % (bibtex would fail otherwise)
	    RefsR = [],
	    RefPairs = []
	; % Building references BiBTeX
	  % TODO: BblFile, RAuxFile can be removed later
	  atom_concat([Name, '_tmp'], TmpBase),
	  atom_concat([TmpBase, '.bbl'], BblFile),
	  atom_concat([TmpBase, '.aux'], RAuxFile),
	  %
	  write_bibtex_citations(DocSt, RAuxFile),
	  run_bibtex(TmpBase, RAuxFile, BblFile),
	  % Read BblFile and parse it
	  file_to_string(BblFile, RefsString0),
	  %
	  parse_commands(RefsString, RefsString0, []),
	  parse_docstring_loc(DocSt, _Loc, RefsString, RefsR),
	  findall((Label,Ref), member(bibitem(Label, Ref), RefsR), RefPairs)
	),
	% Save results in the docstate
	% TODO: at this time this is more convenient than asserting data
	docst_mvar_lookup(DocSt, biblio_doctree, RefsR),
	docst_mvar_lookup(DocSt, biblio_pairs, RefPairs),
	verbose_message("}", []).

run_bibtex(TmpBase, _RAuxFile, _BblFile) :-
	% TODO: RAuxFile can be removed later
	bibtex(BibTex),
	% TODO: allowing errors here, fix
	% This will take as input RAuxFile and output BblFile
	autodoc_process_call(path(BibTex), [TmpBase],
	                     [logbase(TmpBase, '_bibtex'), status(_)]).

:- use_module(lpdoc(autodoc_parse), [parse_docstring_loc/4]).

:- pred write_bibtex_citations(DocSt, RAuxFile) # "Write all the
   citations in the file with name @var{RAuxFile}, compatible with
   BibTeX.".

write_bibtex_citations(DocSt, RAuxFile) :-
	bibtex_config(LibDir, BibFiles),
	%
	open(RAuxFile, write, CS),
	% Write all citations
	( % (failure-driven loop)
	  docst_gdata_query(DocSt, citation(RefClean)),
	    format(CS, "\\citation{~s}\n", [RefClean]),
	    fail
	; true
	),
	% Our custom style that writes cites in pseudo-lpdoc notation
	Style = 'docstring', 
	path_concat(LibDir, Style, StyleFile),
	format(CS, "\\bibstyle{~w}~n", [StyleFile]),
	% The .bib files required to resolve references
	format(CS, "\\bibdata{", []),
	write_list_sep(BibFiles, ',', CS),
	format(CS, "}~n", []),
	%
	close(CS).

% There are no citations in refs_closure
no_citations(DocSt) :-
	\+ docst_gdata_query(DocSt, citation(_)).

% ---------------------------------------------------------------------------

:- doc(bug, "Find a better way to sent/configure this information in
   autodoc (e.g. passed as arguments (like other options are
   passed))").

bibtex_config(LibDir, BibFiles) :-
	check_setting(lpdoclib),
	setting_value(lpdoclib, LibDir),
	findall(BF, setting_value_or_default(bibfile, BF), BibFiles).

% ---------------------------------------------------------------------------
:- doc(section, "Special parser for our custom BBL output").

% TODO: This is not a parser, but a translator from BBL output (a
%       subset of LaTeX) to a docstring (that can be parsed later).
parse_commands([]) --> [].
%% Some special commands, handled directly
parse_commands(" " || Tail) --> "~", !,
	parse_commands(Tail).
parse_commands(" " || Tail) --> start, " ", !, % TODO: This was '@ ' (or '\ '), correct?
	parse_commands(Tail).
parse_commands("_" || Tail) --> start, "_", !,
	parse_commands(Tail).
parse_commands("~" || Tail) --> start, "~{}", !,
	parse_commands(Tail).
parse_commands("&" || Tail) --> start, "&", !,
	parse_commands(Tail).
%% Some accents without braces (e.g. \'a ===> \'{a})
parse_commands(NCommand) -->
	( open ; [] ), % a kludge to void entering the 'alt syntax' clause
	               % (otherwise, it cannot parse {\'e}) 
	start,
	command_char(Accent),
	{ accent(Accent) },
	accented_char(X),
	!,
	{ handle_command([Accent], [X], NCommand, Tail) },
	parse_commands(Tail).
%% Command(s) with two bodies
parse_commands(NCommand) -->
	start,
	command_chars1(Chars),
	open,
	{ Chars = "htmladdnormallink"
	; Chars = "bibitem"
	},
	balanced_braces(1, CommandBody1),
	separators,
	open,
	balanced_braces(1, CommandBody2),
	!,
	{ handle_command_2b(Chars, CommandBody1, CommandBody2, NCommand, Tail) },
	parse_commands(Tail).
%% Generic commands, with space after them
parse_commands(NCommand) -->
	start,
	command_chars1(Chars),
	space_like,
	!,
	{handle_command(Chars, [], NCommand, Tail)},
	parse_commands(Tail).
%% Commands, with no space after them
parse_commands(NCommand) -->
	start,
	command_chars1(Chars),
	open,
	balanced_braces(1, CommandBody),
	!,
	{handle_command(Chars, CommandBody, NCommand, Tail)},
	parse_commands(Tail).
%% Commands, with no space after them, alt syntax
parse_commands(NCommand) -->
	open,
	start,
	command_chars1(Chars),
	separators,
	balanced_braces(1, CommandBody),
	!,
	{handle_command(Chars, CommandBody, NCommand, Tail)},
	parse_commands(Tail).
%% Lone braces: ignore!
parse_commands(Tail) -->
	brace,
	!,
	parse_commands(Tail).
%% Normal chars
parse_commands([X|T]) -->
	normal_char(X),
	!,
	parse_commands(T).
%% Else warning, skip one, continue
parse_commands(Y, Z, W) :-
	Z = [A, B, C, D, E, F, G, H|X],
	!,
	warning_str([A, B, C, D, E, F, G, H]),
	parse_commands(Y, [B, C, D, E, F, G, H|X], W).
%% At end, just give up
parse_commands(Z, Z, _).

warning_str([A, B, C, D, E, F, G, H]) :-
	format("Parsing error around ~s \n", [[A, B, C, D, E, F, G, H]]).

%% Not empty char sequence
command_chars1([C|Cs]) -->
	command_char(C),
	command_chars(Cs).

command_chars([C|Cs]) -->
	command_char(C),
	command_chars(Cs).
command_chars([]) -->
	[].

command_args([Arg|RArgs]) -->
	all_chars(Arg),
	close,
	spaces,
	open,
	!,
	command_args(RArgs).
command_args([Arg]) -->
	all_chars(Arg),
	close.

all_chars([0'@, 0'{|Cs]) -->
	start,
	open,
	all_chars(Cs).
all_chars([0'@, 0'}|Cs]) -->
	start,
	close,
	all_chars(Cs).
all_chars([0'@, 0'@|Cs]) -->
	start,
	start,
	all_chars(Cs).
all_chars([C|Cs]) -->
	normal_char(C),
	all_chars(Cs).
all_chars([]) -->
	[].

spaces --> space, spaces.
spaces --> [].

separators --> space_like, separators.
separators --> [].

% TODO: This is a kludge and incomplete
accented_char(X) --> [X], { accented_char_(X) }.
accented_char_(0'a).
accented_char_(0'e).
accented_char_(0'i).
accented_char_(0'o).
accented_char_(0'u).


normal_char(X) --> [X], {X \== 0'\\, X \== 0'@, X \== 0'{, X \== 0'}}.
command_char(X) --> [X], {X \== 0'\\, X \== 0'@, X \== 0'{, X \== 0'},
	    X \== 0' , X \== 0'\n, X \== 0'\t}.
char_no_space_or_tab(X) --> [X], {X \== 0' , X \== 0'\t}.

brace --> [0'{].
brace --> [0'}].

start --> [0'\\].
start --> [0'@].
open --> [0'{].
close --> [0'}].
space --> [0' ].
tabchar --> [0'\t].
newline --> [0'\n].

space_like --> [0' ].
space_like --> [0'\t].
space_like --> [0'\n].

accent(0'\").
accent(0'').
accent(0',).
accent(0'^).
accent(0'`).
accent(0'~).

balanced_braces(1, []) --> "}", !.
balanced_braces(N, [0'@, 0'@|Rest]) -->	"@@", !,
	balanced_braces(N, Rest).
balanced_braces(N, [0'@, 0'{|Rest]) --> "@{", !,
	balanced_braces(N, Rest).
balanced_braces(N, [0'@, 0'}|Rest]) --> "@}", !,
	balanced_braces(N, Rest).
balanced_braces(N, [0'{|Rest]) --> "{", !,
	{N1 is N+1},
	balanced_braces(N1, Rest).
balanced_braces(N, [0'}|Rest]) --> "}", !,
	{N1 is N-1},
	balanced_braces(N1, Rest).
balanced_braces(N, [X|Rest]) -->
	[X],
	balanced_braces(N, Rest).

handle_command(Command, [], NCommand, Tail) :- !,
	new_command(Command, sp, NewCommand, NewBody),
	handle_body(NewCommand, NewBody, NCommand, Tail).
handle_command(Command, Body, NCommand, Tail) :-
	new_command(Command, Body, NewCommand, NewBody),
	handle_body(NewCommand, NewBody, NCommand, Tail).

handle_command_2b("htmladdnormallink", Body1, Body2, NewCommand, Tail) :- !,
	parse_commands(NBody1, Body1, []),
	parse_commands(NBody2, Body2, []),
	append(NBody1, " (@href{", T1),
	append(T1, NBody2, T2),
	append(T2, "})" || Tail, NewCommand).
handle_command_2b(Command, Body1, Body2, NewCommand, Tail) :- !,
	append("@"||Command, "{"||Body1, T1),
	append(T1, "}{"||Body2, T2),
	append(T2, "}"||Tail, NewCommand).

handle_body(NewCommand, sp, NCommand, Tail) :- !,
	append([0'@|NewCommand], [0' |Tail], NCommand).
handle_body(NewCommand, nsp, NCommand, Tail) :- !,
	append([0'@|NewCommand], Tail, NCommand).
handle_body(NewCommand, NewBody, NCommand, Tail) :-
	parse_commands(ParsedNewBody, NewBody, []),
	append([0'@|NewCommand], [0'{|ParsedNewBody], T1),
	append(T1,               [0'}|Tail],          NCommand).

% Translation of some formatting TeX commands to lpdoc docstring
% TODO: what is this command?
%%new_command("o",      _,     "o{}",                nsp) :- !.
new_command([0'"],    [X],   [0'.,0'.,0'{,X,0'}], nsp) :- !.
new_command([A],      "i",   [A|"{i}"], nsp) :- accent(A), !.
new_command([A],      "\\i", [A|"{i}"], nsp) :- accent(A), !.
new_command([A],      "j",   [A|"{j}"], nsp) :- accent(A), !.
new_command([A],      "\\j", [A|"{j}"], nsp) :- accent(A), !.
new_command([A],      [X],   [A,0'{,X,0'}],  nsp) :- accent(A), !.
new_command("tt",     Body,  "tt", Body) :- !.
new_command("texttt", Body,  "tt", Body) :- !.
new_command("emph",   Body,  "em", Body) :- !.
new_command("em",     Body,  "em", Body) :- !.
new_command("sf",     Body,  "bf", Body) :- !. % TODO: This is sans-serif!
new_command("bf",     Body,  "bf", Body) :- !.
new_command(Command,  sp,    Command, sp) :- !.
new_command(Command,  Body,  Command, Body) :- !.

write_list_sep([], _Sep, _O) :- !.
write_list_sep([E], _Sep, O) :- !,
	format(O, "~w", [E]).
write_list_sep([H|T], Sep, O) :-
	format(O, "~w~w", [H, Sep]),
	write_list_sep(T, Sep, O).


