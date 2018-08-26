:- module(markdown_translate, [], [assertions, isomodes, hiord, dcg]).

:- doc(title, "Translate LPdoc-flavored markdown").

:- doc(authors, "Jose F. Morales").

:- doc(module, "This module implements the translation of
   LPdoc-flavored markdown syntax to the classic LPdoc mark-up (as a
   string or as a list of declarations which may include
   assertions).

   When assertions are parsed this library is using the extended
   assertion syntax implemented in
   @lib{assertions/assrt_lib_extra}.

   Note that these modules are provided as part of the core Ciao
   libraries and are not exclusive to LPdoc and the generation of
   documentation. They can be used, for example, to read predicate
   assertions from comments and pass them to verification tools.").

:- use_module(library(markdown/markdown_syntax)).
:- use_module(library(markdown/markdown_parser),
	[is_string/1, markdown_parse/2]).

:- use_module(library(assertions/assrt_lib_extra), [
	norm_body_extra/3,
	assrt_set_comment/3,
	maybe_assrt_extra/2]).

:- use_module(library(lists), [member/2, append/3]).

:- doc(bug, "Make assertion parsing optional? (or separate from this parser)").
:- doc(bug, "Forbid tabs (do other markdown it?) and show an error
   when tabs are found. All indentation should be done with spaces.").
:- doc(bug, "Warn about negative indentation? E.g., in
    %! Foo
    % bar <- wrong
    %bar  <- wrong
    %  bar <- right
").

% ---------------------------------------------------------------------------

% :- use_module(lpdoc(comments), [docstring/1]).
:- use_module(library(layout_dcg/layout_dcg_rt), [tidy_blanks/3]).

:- export(translate_markdown/2).
:- pred translate_markdown(From, To)
   :: string * string
   # "Translate the lightweight markup into the classical lpdoc markup.".

translate_markdown(From, To) :-
	tidy_blanks(0, From, From2),
	markdown_parse(From2, Envs),
	envs_to_docstring(Envs, nl, _, To, []).

% ---------------------------------------------------------------------------

%% :- use_module(library(lists), [reverse/2]).
%% 
%% % Remove surrounding blanks (spaces and newlines)
%% tidy_comment(Text0, Text) :-
%% 	remove_blank(Text0, Text1),
%% 	reverse(Text1, Text2),
%% 	remove_blank(Text2, Text3),
%% 	reverse(Text3, Text).
%% 
%% % remove spaces (newlines and blanks)
%% remove_blank([X|Xs0], Xs) :- blank(X), !, remove_blank(Xs0, Xs).
%% remove_blank(Xs, Xs).

%! detect_code_cmd(Text, Cmd):
%    Detect the appropriate documentation command `Cmd` (variable,
%    predicate, other code, etc.) for the code `Text`.

% ---------------------------------------------------------------------------

% TODO: This part is too complex due to handling of newlines.
% TODO: Env is not documented

:- export(env_to_docstring/5).
% From env to docstring (with different schemes)
env_to_docstring(Env, A0, A) -->
	( env_to_docstring_(Env, A0, A) ->
	    []
	; { throw(error(env(Env), env_to_docstring/5)) }
	).

% (emit a string)
s(Str, S, S0) :- append(Str, S0, S).

env_to_docstring_(env('text', "\n"), _A0, A) --> !,
	% TODO: Add an environment for this
	% A paragraph break
	{ A = nl }, "\n\n".
env_to_docstring_(env('text', Arg), A0, A) --> !,
	% <Arg>(text)
	arg_to_docstring_(Arg, A0, A).
env_to_docstring_(env(Cmd, Arg), A0, A) --> { block_cmd(Cmd) }, !,
	% <newline>@begin{h}<newline><Arg><newline>@end{h}<newline>
	{ atom_codes(Cmd, CmdS) },
	ensure_nl(A0, _),
	"@begin{", s(CmdS), "}", ensure_nl(nonl, A1),
	arg_to_docstring_(Arg, A1, A3), ensure_nl(A3, _),
	"@end{", s(CmdS), "}", ensure_nl(nonl, A).
env_to_docstring_(env(Cmd, Arg), A0, A) --> { prefix_cmd(Cmd) }, !,
	% <newline>@h <Arg><newline>
	ensure_nl(A0, _),
	"@",
	( { Cmd = 'item'(N) } -> % description items
	    "item{", arg_to_docstring_(N, nonl, _), "}"
	; { Cmd = 'item_num'(N) } -> % numbered items
	    "item{", s(N), "}"
	; % generic case
	  { atom_codes(Cmd, CmdS) },
	  s(CmdS)
	),
	" ",
	arg_to_docstring_(Arg, nonl, A2),
	ensure_nl(A2, A).
env_to_docstring_(env(Cmd, Arg), _A0, A) --> { functor(Cmd, Cmd0, 1) }, !,
	{ arg(1, Cmd, Arg0) },
	% @h{<Arg0>}{<Arg>}
	{ atom_codes(Cmd0, Cmd0S) },
	"@", s(Cmd0S), "{",
	arg_to_docstring_(Arg0, nonl, _), "}{",
	arg_to_docstring_(Arg, nonl, _), "}",
	{ A = nonl }.
env_to_docstring_(env(Cmd, Arg), A0, A) -->
	% (a section cmd)
	{ is_section_cmd(Cmd) },
	!,
	% <newline>@h{Arg}<newline>
	{ atom_codes(Cmd, CmdS) },
	ensure_nl(A0, _),
	"@", s(CmdS), "{",
	arg_to_docstring_(Arg, nonl, _),
	"}", ensure_nl(nonl, A).
env_to_docstring_(env(Cmd, Arg), _A0, A) -->
	% @h{Arg}
	{ atom_codes(Cmd, CmdS) },
	"@", s(CmdS), "{", arg_to_docstring_(Arg, nonl, _), "}",
        { A = nonl }.

is_section_cmd('section').
is_section_cmd('subsection').
is_section_cmd('subsubsection').

:- export(arg_to_docstring/5).
arg_to_docstring(B0, A0, A1) -->
	( arg_to_docstring_(B0, A0, A1) ->
	    []
	; { throw(bug(failed_arg_to_docstring(B0))) }
	).

arg_to_docstring_("\n", _A0, A) --> !,
	"\n",
	{ A = nl }.
arg_to_docstring_(X, _A0, A) --> { is_string(X) }, !,
	s(X),
	{ A = nonl }.
arg_to_docstring_(Envs, A0, A) -->
	envs_to_docstring(Envs, A0, A).

envs_to_docstring([], A, A) --> [].
envs_to_docstring([Env|Envs], A0, A) -->
	env_to_docstring(Env, A0, A1),
	envs_to_docstring(Envs, A1, A).

ensure_nl(nonl, nl) --> "\n".
ensure_nl(nl, nl) --> [].

% ---------------------------------------------------------------------------

% TODO: This should be imported from LPdoc libraries
:- prop doccomment/1 + regtype.
doccomment((:- doc(_, _))).

:- export(markdown_to_docdecls/3).
:- pred markdown_to_docdecls(Comments, DComm, DCommTail) 
   :: string * list(doccomment) * list 
   # "Expand the lightweight markup from a given docstring into a list
     of documentation declarations.".

markdown_to_docdecls(DocString, Ys, Ys0) :-
	markdown_parse(DocString, Envs),
	envs_to_docdecls(Envs, Ys, Ys0).

% ---------------------------------------------------------------------------
% From envs to docdecls and docstrings

% TODO: Integrate inside the LPdoc parsing (so that this won't be --
%   or will be less -- necessary).

% From envs to docdecls in Decls-Decls0
envs_to_docdecls([], Decls, Decls).
envs_to_docdecls([Env|Envs], [Decl|Decls], Decls0) :-
	env_to_docdecl(Env, Decl),
	envs_to_docdecls(Envs, Decls, Decls0).

% From envs to docdecl in Decl
env_to_docdecl(env(docpred(H1, VarNames), B0), Decl) :-
	!,
	% TODO: Emit pred assertions WHEN Cmd is a docpred(_, _) which
	%   contains some kind of domain information (like types or
	%   modes). Emit doc assertions otherwise.
	docpred_to_docdecl(H1, VarNames, B0, Decl).
env_to_docdecl(env(Cmd,B0), Decl) :-
	% The command can appear as a declaration
	decl_cmd(Cmd),
	!,
	Decl = (:- doc(Cmd, Arg)),
	cmd_type(Cmd, Type),
	arg_to_term(Type, B0, Arg).
env_to_docdecl(Env, Decl) :-
	% TODO: Not yet supported in LPdoc
	% TODO: Collapse them
	Decl = (:- doc('text', S)),
	env_to_docstring(Env, nl, _, S, []).

% Obtain the right declaration from a 'docpred' documentation
% environment.
docpred_to_docdecl(Head, VarNames, DocBody, Decl) :-
	cmd_type(docpred(Head, VarNames), Type),
	arg_to_term(Type, DocBody, Arg),
	% Insert varnames (see autodoc_state:doc_assertion_read/9 and
	% autodoc_state:get_docdecl/4 in LPdoc)
	( VarNames = [] -> Arg2 = Arg
	; Arg2 = '\6\varnames'(Arg, VarNames)
	),
	% Emit pred or doc assertion
	( head_contains_usage(Head) ->
	    norm_body_extra(Head, _, Head1), % TODO: warn if fails
	    assrt_set_comment(Head1, Arg2, Head2),
	    Decl0 = (:- pred(Head2))
	; Decl0 = (:- doc(Head, Arg2))
	),
	% TODO: merge into assrt_lib
	maybe_assrt_extra(Decl0, Decl).

% The head contains usage information (i.e., it is a 'pred'
% assertion).
% TODO: This should be moved to LPdoc (or to the assertion library)
head_contains_usage(_/_) :- !,
	fail.
head_contains_usage(Head) :-
	Head =.. [_|Args],
	member(X, Args),
	nonvar(X),
	!.

% Translate a command argument into a term (for docdecls)
%
% NOTE:
%   The \dc{} command is added at the beginning to explicitly annotate
%   that this text is partially parsed by markdown_translate. See autodoc_parse
%   for more details.

arg_to_term(term, X, Term) :- !, Term = X.
arg_to_term(docstring, X, Str) :- !,
	Str = "\\dc{}"||Str0,
	arg_to_docstring(X, nl, _, Str0, []).
arg_to_term(docstring_oneline, X, Str) :- !,
	Str = "\\dc{}"||Str0,
	arg_to_docstring(X, nl, _, Str0, []).
arg_to_term(Type, _X, _) :-
	throw(error(unknown_cmd_type(Type), arg_to_term/3)).
