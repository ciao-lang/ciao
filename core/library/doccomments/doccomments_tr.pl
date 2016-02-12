:- module(doccomments_tr, [], [assertions, isomodes, hiord, layout_dcg]).

:- doc(title, "Processing of doccomments").
:- doc(subtitle, "(and lightweight mark-up syntax)").

:- doc(authors, "Jose F. Morales").
:- doc(authors, "Manuel Hermenegildo").

:- doc(module, "This module implements of documentation comments (see
   the package documentation for more details). Documentation comments
   are read as part of the syntax tree by a specialized Prolog
   reader. The documentation comments may contain lightweight mark-up
   syntax. In that case, @lib{doccomments_parser} is used to parse
   them in order to generate declarations readable by @apl{lpdoc}").

:- doc(bug, "Forbid tabs (other wikis do it) and show an error
   when tabs are found. All indentation should be done with spaces.").
:- doc(bug, "Escape code properly in code blocks; change LPdoc verbatim?").
:- doc(bug, "Warn about negative indentation? E.g., in
    %! Foo
    % bar <- wrong
    %bar  <- wrong
    %  bar <- right
").

:- use_module(library(doccomments/doccomments_syntax)).
:- use_module(library(doccomments/doccomments_parser)).

% ===========================================================================

% TODO: Implement motion of doccomments?
%
%   Sent1. %<C Sent2. ===> Sent1 %<C. Sent2. 
%   F(..., A0, %<C A1, ...) ===> F(..., A0 %<C, A1, ...)

% Remove doccomments what appear within predicates, assertions, and
% program text.
:- export(doccomments_term/3).
doccomments_term('\6\doccomment'(_, _, _, _, X), X, _).

:- export(doccomments_sentence/3).
% doccomments_sentence(0, _, Mod) :- !, % no need for initialization
doccomments_sentence(X, R, _Mod) :-
	( X = '\6\doccomment'(_, _, _, _, _)
	; X = '\6\doccomment'(_, _, _)
	),
	!,
	translate_comments(X, R, _Mod).
% doccomments_sentence(end_of_file, _, Mod) :- !,
% 	retractall_fact('$lastSentence'(Mod,_)).
doccomments_sentence(X, R, _Mod) :-
	% TODO: merge into assrt_lib
	assrt_extra(X, R),
	!.

translate_comments('\6\doccomment'(Col, _Mark, _Type, DocString0, Sent),
	           R, _Mod) :- !,
	tidy_blanks(Col, DocString0, DocString),
	( unwiki_comment(DocString, R, RE) -> true
	; R = RE % TODO: Emit error
	),
	translate_comments(Sent, RE, _Mod).
translate_comments('\6\doccomment'(Col, _Mark, DocString0), R, _Mod) :- !,
	tidy_blanks(Col, DocString0, DocString),
	( unwiki_comment(DocString, R, []) -> true
	; R = [] % TODO: Emit error
	).
translate_comments(Sent, [Sent], _Mod).

% ---------------------------------------------------------------------------
% Additional syntactic sugar for assertions
% TODO: merge into the different versions of assrt_lib
%   (try to unify them before):
%        core/lib/assertions/assrt_lib.pl
%        ciaopp/p_unit/assrt_norm.pl
%        core_OC/compiler/assertions__syntactic.pl

:- use_module(library(assertions/assrt_lib), [assertion_body/7]).

% Replace the comment of `Body0` with by `Co` in `Body`.
assrt_set_comment(ABody0, CO, ABody) :-
	assertion_body(PD,DP,CP,AP,GP,_CO0,ABody0),
	assertion_body(PD,DP,CP,AP,GP,CO,ABody).

% Normalize an assertion declaration, or leave unchanged.
maybe_assrt_extra(Decl0, Decl) :-
	assrt_extra(Decl0, Decl1), !, Decl = Decl1.
maybe_assrt_extra(Decl, Decl).

% (fail if this is not extra syntax for assertions)
assrt_extra((:- pred ABody), (:- pred ABody2)) :-
	% TODO: Use normalize_status_and_type
	norm_body(ABody, _, ABody1),
	!, % if fail, assume that it was not an assertion
	norm_args(ABody1, ABody2).

norm_args(ABody0, ABody) :-
	assertion_body(PD0,true,CP,AP,GP,CO,ABody0),
%	display(aa(assertion_body(PD0,true,CP,AP,GP,CO,ABody0))), nl,
	PD0 =.. [H|Args0],
	extract_types(Args0, Args, DP1),
	list_to_prod(DP1, DP),
	\+ useless_type_annot(DP),
	!,
	PD =..[H|Args],
%	display(aa(assertion_body(PD,DP,CP,AP,GP,CO,ABody0))), nl,
	assertion_body(PD,DP,CP,AP,GP,CO,ABody).
norm_args(ABody, ABody).

extract_types([], [], []).
extract_types([A0|As0], [A|As], [T|Ts]) :-
	extract_type(A0, A, T),
	extract_types(As0, As, Ts).

extract_type(A, A, term) :- var(A), !.
extract_type(X :: Type0, A, Type) :- !,
	% A type annotation (X can be an argument or a moded argument)
	A = X, Type = Type0. 
extract_type(M, A, Type) :-
	functor(M, N, A),
	A1 is A + 1,
	functor(M1, N, A1),
	is_modedef(M1),
	% A moded argument, do nothing
	!,
	A = M, Type = term.
extract_type(X, A, Type) :-
	% Assume that it is a type
	A = '?',
	Type = X.

useless_type_annot(term).
useless_type_annot(A * B) :-
	useless_type_annot(A), useless_type_annot(B).

% ---------------------------------------------------------------------------
% TODO: Move to the assertion library to avoid this ugly hack.
%       I need it to distinguish modedefs from typedefs (for norm_args/2).

is_modedef('+'(_)).  is_modedef('+'(_,_)).
is_modedef('-'(_)).  is_modedef('-'(_,_)).
is_modedef('?'(_)).  is_modedef('?'(_,_)).
is_modedef(':'(_)).  is_modedef(':'(_,_)). % From pldoc (can bring problems...)
is_modedef('@'(_)).  is_modedef('@'(_,_)).
is_modedef(in(_)).   is_modedef(in(_,_)). 
is_modedef(out(_)).  is_modedef(out(_,_)).
is_modedef(go(_)).   is_modedef(go(_,_)). 

% ---------------------------------------------------------------------------
% Auxiliary for normalization

:- export(list_to_prod/2).
% From [A1,...,An] to (A1*...*An) ('*' is left associative).
list_to_prod([A|As], B) :-
	list_to_prod_(As, A, B).

list_to_prod_([], Acc, R) :- !, R = Acc.
list_to_prod_([A|As], Acc, R) :- 
	list_to_prod_(As, Acc * A, R).

% TODO: Extended assrt_lib:norm_body/3 with 'is' for comments.
% ------------ A  B   C  D  E --FormatId--------------------------- %ABCDE
norm_body((PD::DP:CP=>AP is GP#CO),p,(PD::DP  :CP  =>AP  +GP  #CO)):-!.%11111I
norm_body((PD::DP:CP=>AP  + GP#CO),p,(PD::DP  :CP  =>AP  +GP  #CO)):-!.%11111
norm_body((PD::DP:CP=>AP is GP   ),p,(PD::DP  :CP  =>AP  +GP  #"")):-!.%11110I
norm_body((PD::DP:CP=>AP  + GP   ),p,(PD::DP  :CP  =>AP  +GP  #"")):-!.%11110
norm_body((PD::DP:CP=>AP      #CO),p,(PD::DP  :CP  =>AP  +true#CO)):-!.%11101
norm_body((PD::DP:CP=>AP         ),p,(PD::DP  :CP  =>AP  +true#"")):-!.%11100
norm_body((PD::DP:CP     is GP#CO),p,(PD::DP  :CP  =>true+GP  #CO)):-!.%11011I
norm_body((PD::DP:CP      + GP#CO),p,(PD::DP  :CP  =>true+GP  #CO)):-!.%11011
norm_body((PD::DP:CP     is GP   ),p,(PD::DP  :CP  =>true+GP  #"")):-!.%11010I
norm_body((PD::DP:CP      + GP   ),p,(PD::DP  :CP  =>true+GP  #"")):-!.%11010
norm_body((PD::DP:CP          #CO),p,(PD::DP  :CP  =>true+true#CO)):-!.%11001
norm_body((PD::DP:CP             ),p,(PD::DP  :CP  =>true+true#"")):-!.%11000
norm_body((PD::DP   =>AP is GP#CO),p,(PD::DP  :true=>AP  +GP  #CO)):-!.%10111I
norm_body((PD::DP   =>AP  + GP#CO),p,(PD::DP  :true=>AP  +GP  #CO)):-!.%10111
norm_body((PD::DP   =>AP is GP   ),p,(PD::DP  :true=>AP  +GP  #"")):-!.%10110I
norm_body((PD::DP   =>AP  + GP   ),p,(PD::DP  :true=>AP  +GP  #"")):-!.%10110
norm_body((PD::DP   =>AP      #CO),p,(PD::DP  :true=>AP  +true#CO)):-!.%10101
norm_body((PD::DP   =>AP         ),p,(PD::DP  :true=>AP  +true#"")):-!.%10100
norm_body((PD::DP        is GP#CO),p,(PD::DP  :true=>true+GP  #CO)):-!.%10011I
norm_body((PD::DP         + GP#CO),p,(PD::DP  :true=>true+GP  #CO)):-!.%10011
norm_body((PD::DP        is GP   ),p,(PD::DP  :true=>true+GP  #"")):-!.%10010I
norm_body((PD::DP         + GP   ),p,(PD::DP  :true=>true+GP  #"")):-!.%10010
norm_body((PD::DP             #CO),d,(PD::DP  :true=>true+true#CO)):-!.%10001
norm_body((PD::DP                ),d,(PD::DP  :true=>true+true#"")):-!.%10000
norm_body((PD    :CP=>AP is GP#CO),p,(PD::true:CP  =>AP  +GP  #CO)):-!.%01111I
norm_body((PD    :CP=>AP  + GP#CO),p,(PD::true:CP  =>AP  +GP  #CO)):-!.%01111
norm_body((PD    :CP=>AP is GP   ),p,(PD::true:CP  =>AP  +GP  #"")):-!.%01110I
norm_body((PD    :CP=>AP  + GP   ),p,(PD::true:CP  =>AP  +GP  #"")):-!.%01110
norm_body((PD    :CP=>AP      #CO),s,(PD::true:CP  =>AP  +true#CO)):-!.%01101
norm_body((PD    :CP=>AP         ),s,(PD::true:CP  =>AP  +true#"")):-!.%01100
norm_body((PD    :CP     is GP#CO),g,(PD::true:CP  =>true+GP  #CO)):-!.%01011I
norm_body((PD    :CP      + GP#CO),g,(PD::true:CP  =>true+GP  #CO)):-!.%01011
norm_body((PD    :CP     is GP   ),g,(PD::true:CP  =>true+GP  #"")):-!.%01010I
norm_body((PD    :CP      + GP   ),g,(PD::true:CP  =>true+GP  #"")):-!.%01010
norm_body((PD    :CP          #CO),c,(PD::true:CP  =>true+true#CO)):-!.%01001
norm_body((PD    :CP             ),c,(PD::true:CP  =>true+true#"")):-!.%01000
norm_body((PD       =>AP is GP#CO),p,(PD::true:true=>AP  +GP  #CO)):-!.%00111I
norm_body((PD       =>AP  + GP#CO),p,(PD::true:true=>AP  +GP  #CO)):-!.%00111
norm_body((PD       =>AP is GP   ),p,(PD::true:true=>AP  +GP  #"")):-!.%00110I
norm_body((PD       =>AP  + GP   ),p,(PD::true:true=>AP  +GP  #"")):-!.%00110
norm_body((PD       =>AP      #CO),s,(PD::true:true=>AP  +true#CO)):-!.%00101
norm_body((PD       =>AP         ),s,(PD::true:true=>AP  +true#"")):-!.%00100
norm_body((PD            is GP#CO),g,(PD::true:true=>true+GP  #CO)):-!.%00011I
norm_body((PD             + GP#CO),g,(PD::true:true=>true+GP  #CO)):-!.%00011
norm_body((PD            is GP   ),g,(PD::true:true=>true+GP  #"")):-!.%00010I
norm_body((PD             + GP   ),g,(PD::true:true=>true+GP  #"")):-!.%00010
norm_body((PD                 #CO),p,(PD::true:true=>true+true#CO)):-!.%00001
norm_body((PD                    ),t,(PD::true:true=>true+true#"")):-!.%00000
% ---------------------------------------------------------------------------

% TODO: This should be imported from LPdoc libraries
:- prop doccomment/1 + regtype.
doccomment((:- doc(_, _))).

:- pred unwiki_comment(Comments, DComm, DCommTail) 
   :: string * list(doccomment) * list 
   # "Expand the lightweight markup from a given docstring into a list
     of documentation declarations.".

unwiki_comment(DocString, Ys, Ys0) :-
	wiki_parse(DocString, Envs),
	envs_to_docdecls(Envs, Ys, Ys0).

% ---------------------------------------------------------------------------
% From envs to docdecls and docstrings (for communication with LPdoc)

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

% Translate a command argument into a term (for docdecls)
arg_to_term(term, X, Term) :- !, Term = X.
arg_to_term(docstring, X, Term) :- !,
	arg_to_docstring(X, nl, _, Term, []).
arg_to_term(docstring_oneline, X, Term) :- !,
	arg_to_docstring(X, nl, _, Term, []).
arg_to_term(Type, _X, _Term) :-
	throw(bug(unknown_cmd_type(Type))).

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
	    norm_body(Head, _, Head1), % TODO: warn if fails
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

% ---------------------------------------------------------------------------

% TODO: This part is too complex due to handling of newlines.

% From env to docstring (with different schemes)
env_to_docstring(Env, A0, A) -->
	( env_to_docstring_(Env, A0, A) ->
	    []
	; { throw(failed_env_to_docstring(Env)) }
	).

:- use_module(library(lists), [append/3]).
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
	envs_to_docstring_(Envs, A0, A).

envs_to_docstring_([], A, A) --> [].
envs_to_docstring_([Env|Envs], A0, A) -->
	env_to_docstring(Env, A0, A1),
	envs_to_docstring_(Envs, A1, A).

ensure_nl(nonl, nl) --> "\n".
ensure_nl(nl, nl) --> [].

% ---------------------------------------------------------------------------
% Tidy blanks

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

detect_margin_size_([], M, M).
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
cut_left_([0'\n|Cs0], M, [0'\n|Cs]) :-
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

