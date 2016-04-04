:- module(doccomments_tr, [], [assertions, isomodes, hiord, dcg]).

:- doc(title, "Processing of doccomments").
:- doc(subtitle, "(and lightweight mark-up syntax)").

:- doc(authors, "Jose F. Morales").
:- doc(authors, "Manuel Hermenegildo").

:- doc(module, "This module implements the extraction of assertions
   and documentation (see this package documentation for more details)
   from specially marked documentation comments. Documentation
   comments are read as part of the syntax tree (implemented as a flag
   in @lib{library(read)}).

   This module assumes that the format of documentation comments is in
   the LPdoc-flavored markdown syntax. The module
   @lib{markdown_translate} is used to translate the syntax to classic
   @apl{lpdoc} markup").

:- use_module(library(assertions/assrt_lib_extra), [assrt_extra/2]).
:- use_module(library(markdown/markdown_translate)).
:- use_module(library(layout_dcg/layout_dcg_rt), [tidy_blanks/3]).

:- use_module(library(markdown/markdown_syntax), [decl_cmd/1, cmd_type/2]).

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
	( markdown_to_docdecls(DocString, R, RE) -> true
	; R = RE % TODO: Emit error
	),
	translate_comments(Sent, RE, _Mod).
translate_comments('\6\doccomment'(Col, _Mark, DocString0), R, _Mod) :- !,
	tidy_blanks(Col, DocString0, DocString),
	( markdown_to_docdecls(DocString, R, []) -> true
	; R = [] % TODO: Emit error
	).
translate_comments(Sent, [Sent], _Mod).

