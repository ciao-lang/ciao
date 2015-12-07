:- module(dcg_phrase_rt, [phrase/2, phrase/3], 
	[assertions, nortchecks, isomodes]).

:- use_module(engine(internals), [module_concat/3]).
:- use_module(engine(hiord_rt), ['$meta_call'/1]).
:- use_module(library(dcg/dcg_tr), [dcg_translate_dcg/5]).

:- doc(title,"Runtime Support for DCGs").

:- doc(author, "Jose F. Morales").

:- doc(module, "This module offers translation of Definite Clause
   Grammars (DCGs) at runtime.").

% call-in-module
% WARNING: skips the module system, since dcg_phrase_rt does not import M
call_in_module(M, P1) :-
	module_concat(M, '\6\call_from_phrase'(P1), P2),
	'$meta_call'(P2).

% TODO: Those metapredicates are wrong
% (dcg_translate_dcg/4 works on unexpanded code)
%:- meta_predicate phrase(goal,?).
%:- meta_predicate phrase(goal,?,?).

:- doc(phrase(Phrase,List),"Like @tt{phrase(Phrase,List,[])}.").
:- meta_predicate phrase(addmodule,?).
phrase(P, _, _) :-
	var(P), !,
	throw(error(instantiation_error, phrase/2-1)).
phrase(P, M, L) :-
	dcg_translate_dcg(P, P1, M, L, []), !,
	call_in_module(M, P1).

:- pred phrase(+Phrase,?List,?Remainder)
# "The list @var{List} is a phrase of type @var{Phrase} (according to
   the current grammar rules), where @var{Phrase} is either a
   non-terminal or more generally a grammar rule body. @var{Remainder}
   is what remains of the list after a phrase has been found.".
:- meta_predicate phrase(addmodule,?,?).
phrase(P, _, _, _) :-
	var(P), !,
	throw(error(instantiation_error, phrase/3-1)).
phrase(P, M, L, R) :-
	dcg_translate_dcg(P, P1, M, L, R), !,
	call_in_module(M, P1).



