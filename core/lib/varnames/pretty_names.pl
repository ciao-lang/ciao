:- module(pretty_names, [pretty_names/3], [assertions, basicmodes, nortchecks]).

:- use_module(library(varnames/dict_types),    [varnamesl/1]).
:- use_module(library(varnames/complete_dict), [complete_dict/4]).
:- use_module(library(varnames/apply_dict),    [apply_dict/3]).
:- use_module(library(lists),                  [append/3]).

:- pred pretty_names(+varnamesl, ?term, ?term).

pretty_names(Dict, Term0, Term) :-
	complete_dict(Term0, Dict, [], CDict),
	append(Dict, CDict, ADict),
	apply_dict(Term0, ADict, Term).
