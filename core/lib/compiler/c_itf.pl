:- module(c_itf, [], []).

% This module is (will be) a wrapper of c_itf_internal to 'legacy' applications.

:- reexport(library(compiler/c_itf_internal)).

:- export(module_expansion/12).
module_expansion(H, B, Module, Dict, Mode, Src, Ln0, Ln1, H1, B1, H2, B2):-
	asserta_fact(location(Src,Ln0,Ln1), Ref),
	( module_expansion(H, B, Module, Dict, Mode, H1, B1, H2, B2)
	-> true
	 ; display(internal_error(H,B)), nl
	),
	erase(Ref).

:- export(def_multifile/4).
:- redefining(def_multifile/4).
def_multifile(Base,F,A,DefType) :-
        c_itf_internal:def_multifile(Base,F,A,DefType),
        \+ internal_predicate(F,A).

:- export(clause_of/7).
:- redefining(clause_of/7).
clause_of(Base, Head, Body, VarNames, Source, Line0, Line1):-
	c_itf_internal:clause_of(Base, Head, Body, VarNames, Source, Line0, Line1),
	\+ (number(Head), internal_decl(Body), arg(1,Body,Pred/Arity),
	  internal_predicate(Pred,Arity)).

:- export(add_clause_of/7).
add_clause_of(Base, Head, Body, VarNames, Source, Line0, Line1) :-
	assertz_fact(c_itf_internal:clause_of(Base, Head, Body, VarNames, 
                     Source, Line0, Line1)).

% See prelude.pl --EMM:
internal_decl(multifile(_)).
internal_decl(discontiguous(_)).
internal_decl(dynamic(_)).

internal_predicate('$primitive_meta_predicate',2).
internal_predicate('$current_module',1).
internal_predicate('$ldlibs',1).
internal_predicate('$multifile',3).
internal_predicate('$load_libs',0).
internal_predicate('$meta_args',2).
internal_predicate('$u',2).
internal_predicate('$initialization',1).
internal_predicate('$on_abort',1).
internal_predicate('$imports',5).
internal_predicate('$defines',3).
