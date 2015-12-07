:- module(foreign_interface_tr, [foreign_interface_tr/3], []).

:- use_package(assertions).
:- use_package(library(foreign_interface/foreign_interface_ops)).

:- use_module(library(foreign_interface/foreign_inliner_tr)).

% BUG: The assertions are treated before being normalized
foreign_interface_tr((:- Spec pred PredDecl --> InlineDef), Decls, Mod) :- !,
	Assertion = (:- Spec pred PredDecl),
	is_assertion(Assertion),
	assertion_pred(Assertion, Pred),
	assertion_props(Assertion, Props),
	foreign_prop(Props),
	functor(Pred, Name, Arity),
	foreign_inliner_tr((:- foreign_inline(Name/Arity, InlineDef)), Decl0, Mod),
	Decls = [Assertion, (:- impl_defined(Name/Arity))| Decl0].
foreign_interface_tr(Assertion, Decls, _Mod) :-
	is_assertion(Assertion),
	assertion_pred(Assertion, Pred),
	assertion_props(Assertion, Props),
	foreign_prop(Props),
	functor(Pred, Name, Arity),
	Decls = [Assertion, (:- impl_defined(Name/Arity))].

is_assertion((:- _ pred _)).

assertion_pred((:- _ pred Pred + _ # _), Pred) :- !.
assertion_pred((:- _ pred Pred # _), Pred) :- !.
assertion_pred((:- _ pred Pred :: _), Pred) :- !.
assertion_pred((:- _ pred Pred + _), Pred) :- !.
assertion_pred((:- _ pred Pred), Pred).

assertion_props((:- _ pred _ :: _ + Props # _), Props) :- !.
assertion_props((:- _ pred _ :: _ + Props), Props) :- !.
assertion_props((:- _ pred _ + Props # _), Props) :- !.
assertion_props((:- _ pred _ + Props), Props) :- !.

foreign_prop(foreign(_)).
foreign_prop(foreign).
foreign_prop(foreign_low(_)).
foreign_prop(foreign_low).
foreign_prop((A, B)) :- ( foreign_prop(A) -> true ; foreign_prop(B) ).
