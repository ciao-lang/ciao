:- module(plindent_types, _, [assertions, regtypes]).

:- use_package(plindent(plindent_decl)).

:- regtype functorproc_t/1.
functorproc_t(functorproc${}).

:- regtype functordesc_t/1.
functordesc_t(functordesc${arg => Arg}) :-
	list(Arg, functordesc_t).

:- regtype argdesc_t/1.
argdesc_t(argdesc${arg => Arg}) :-
	list(Arg, functordesc_t).

:- regtype token_t/1.
token_t(token${}).
