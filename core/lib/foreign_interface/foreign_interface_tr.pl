:- module(foreign_interface_tr, [foreign_interface_tr/3], [assertions]).

% Syntax translations for the foreign interface

:- if(defined(optim_comp)).
:- use_module(library(streams)).
:- use_module(compiler(frontend), [callback__loc/3]).
:- use_module(library(pathnames), [path_split/3]).
:- use_module(compiler(store), [find_source/3]).
:- endif.

:- if(defined(optim_comp)).
foreign_interface_tr((:- use_foreign_source(Spec)), 
                      [(:- '$native_include_c_source'(Spec2))], M) :-
    % TODO: fix file location in optim_comp
    ( atom(Spec) ->
        ( callback__loc(Src,_,_),
          path_split(Src,Dir,_),
          path_split(Dir,_,LastM),
          LastM = M ->
            Spec2 = '+'(Spec)
        ; Spec2 = '.'(Spec)
        )
    ; Spec2 = Spec
    ).
:- else.
% BUG: The assertions are treated before being normalized
foreign_interface_tr(Assertion, Decls, _Mod) :-
    is_assertion(Assertion),
    assertion_pred(Assertion, Pred),
    assertion_props(Assertion, Props),
    foreign_prop(Props),
    functor(Pred, Name, Arity),
    Decls = [Assertion, (:- impl_defined(Name/Arity))].

is_assertion((:- _ pred _)).

% TODO:[xsyntax] new :: and => priorities
assertion_pred((:- _ pred Pred + _ # _), Pred) :- !.
assertion_pred((:- _ pred Pred # _), Pred) :- !.
assertion_pred((:- _ pred Pred :: _), Pred) :- !.
assertion_pred((:- _ pred Pred + _), Pred) :- !.
assertion_pred((:- _ pred Pred), Pred).

assertion_props((:- _ pred _ :: _ + Props # _), Props) :- !.
assertion_props((:- _ pred _ :: _ + Props), Props) :- !.
assertion_props((:- _ pred _ + Props # _), Props) :- !.
assertion_props((:- _ pred _ + Props), Props) :- !.

% Comps contain any foreign prop
foreign_prop(A) :- var(A), !, fail.
foreign_prop((A, B)) :- ( foreign_prop(A) -> true ; foreign_prop(B) ).
foreign_prop(foreign(_)).
foreign_prop(foreign).
foreign_prop(foreign_low(_)).
foreign_prop(foreign_low).
:- endif.
