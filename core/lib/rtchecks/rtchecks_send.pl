:- module(_, [send_rtcheck/6, send_comp_rtcheck/3], [assertions, nortchecks, dcg]).

:- use_module(engine(attributes)).
:- use_module(library(terms_vars)).
:- use_module(library(hiordlib), [foldl/4]).
:- use_module(library(lists)).

pretty_attributes(Term, Attrs) :-
    varset(Term, Vars),
    foldl(pretty_attribute, Vars, Attrs, []).

pretty_attribute(Var) -->
    ( {get_attribute(Var, Attr)} ->
         [attach_attribute(Var, Attr)]
    ; []
    ).

send_rtcheck(ErrType, PredName, Dict, PropName, ActualProp0, AsrLocs) :-
    % expose_attributes(ActualProp0, ActualProp),
    pretty_attributes(ActualProp0, Atts),
    append(ActualProp0, Atts, ActualProp),
    E = rtcheck(ErrType, PredName, Dict, PropName, ActualProp, AsrLocs),
    send_signal(E, Intercepted),
    ( Intercepted = false -> throw(E) ; true ). % throw if no handler

send_comp_rtcheck(PredName, PropName, ActualProp) :-
    send_rtcheck(comp, PredName, [], PropName, [ActualProp], []).
