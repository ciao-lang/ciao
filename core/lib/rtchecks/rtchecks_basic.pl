:- module(rtchecks_basic, [
            collapse_prop/4,
            diff_props/3,
            get_predname/4,
            get_propnames/2,
            get_pretty_names/5,
            insert_posloc/7,
            is_member_prop/2,
            is_same_prop/2,
            list_to_lits/2, % TODO:T67
            lists_to_lits/2,
            list_to_disj/2, % TODO:T67
            lists_to_disj/2,
            remove_element/3
        ], [assertions, nortchecks, dcg, hiord]).

:- use_module(library(llists),     [flatten/2]).
:- use_module(library(hiordlib), [maplist/3, maplist/4]).
:- use_module(library(varnames/apply_dict),    [apply_dict/3,
                                            select_applicable/3]).
:- use_module(library(varnames/pretty_names),  [pretty_names/3]).
:- use_module(library(varnames/complete_dict), [complete_dict/4]).
:- use_module(library(varnames/dict_types),    [varnamesl/1]).
:- use_module(library(lists),                  [append/3,list_of_lists/1]).
:- use_module(library(rtchecks/term_list),     [push_meta/3, push_term/3]).
:- use_module(library(assertions/native_props),[nonground/1]).
:- use_module(library(assertions/assertions_props), [head_pattern/1]).

% ----------------------------------------------------------------------

:- doc(author, "Edison Mera").
:- doc(author, "Nataliia Stulova (documentation)").

:- doc(module, "Basic predicates used in rtchecks expansion.").

% ----------------------------------------------------------------------

:- pred insert_posloc(Flags,PredName0,PLoc0,ALoc,PosLocs,PredName,PosLoc)
           :  gndstr * struct * struct * gndstr * list * var * var
           => gndstr * struct * struct * gndstr * list * var * list(var)
           # "Depending on the value of flags @var{Flags} pair constructs
             a list of variables @var{PosLoc} where list elements are
             associated with the location information terms; or returns
             an empty list.".

insert_posloc((UsePredLoc, UseAsrLoc), PredName0, PLoc0, ALoc,
        PosLocs, PredName, PosLoc) :-
    (UsePredLoc == yes ; UseAsrLoc == yes) ->
    push_meta(PredName0, PosLocs, PredName),
    ( UsePredLoc == yes ->
        push_term(PLoc0,                   PosLocs, PLoc),
        push_term(predloc(PredName, PLoc), PosLocs, PosPLoc),
        PosLoc = [PosPLoc|PosLoc1]
    ;
        PosLoc = PosLoc1
    ),
    ( UseAsrLoc == yes ->
        push_term(asrloc(ALoc), PosLocs, PosALoc),
        PosLoc1 = [PosALoc]
    ;
        PosLoc1 = []
    )
    ;
    PosLoc = [].

% ----------------------------------------------------------------------

get_predname(short, _, Pred, F/A) :-
    functor(Pred, F, A).
get_predname(long, Dict, Pred, PredName) :-
    pretty_names(Dict, Pred, PredName).

get_propnames(Prop, Prop).

short_prop_name(Prop, Name-[]) :-
    callable(Prop),
    arg(1, Prop, Arg),
    var(Arg),
    Prop =.. [FName, _|Args],
    gnd(Args) ->
    Name =.. [FName|Args]
    ;
    Name = Prop.

short_prop_names(Props, Names) :-
    maplist(short_prop_name, Props, Names).

propname_name(Name, Name-_).

propdict_name(PropDict, _-PropDict).

% Replaces the keys K of the dict, e.g. 'X', for their pretty name, e.g. '$VAR'('X')
'atoms_to_$VAR'(Dict0,Dict) :-
    apply_dict(Dict0,Dict0,Dict1),
    maplist('atom_to_$VAR' ,Dict0,Dict1,Dict).

'atom_to_$VAR'(Atom=V,Atom=PrettyV,PrettyV=V).

long_prop_names(Props, PropNames, Dict, Names) :-
    maplist(my_select_applicable(Dict), Props, PropDicts0),
    maplist('atoms_to_$VAR',PropDicts0,PropDicts),
    maplist(propname_name, PropNames, Names),
    maplist(propdict_name, PropDicts, Names).

my_select_applicable(Dict, Prop, PropDict) :-
    select_applicable(Prop, Dict, PropDict).

% in this predicate, PredName and the name of each property must be ground
% to avoid undesired unifications.
get_pretty_names(short, Term, Dict, TermName, Dict) :-
    Term = n(Pred, Compat, Call, Succ, Comp),
    functor(Pred, F, A),
    PredName = F/A,
    short_prop_names(Compat, CompatName),
    short_prop_names(Call,   CallName),
    short_prop_names(Succ,   SuccName),
    short_prop_names(Comp,   CompName),
    TermName = n(PredName, CompatName, CallName, SuccName, CompName).
get_pretty_names(long, Term, Dict0, TermName, Dict) :-
    Term = n(_Pred, Compat, Call, Succ, Comp),
    complete_dict(Term, Dict0, [], CDict),
    append(Dict0, CDict, Dict),
    apply_dict(Term, Dict, TermName0),
    TermName0 = n(PredName, CompatName0, CallName0, SuccName0, CompName0),
    long_prop_names(Compat, CompatName0, Dict, CompatName),
    long_prop_names(Call,   CallName0,   Dict, CallName),
    long_prop_names(Succ,   SuccName0,   Dict, SuccName),
    long_prop_names(Comp,   CompName0,   Dict, CompName),
    TermName = n(PredName, CompatName, CallName, SuccName, CompName).

% ----------------------------------------------------------------------

% note that the following predicates make partial unification, and comparison
% over the given term: cui(Compare, Unify, Ignore)

diff_props(L,      [], L) :- !. % Minor optimization
diff_props(L1, [H|L2], L3) :- diff_props_2(L1, [H|L2], L3).

diff_props_2([],     _,  []).
diff_props_2([H|L1], L2, L3) :-
    is_member_prop(L2, H),
    !,
    diff_props(L1, L2, L3).
diff_props_2([H|L1], L2, [H|L3]) :-
    diff_props_2(L1, L2, L3).

is_member_prop([T0|_], T1) :-
    is_same_prop(T0, T1),
    !.
is_member_prop([_|L], X) :- is_member_prop(L, X).

is_same_prop(T0, T1) :-
    T0 = cui(C0, U0, _),
    T1 = cui(C1, U1, _),
    C0 == C1,
    % The unification should be done After the comparison, to avoid
    % problems if [U0,U1] share variables with [C0,C1]:
    U0 = U1.

collapse_prop(T0, T1, Es, Es) :-
    is_same_prop(T0, T1),
    !.
collapse_prop(_, cui(_, _, I), [I|Es], Es).

remove_element(A,     _, A) :- var(A), !.
remove_element([],    _, []).
remove_element([X|Y], E, Z0) :-
    (
        X == E ->
        Z0 = Z
    ;
        Z0 = [X|Z]
    ),
    remove_element(Y, E, Z).

lists_to_lits(A0, L) :-
    flatten(A0, A1),
    remove_element(A1, true, A2),
    list_to_lits(A2, L).

list_to_lits([],     true).
list_to_lits([X|Xs], Lits) :-
    list_to_lits2(Xs, X, Lits).

list_to_lits2([],     X,  X).
list_to_lits2([X|Xs], X0, (X0, Lits)) :-
    list_to_lits2(Xs, X, Lits).

list_to_disj([],     fail).
list_to_disj([X|Xs], Lits) :-
    list_to_disj2(Xs, X, Lits).

list_to_disj2([],     X,  X).
list_to_disj2([X|Xs], X0, (X0 ; Lits)) :-
    list_to_disj2(Xs, X, Lits).

lists_to_disj(A0, L) :-
    flatten(A0, A1),
    remove_element(A1, fail, A2),
    list_to_disj(A2, L).
