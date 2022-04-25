:- module(assrt_write, [
    write_assertion/6,
    write_assertion/7,
    write_assertion_as_comment/6,
    write_assertion_as_comment/7,
    write_assertion_as_double_comment/6,
    write_assertion_as_double_comment/7
], [assertions, regtypes]).

:- doc(title,"Pretty-printing assertions").
:- doc(author,"Francisco Bueno").

:- doc(module,"This module defines some predicates which are
   useful for writing assertions in a readable form.").

:- use_module(library(format)).  
:- use_module(engine(stream_basic)).  
:- use_module(engine(io_basic)).

% Other libraries
:- use_module(library(assertions/assrt_lib), [assertion_body/7]).
:- use_module(library(messages)).
:- use_module(library(assertions/assertions_props)).

:- use_module(library(vndict), [varnamesl2dict/2, rename/2, complete_dict_alpha/3]).


:- regtype status_flag(F) # "@var{F} is @tt{status} or @tt{nostatus}.".

status_flag(status).
status_flag(nostatus).

:- pred write_assertion(Goal,Status,Type,Body,Dict,Flag)
    :: ( assrt_status(Status), assrt_type(Type),
         nabody(Body), dictionary(Dict),
         status_flag(Flag) )
    # "Writes the (normalized) assertion to current output.".

write_assertion(Goal,Status,Type,Body,Dict,Flag) :-
    current_output(CO),
    \+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,no,CO).

:- pred write_assertion(Stream,Goal,Status,Type,Body,Dict,Flag)
    :: ( assrt_status(Status), assrt_type(Type),
         nabody(Body), dictionary(Dict),
         status_flag(Flag) )
    # "Writes the (normalized) assertion to stream @var{Stream}.".

write_assertion(S,Goal,Status,Type,Body,Dict,Flag) :-
    \+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,no,S).

:- pred write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag)
    :: ( assrt_status(Status), assrt_type(Type),
         nabody(Body), dictionary(Dict),
         status_flag(Flag) )
    # "Writes the (normalized) assertion to current output as
       a Prolog comment.".

write_assertion_as_comment(Goal,Status,Type,Body,Dict,Flag) :-
    current_output(CO),
    \+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,yes,CO).

:- pred write_assertion_as_comment(Stream,Goal,Status,Type,Body,Dict,Flag)
    :: ( assrt_status(Status), assrt_type(Type),
         nabody(Body), dictionary(Dict),
         status_flag(Flag) )
    # "Writes the (normalized) assertion to stream @var{Stream} as
       a Prolog comment.".

write_assertion_as_comment(Stream,Goal,Status,Type,Body,Dict,Flag) :-
    \+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,yes,Stream).


:- pred write_assertion_as_double_comment(Goal,Status,Type,Body,Dict,Flag)
    :: ( assrt_status(Status), assrt_type(Type),
         nabody(Body), dictionary(Dict),
         status_flag(Flag) )
    # "Writes the (normalized) assertion to current output as
       a Prolog double comment.".

write_assertion_as_double_comment(Goal,Status,Type,Body,Dict,Flag) :-
    current_output(CO),
    \+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,double,CO).


:- pred write_assertion_as_double_comment(Stream,Goal,Status,Type,Body,Dict,Flag)
    :: ( assrt_status(Status), assrt_type(Type),
         nabody(Body), dictionary(Dict),
         status_flag(Flag) )
    # "Writes the (normalized) assertion to stream @var{Stream} as
       a Prolog double comment.".

write_assertion_as_double_comment(Stream,Goal,Status,Type,Body,Dict,Flag) :-
    \+ \+ write_assertion_(Goal,Status,Type,Body,Dict,Flag,double,Stream).

write_assertion_(Goal,Status,Type,Body,Dict,Flag,AsComm,Stream) :-       
    % unify_vars(Dict),
    varnamesl2dict(Dict, VnDict),
    complete_dict_alpha(VnDict, Goal, D2),
    % DTM: Maybe someone instantiated our variables when doing a write!!!
    ( rename(Goal, D2) -> true ; true ),
    comm_prefix(AsComm,Stream),
    ( Flag=nostatus ->
        format(Stream,":- ~w ~q",[Type,Goal])
    ; format(Stream,":- ~w ~w ~q",[Status,Type,Goal])
    ),
    complete_dict_alpha(D2, Body, D3),
    ( rename(Body, D3) -> true ; true ),
    assertion_body(Goal,Compat,Call,Succ,Comp,Comm,Body),
    Tab = 3,
    write_props(Compat,'::',AsComm,Tab,Stream),
    write_props(Call,':',AsComm,Tab,Stream), % ' :'
    write_props(Succ,'=>',AsComm,Tab,Stream),
    write_props(Comp,'+',AsComm,Tab,Stream), % ' +'
    write_comment(Comm,AsComm,Tab,Stream),
    display(Stream, '.'), nl(Stream),
    !.
write_assertion_(_Goal,Status,Type,Body,_Dict,_Flag,_AsComm,_Stream) :-
    error_message("Error printing assertion:~n:- ~w ~w ~w~n",
           [Status,Type,Body]),
    fail.

write_comment([],_AsComm,_Tab,_) :- !.
write_comment(Comm,AsComm,Tab,S) :-
    escape_string(Comm,Comm2),
    print_sep(nl,AsComm,Tab,S),
    format(S,'# "~s"',[Comm2]).

% TODO: reuse other code?
escape_string([], []).
escape_string([A|Ar], [A|Br]) :- A \== 0'", !,
    escape_string(Ar,Br).
escape_string([A|Ar], [0'\\,A|Br]) :-
    escape_string(Ar,Br).

write_props(Props,Modif,AsComm,Tab,Stream) :-
    ( Modif = '::' -> FormS = conj % TODO: why?
    ; props_form(Props,FormS)
    ),
    write_props_(Props,Modif,AsComm,Tab,FormS,Stream).

write_props_([]      ,_Modif,_AsComm,_Tab,_Form,_) :- !.
write_props_([true]  ,_Modif,_AsComm,_Tab,conj,_) :- !.
write_props_([[]]    ,_Modif,_AsComm,_Tab,disj,_) :- !.
write_props_([[true]],_Modif,_AsComm,_Tab,disj,_) :- !.
write_props_(List    ,Modif,AsComm,Tab,Form,Stream) :-
    print_sep(nl,AsComm,Tab,Stream),
    atom_length(Modif,ModifL),
    Tab2 is Tab + ModifL + 3, % size of ' ( '
    display(Stream,Modif), display(Stream,' '),
    ( List = [(C1;C2)] ->
        conj_to_list_of_list((C1;C2), List2)
    ; List2 = List
    ),
    print_prop_list(Form,List2,AsComm,Tab2,Stream).

% TODO: old comment: DTM: this case appears in :- calls p(X): (ground(X);var(X)).
props_form([(_;_)],disj) :- !.
props_form(Call,disj) :- llist(Call), !.
props_form(_Call,conj).

conj_to_list_of_list((A;B), [A|Bs]) :- list(A), !,
    conj_to_list_of_list(B, Bs).
conj_to_list_of_list((A;B), [AL|Bs]) :- !,
    conj_to_list(A, AL),
    conj_to_list_of_list(B, Bs).
conj_to_list_of_list(A, [A]) :- list(A), !.
conj_to_list_of_list(A, [AL]) :-
    conj_to_list(A, AL).

conj_to_list((A,B), [A|Bs]) :- !,
    conj_to_list(B, Bs).
conj_to_list(A, [A]).

print_prop_list(conj,List,AsComm,Tab,S) :-
    print_conjunction(List,AsComm,Tab,S).
print_prop_list(disj,List,AsComm,Tab,S) :-
    print_disjunction(List,AsComm,Tab,S).

print_disjunction([],_AsComm,_Tab,_).
print_disjunction([Prop],AsComm,Tab,S) :- !,
    print_conjunction_0(Prop,AsComm,Tab,S).
print_disjunction([Prop|Props],AsComm,Tab,S) :-
    display(S,'( '),
    print_conjunction_0(Prop,AsComm,Tab,S),
    print_disjunction_2(Props,AsComm,Tab,S).

print_disjunction_2([],_AsComm,_Tab,S) :-
    display(S,' )').
print_disjunction_2([Prop|Props],AsComm,Tab,S) :-
    display(S,'; '),
    print_conjunction_0(Prop,AsComm,Tab,S),
    print_disjunction_2(Props,AsComm,Tab,S).

print_conjunction_0([],_AsComm,_Tab,S) :- !,
    display(S,'true').
print_conjunction_0(A,AsComm,Tab,S) :-
    print_conjunction(A,AsComm,Tab,S).

print_conjunction([],_AsComm,_Tab,_S).
print_conjunction([Prop],_AsComm,_Tab,S) :- !,
    print_prop(Prop,S).
print_conjunction([Prop|Props],AsComm,Tab,S) :-
    display(S, '( '),
    ownline_prop(Prop,OwnLine),
    print_prop(Prop,S),
    print_conjunction_2(Props,OwnLine,AsComm,Tab,S).

print_conjunction_2([],_PrevOwnLine,_AsComm,_Tab,S) :-
    display(S, ' )').
print_conjunction_2([Prop|Props],PrevOwnLine,AsComm,Tab,S) :-
    display(S, ','),
    ownline_prop(Prop,OwnLine),
    % use nl separator when either this or the previous properties
    % require their own line
    ( ( PrevOwnLine = yes ; OwnLine = yes ) -> Sep = nl
    ; Sep = nonl
    ),
    print_sep(Sep,AsComm,Tab,S),
    print_prop(Prop,S),
    print_conjunction_2(Props,OwnLine,AsComm,Tab,S).

print_sep(nonl,_AsComm,_Tab,S) :- display(S, ' ').
print_sep(nl,AsComm,Tab,S) :- nl(S), comm_prefix(AsComm,S), tab(S,Tab).

comm_prefix(double,S) :- display(S,'%% %% ').
comm_prefix(yes,S) :- display(S,'%% ').
comm_prefix(no,_S).

% print in its own line (heuristics)
ownline_prop(mshare(_),yes) :- !. % TODO: customize with a multifile?
ownline_prop(P,yes) :- functor(P,_,A), A>=3, !.
ownline_prop(_,no).

print_prop(Prop,S) :-
    ( needs_paren(Prop) ->
        format(S,"(~q)",[Prop])
    ; format(S, "~q" ,[Prop])
    ).

% TODO: incomplete
needs_paren((_:_)).
needs_paren((_,_)).
needs_paren((_=_)).

% unify_vars([]).
% unify_vars([N=V|Dict]) :-
%     V='$VAR'(N),
%     unify_vars(Dict).

llist([]).
llist([X|Xs]) :- list(X), llist(Xs).
