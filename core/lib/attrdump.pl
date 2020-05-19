:- module(attrdump, [
    copy_extract_attr/3, 
    copy_extract_attr_nc/3, 
    reinstall_attributes/1
], [dcg,assertions]).

:- use_module(library(assoc), [
    empty_assoc/1,
    get_assoc/3,
    put_assoc/4,
    assoc_to_list/2]).
:- use_module(library(attr/attr_rt), [attvar/1, attvars_residuals/3]).
:- use_module(engine(attributes)).

% TODO: merge copy_extract_attr/3 and copy_extract_attr_nc/3

% ---------------------------------------------------------------------------
% Copy is a copy of Term with fresh, non-attributed variables, and
% AttrList is a list containing a representation of the attributes
% which where associated with each attributed variables in Term.
% The variables contained in AttrList are the same (in the sense of
% ==/2) as those in Copy. Thus, Copy plus OrdAttr convey the
% same information as Term, but expanded.

copy_extract_attr(Term, Copy, AttrList) :-
    empty_assoc(Dict0),
    empty_assoc(Seen),
    cp_attr(Term, Copy, Seen, Dict0, Dict),
    assoc_to_list(Dict, AttrList0),
    attrlist(AttrList0, AttrList, []).

copy_extract_attr_nc(Term, Copy, AttrList) :-
    empty_assoc(Dict0),
    cp_attr(Term, Copy, nc, Dict0, Dict),
    assoc_to_list(Dict, AttrList0),
    attrlist(AttrList0, AttrList, []).

cp_attr(Var, Copy, _Seen, Dict0, Dict) :- var(Var), !,
    ( attvar(Var) ->
        % RH : att_var v2.0 
        ( get_assoc(Var, Dict0, Val) ->
            Val = cva2(Copy, _, _),
            Dict = Dict0
        ; put_assoc(Var, Dict0, cva2(Copy, L_, T_), Dict1),
          attvars_residuals([Var], L, T), 
          clean_seen(Seen, Seen1),
          cp_attr(L, L_, Seen1, Dict1, Dict2),
          assoc_lookup(T, Dict2, v(T_), Dict)
        )
    ; get_attribute(Var, Attrib) ->
        % RH : att_var v1.0
        ( get_assoc(Var, Dict0, Val) ->
            Val = cva(Copy, _),
            Dict = Dict0
        ; put_assoc(Var, Dict0, cva(Copy,Attcopy), Dict1),
          clean_seen(Seen, Seen1),
          cp_attr(Attrib, Attcopy, Seen1, Dict1, Dict)
        )
    ; assoc_lookup(Var, Dict0, v(Copy), Dict)
    ).
cp_attr(Const, Const, _Seen, Dict0, Dict) :- atomic(Const), !, Dict=Dict0.
cp_attr(Term, Copy, Seen, Dict0, Dict) :-
    already_seen(Seen, Term, Copy), !, Dict=Dict0.
cp_attr(Term, Copy, Seen, Dict0, Dict) :-
    functor(Term, N, A),
    functor(Copy, N, A),
    add_seen(Term, Copy, Seen, Seen1),
    cp_attr_args(A, Term, Copy, Seen1, Dict0, Dict).

assoc_lookup(K, Dict0, V, Dict) :-
    ( get_assoc(K, Dict0, V0) -> V = V0, Dict = Dict0
    ; put_assoc(K, Dict0, V, Dict)
    ).

cp_attr_args(0, _, _, _, Dict0, Dict) :- !, Dict = Dict0.
cp_attr_args(N, Term, Copy, Seen, Dict0, Dict) :-
    N1 is N-1,
    arg(N, Term, SubT),
    arg(N, Copy, SubC),
    cp_attr(SubT, SubC, Seen, Dict0, Dict1),
    cp_attr_args(N1, Term, Copy, Seen, Dict1, Dict).

clean_seen(nc,nc) :- !.
clean_seen(_,Seen) :- empty_assoc(Seen).

add_seen(_,_,nc,nc) :- !.
add_seen(Term,Copy,Seen0,Seen) :- put_assoc(Term,Seen0,Copy,Seen).

already_seen(nc, _Term, _Copy):- !, fail. % 'Seen' disabled
already_seen(Seen, Term, Copy) :- get_assoc(Term, Seen, Copy).

attrlist([]) --> !, [].
attrlist([_-Val|KVs]) -->
    attr(Val),
    attrlist(KVs).

attr(cva(Copy,Constr)) --> [attach_attribute(Copy, Constr)].
attr(v(_)) --> [].
attr(cva2(_, L,T), L, T). % (insert the L-T difference list)

% ---------------------------------------------------------------------------

reinstall_attributes([]).
reinstall_attributes([attach_attribute(Copy, Constr)|Rest]):-
    attach_attribute(Copy, Constr),
    reinstall_attributes(Rest).
