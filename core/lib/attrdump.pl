:- module(attrdump, 
        [
            copy_extract_attr/3, 
            copy_extract_attr_nc/3, 
            cp_attr/3,
            reinstall_attributes/1
        ], [dcg,assertions]).

:- use_module(library(dict), [dic_lookup/4, dictionary/5]).
:- use_module(library(attr/attr_rt), [attvar/1, attvars_residuals/3]).
:- use_module(engine(attributes)).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Copy is a copy of Term with fresh, non-attributed variables, and
 %% AttrList is a list containing a representation of the attributes
 %% which where associated with each attributed variables in Term.
 %% The variables contained in AttrList are the same (in the sense of
 %% ==/2) as those in Copy. Thus, Copy plus OrdAttr convey the
 %% same information as Term, but expanded.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_extract_attr(Term, Copy, AttrList) :-
        cp_attr(Term, Copy, Dict),
        attrlist(Dict, AttrList, []).

copy_extract_attr_nc(Term, Copy, AttrList) :-
        cp_attr_nc(Term, Copy, Dict),
        attrlist(Dict, AttrList, []).

cp_attr(Term, Copy, Dict) :-
        cp_attr_(Term, Copy, [], Dict).

cp_attr_(Var, Copy, _Seen, Dict) :-
        var(Var), !,
        ( 
	    attvar(Var) ->
	    % RH : att_var v2.0 
	    dic_lookup(Dict, Var, cva2(Copy, L_, T_), Stat),
	    (
		Stat = new ->
		attvars_residuals([Var], L, T), 
		cp_attr(L, L_, Dict),
		dic_lookup(Dict, T, v(T_), _)
	    ;
		true
	    )
	;
	    get_attribute(Var, Attrib) ->
	    % RH : att_var v1.0
            dic_lookup(Dict, Var, cva(Copy,Attcopy), Stat),
            ( Stat = new ->
                cp_attr(Attrib, Attcopy, Dict)
            ; true
            )
	;
	    dic_lookup(Dict, Var, v(Copy), _)
        ).
cp_attr_(Const, Const, _Seen, _Dict) :-
        atomic(Const), !.
cp_attr_(Term, Copy, Seen, _Dict) :-
        already_seen(Seen, Term, Copy), !.
cp_attr_(Term, Copy, Seen, Dict) :-
        functor(Term, N, A),
        functor(Copy, N, A),
        cp_attr_args(A, Term, Copy, [(Term,Copy)|Seen], Dict).

cp_attr_args(0, _,    _,    _,    _   ) :- !.
cp_attr_args(N, Term, Copy, Seen, Dict) :-
        N1 is N-1,
        arg(N, Term, SubT),
        arg(N, Copy, SubC),
        cp_attr_(SubT, SubC, Seen, Dict),
        cp_attr_args(N1, Term, Copy, Seen, Dict).

already_seen([(T,C)|_], Term, Copy):-
        Term == T, !,
        Copy = C.
already_seen([_|Ps], Term, Copy) :-
        already_seen(Ps, Term, Copy).


cp_attr_nc(Term, Copy, Dict) :-
        type(Term, Type),
        cp_attr_nc_t(Type, Term, Copy, Dict).

cp_attr_nc_t(attv,      Cva,    Copy,     Dict) :-
	(
	    attvar(Cva) ->
	    % RH : att_var v2.0 
	    dic_lookup(Dict, Cva, cva2(Copy, L_, T_), Stat),
	    (
		Stat = new ->
		attvars_residuals([Cva], L, T), 
		cp_attr_nc(L, L_, Dict),
		dic_lookup(Dict, T, v(T_), _)
	    ;
		true
	    )
	;
	    % RH : att_var v1.0
	    dic_lookup(Dict, Cva, cva(Copy,Attcopy), Stat),
	    ( 
		Stat = new ->
		get_attribute(Cva, Attrib),
		cp_attr_nc(Attrib, Attcopy, Dict)
	    ;
		true
	    )
	).
cp_attr_nc_t(var,       V,      Copy,     Dict) :-
        dic_lookup(Dict, V, v(Copy), _).
cp_attr_nc_t(integer,   I,      I,        _).
cp_attr_nc_t(float,     F,      F,        _).
cp_attr_nc_t(atom,      A,      A,        _).
cp_attr_nc_t(list,      [X|Xt], [Xc|Xct], Dict) :-
        cp_attr_nc(X, Xc, Dict),
        cp_attr_nc(Xt, Xct, Dict).
cp_attr_nc_t(structure, Term,   Copy,     Dict) :-
        functor(Term, N, A),
        functor(Copy, N, A),
        cp_attr_nc_args(A, Term, Copy, Dict).

cp_attr_nc_args(0, _,    _,    _   ) :- !.
cp_attr_nc_args(N, Term, Copy, Dict) :-
        N1 is N-1,
        arg(N, Term, At),
        arg(N, Copy, Ac),
        cp_attr_nc(At, Ac, Dict),
        cp_attr_nc_args(N1, Term, Copy, Dict).

attrlist(Dict) --> {var(Dict)}, !, [].
attrlist(Dict) --> {dictionary(Dict,_,Val,L,R)},
        attrlist(L),
        attr(Val),
        attrlist(R).

attr(cva(Copy,Constr)) --> [attach_attribute(Copy, Constr)].
attr(v(_)            ) --> [].
attr(cva2(_, L,T), L, T).


reinstall_attributes([]).
reinstall_attributes([attach_attribute(Copy, Constr)|Rest]):-
        attach_attribute(Copy, Constr),
        reinstall_attributes(Rest).
