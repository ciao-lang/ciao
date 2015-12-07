:- package(attr_mono_compat).

% uncomment to use mutli-attributes interface by default.
% :- compilation_fact(use_mutliattr_by_default).

:- if((defined(use_mutliattr_globally), \+ defined(use_monoattr_locally))).

% TODO: warning directive is not implemented. 
:- warning("Your are using a multi-attributes version of the
package. It has not been extensively tested and may be not
efficient.").

:- use_package(attr).
:- use_module(library(lists), [append/3]).

attach_attribute(Var, Attr):-
	put_attr_local(Var, Attr).

update_attribute(Var, Attr):-
	put_attr_local(Var, Attr).

get_attribute(Var, Attr):-
	var(Var),
	get_attr_local(Var, Attr).

detach_attribute(Var) :- nonvar(Var), !.
detach_attribute(Var) :-
	del_attr_local(Var).

attr_unify_hook(Attr1, Other):-
        (
            nonvar(Other) ->
            verify_attribute(Attr1, Other)
        ;
            get_attr_local(Other, Attr2) ->
            combine_attributes(Attr1, Attr2)
        ;
            put_attr_local(Other, Attr1)
        ).

:- push_prolog_flag(multi_arity_warnings, off).
dump_constraints(Term, Copy, Cs, Cs0):-
	dump_constraints(Term, Copy, Cs1), 
	lists:append(Cs1, Cs0, Cs).

:- discontiguous(dump_constraints/3).
dump_constraints(_, _, _) :- fail.

:- discontiguous(portray_attribute/3).
portray_attribute(_, _) :- fail.

:- pop_prolog_flag(multi_arity_warnings).

:- else.

:- use_module(engine(attributes)).
:- multifile verify_attribute/2.
:- multifile combine_attributes/2.
:- multifile portray_attribute/2.
:- multifile dump_constraints/3.

:- endif.
