:- use_module(engine(hiord_rt), ['$meta_call'/1]).

:- use_package(dcg).

:- if(defined(clpqr__use_multi_attributes)).


:- use_package(attr).

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

attribute_goals(_) --> [].

dump_constraints(Term, Copy) --> 
	{clpqr_dump_constraints(Term, Copy, Constraints)}, 
	insert(Constraints).

insert([H|T]) --> [H], insert(T).
insert([]) --> [].


:- else. % if(defined(clpqr__use_multi_attributes)).


:- multifile 
        verify_attribute/2,
        combine_attributes/2.

:- multifile dump_constraints/3. % Hook in the toplevel

dump_constraints(Term, Copy, Constraints):- 
	clpqr_dump_constraints(Term, Copy, Constraints).

:- endif. % if(defined(clpqr__use_multi_attributes)).


verify_attribute(clpr_frozen(X,System,User), Value) :-
        detach_attribute(X),
        X = Value,
        '$meta_call'(System),
        '$meta_call'(User).
verify_attribute(float(F), Term) :-
        arith_eval(F=:=Term).
verify_attribute(term_wrap(Self,T), Const) :-
        normalize(T-Const, _, I, H),
        solve_lin(H, I),
        ground_meta(Self, Const).
verify_attribute(eqn_var(_Self,_,Lin,_Ref,_Nl), Const) :-
        get_attribute(Lin, I+H),
        arith_eval(I-Const, I1),
        % this leads to a redundant collect_equate_eqs ...
        solve_lin(H, I1).

% first level dispatch
%
combine_attributes(clpr_frozen(X,Gs,Gu), M2) :-
        combine_attributes_frz(M2, X, Gs, Gu).
combine_attributes(float(T1),     M2) :-
        combine_attributes_f(M2, T1).
combine_attributes(term_wrap(S1,T1), M2) :-
        combine_attributes_t(M2, S1, T1).
combine_attributes(eqn_var(S,A,Lin,D,E), M2) :-
        combine_attributes_e(M2, S,A,Lin,D,E).

% second level dispatch
%
combine_attributes_frz(clpr_frozen(X2,G2s,G2u), X1, G1s, G1u) :-
        detach_attribute(X1),
        X1 = X2,
        join_goals(G1s, G2s, Gs),
        join_goals(G1u, G2u, Gu),
        update_attribute(X1, clpr_frozen(X1,Gs,Gu)).
combine_attributes_frz(float(F), X1, G1s, G1u) :-
        combine_attributes_frz_f(F, X1, G1s, G1u).
combine_attributes_frz(term_wrap(X,Term), X1, G1s, G1u) :-
        combine_attributes_frz_t(X, Term, X1, G1s, G1u).
combine_attributes_frz(eqn_var(X2,_,_,Ref,Ga), X1, G1s, G1u) :-
        combine_attributes_frz_e(X2, Ref, Ga, X1, G1s, G1u).
%
combine_attributes_f(float(T2),     T1) :- arith_eval(T2=:=T1).
combine_attributes_f(clpr_frozen(X,S,U), F) :-
        combine_attributes_frz_f(F, X, S, U).
combine_attributes_f(term_wrap(S2,T2), T1) :-
        normalize(T2-T1, _, I, H),
        solve_lin(H, I),
        ground_meta(S2, T1).
combine_attributes_f(eqn_var(_,_,Lin,_,_), T1) :-
        get_attribute(Lin, I+H),
        arith_eval(I-T1, Inh),
        solve_lin(H, Inh).
%
combine_attributes_t(float(T2), S1, T1) :-
        normalize(T1-T2, _, I, H),
        solve_lin(H, I),
        ground_meta(S1, T2).
combine_attributes_t(clpr_frozen(Y,G1s,G1u), X, Term) :-
        combine_attributes_frz_t(X, Term, Y, G1s, G1u).
combine_attributes_t(term_wrap(S2,T2), S1, T1) :-
        detach_attribute(S1),                                   % unwrap
        detach_attribute(S2), normalize(T2, S2),                % unwrap
        normalize(T1-S2, _, I, H),
        solve_lin(H, I),
        S1 = S2.
combine_attributes_t(eqn_var(S2,_,Lin,_,_), S1, T1) :-
        get_attribute(Lin, I+H),
        detach_attribute(S1),                         % unwrap
        S1 = S2,                                      % they are equal now
        normalize(T1, Tk, Ti, Th),
        arith_eval(Tk*Ti-I, I1),
        add_linear_ff(Th, Tk, H, -1, H1),
        solve_lin(H1, I1).
%
combine_attributes_e(clpr_frozen(X1,G1s,G1u), X2, _, _, Ref, Ga) :-
        combine_attributes_frz_e(X2, Ref, Ga, X1, G1s,G1u).
combine_attributes_e(float(T2), _,  _, Lin2, _, _) :-
        get_attribute(Lin2, I+H),
        arith_eval(I-T2, Inh),
        solve_lin(H, Inh).
combine_attributes_e(term_wrap(S2,T2), S1, _, Lin2, _, _) :-
        get_attribute(Lin2, I+H),
        detach_attribute(S2),                         % unwrap
        S1 = S2,                                      % they are equal now
        normalize(T2, Tk, Ti, Th),
        arith_eval(Tk*Ti-I, I1),
        add_linear_ff(Th, Tk, H, -1, H1),
        solve_lin(H1, I1).
combine_attributes_e(eqn_var(_M1,_T1,Lin1,_Ref1,_Nl1), _M2, _, Lin2, _, _) :-
        get_attribute(Lin1, I1+H1),
        get_attribute(Lin2, I2+H2),
        arith_eval(I1 - I2, I),
        add_linear_ff(H1, 1, H2, -1, H),
        solve_lin(H, I).

combine_attributes_frz_f(F, X, Gs, Gu) :-
        update_attribute(X, float(F)),
        '$meta_call'(Gs),
        '$meta_call'(Gu).

combine_attributes_frz_t(X, Term, Y, G1s, G1u) :-
        detach_attribute(X),
        detach_attribute(Y),
        X = Y,
        normalize(Term, X),
        get_attribute(X, eqn_var(_,_,_,Ref,Ga)),
        ( get_attribute(Ga, (G2s,G2u)) ->
            join_goals(G1s, G2s, Gs),
            join_goals(G1u, G2u, Gu),
            update_attribute(Ga, (Gs,Gu))
        ; attach_attribute(Ga, (G1s,G1u)),
          get_attribute(Ref, p(T,De,Det,In,Int)),
          EqsType is T \/ 2'01,
          update_attribute(Ref, p(EqsType,De,Det,In,Int))
        ).

combine_attributes_frz_e(X2, Ref, Ga, X1, G1s, G1u) :-
        detach_attribute(X1),
        X1 = X2,
        ( get_attribute(Ga, (G2s,G2u)) ->
            join_goals(G1s, G2s, Gs),
            join_goals(G1u, G2u, Gu),
            update_attribute(Ga, (Gs,Gu))
        ; attach_attribute(Ga, (G1s,G1u)),
          get_attribute(Ref, p(T,De,Det,In,Int)),
          EqsType is T \/ 2'01,
          update_attribute(Ref, p(EqsType,De,Det,In,Int))
        ).

%%% Print hooks

:- import(write, [print/1]).

:- multifile portray_attribute/2.

portray_attribute(float(F),_) :- print(F).
portray_attribute(term_wrap(_,T), _) :-
        normalize(T, I, H),
        H = [],                   % only if ground
        print(I).

:- multifile portray/1.

portray(rat(N,D)) :-
        print(N/D).
portray(eqn_var(Self,A,B,R,Nl)) :-
        print(eqn_var(Self,A,B,R,Nl)).
