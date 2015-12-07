list_to_conjunct([], true).
list_to_conjunct([G|Gs], Goal) :-
        list_to_conjunct_(Gs, G, Goal).

list_to_conjunct_(Gs, G, Goal) :- var(Gs), !,
        Goal = (G,Gs).
list_to_conjunct_([], G, Goal) :- !,
        Goal = G.
list_to_conjunct_([G|Gs], G1, Goal) :- !,
        Goal = (G1,Gs1),
        list_to_conjunct_(Gs, G, Gs1).

translate_clpqr(A.=.B,  Code) :- !,
        compile_constr(.=.,  A-B, L, []),
        list_to_conjunct(L, Code).
translate_clpqr(A.<>.B, Code) :- !,
        compile_constr(.<>., A-B, L, []),
        list_to_conjunct(L, Code).
translate_clpqr(A.<.B,  Code) :- !,
        compile_constr(.<.,  A-B, L, []),
        list_to_conjunct(L, Code).
translate_clpqr(A.=<.B, Code) :- !,
        compile_constr(.=<., A-B, L, []),
        list_to_conjunct(L, Code).
translate_clpqr(A.>.B,  Code) :- !,
        compile_constr(.<.,  B-A, L, []),
        list_to_conjunct(L, Code).
translate_clpqr(A.>=.B, Code) :- !,
        compile_constr(.=<., B-A, L, []),
        list_to_conjunct(L, Code).

% translate_hash(#(p), 3.141592653589793).
% translate_hash(#(e), 2.718281828459045).
% translate_hash(#(zero), Eps) :- arith_eps(Eps).
