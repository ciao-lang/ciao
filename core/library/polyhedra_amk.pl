% (c) Florence Benoy, Andy King and Fred Mesnard
%
% See the TPLP paper "Calculating Convex Hulls with
% a Linear Solver" for explanation and discussion
%



%% project(+Xs,+Cxs,-ProjectCxs): Xs is a list of variables, Cxs is a list of contraints,
%% Projectxs is the projection of the conjunction of the constraints of Cxs on the
%% variables Cs
%%

project(Xs, Cxs, ProjectCxs) :-
        copy_term(Xs-Cxs, CpyXs-CpyCxs),
        clpqr_meta(CpyCxs),
        prepare_dump(CpyXs, Xs, Zs, DumpCxs, ProjectCxs),
        dump_constraints(Zs,Vs, DumpCxs), Xs = Vs.


prepare_dump([], [], [], Cs, Cs).
prepare_dump([X|Xs], YsIn, ZsOut, CsIn, CsOut) :-
        (ground(X) ->
            YsIn  = [Y|Ys],
            ZsOut = [_|Zs],
            CsOut = [Y .=. X|Cs]
        ;
            YsIn  = [_|Ys],
            ZsOut = [X|Zs],
            CsOut = Cs
        ),
        prepare_dump(Xs, Ys, Zs, CsIn, Cs).



%% convex_hull(+Xs, +Cxs, +Ys, +Cys, +Zs, +Czs) : Xs and Ys are list of variables of
%% the same length, Cxs and Cys are lists of constraints of the variables of Xs and
%% Ys repressively, Zs is a list of new fresh variables of the same length as Xs and
%% Ys, and Czs is a list of constrains over Zs which represents the convex-hull of
%% Cxs and Cys.
%%

convex_hull(Xs, Cxs, Ys, Cys, Zs, Czs) :-
        scale(Cxs, Sig1, [], C1s),
        scale(Cys, Sig2, C1s, C2s),
        add_vect(Xs, Ys, Zs, C2s, C3s),
        project(Zs, [Sig1 .>=. 0, Sig2 .>=. 0, Sig1+Sig2 .=. 1|C3s], Czs).

scale([], _, Cs, Cs).
scale([C1|C1s], Sig, C2s, C3s) :-
        C1 =.. [RelOp, A1, B1],
        C2 =.. [RelOp, A2, B2],
        mul_exp(A1, Sig, A2),
        mul_exp(B1, Sig, B2),
        scale(C1s, Sig, [C2|C2s], C3s).

mul_exp(E1, Sigma, E2) :- once(mulexp(E1, Sigma, E2)).

mulexp(  X,   _,     X) :- var(X).
mulexp(N*X,   _,   N*X) :- ground(N), var(X).
mulexp( -X, Sig,    -Y) :- mulexp(X, Sig, Y).
mulexp(A+B, Sig,   C+D) :- mulexp(A, Sig, C), mulexp(B, Sig, D).
mulexp(A-B, Sig,   C-D) :- mulexp(A, Sig, C), mulexp(B, Sig, D).
mulexp(  N, Sig, N*Sig) :- ground(N).

add_vect([], [], [], Cs, Cs).
add_vect([U|Us], [V|Vs], [W|Ws], C1s, C2s) :-
        add_vect(Us, Vs, Ws, [W .=. U+V|C1s], C2s).




%% polyhedra_widen(+Vars,+Clist1,+Clist2,-WidenVars,-ClistWiden) : Clist1,
%% Clist2 are 2 lists of cosntraints on Vars. WidenVars are fresh
%% variables and ClistWiden is the result of selecting the constraints
%% from Clist1 which are entailed by Clist2.
%%
%% This predicates was taken from the termination analyser of Cohavit Taboch and Mike Codish
%%

polyhedra_widen(_,_,[],_,[]) :- !.
polyhedra_widen(_,[],_,_,[]) :- !.
polyhedra_widen(Vars,Cons1,Cons2,WidenVars,ClistWiden) :-
        copy_term((Vars,Cons1,Cons2),(Vars1,Clist1,Clist2)),
        copy_term((Vars1,Clist1),(WidenVars,ClistCopy)),
        clpqr_meta(Clist2),
        entailed_constraints(Clist1,ClistCopy,ClistWiden).

entailed_constraints([],[],[]).
entailed_constraints([C|Cs],[CC|CCs],[CC|En]) :-
        clpqr_entailed([C]),!,
        entailed_constraints(Cs,CCs,En).
entailed_constraints([_|Cs],[_|CCs],En) :-
        entailed_constraints(Cs,CCs,En).

