%% Movims, Pos_Ant, Mov_Ant, Limites, Visitadas, Objetos
despertar(
    c([],   p(0,-1), n, lim(_,_,_,_), _Visitadas, o([],[],[],[]))
         ).

robot(Cons, Perceps, Mov, NewCons) :-
        arg(1, Cons, Movs),
        actua(Movs, Cons, Perceps, Mov, NewCons).

% Percibiendo y deduciendo 
actua([], Cons, Perceps, Mov, NewCons) :-
        Cons = c(_, Sit0, Mov0, Lims, Vis, Obj0),
        NewCons = c(Movs, Sit, Mov, Lims, Vis, Obj),
        deduce(Perceps, Sit0, Mov0, Sit, Lims, Vis, Obj0, Obj,
               Tengo_oro),
        nuevos_movs(Tengo_oro, Sit, Vis, Obj, [Mov|Movs]).
% Siguiendo un camino precomputado
actua([M|Ms], Cons, _Per, M, NewCons) :-
        Cons = c(_, Sit0, M0, Lims, Vis, Obj),
        NewCons = c(Ms, Sit, M, Lims, Vis, Obj),
        cambia_coord(M0, Sit0, Sit).

% Deducir informacion a partir de las percepciones
deduce([golpe], Sit0, Mov0, Sit0, Lims, _Vis, Obj0, Obj,
       no_tengo_oro) :-
        !,
        cambia_limites(Mov0, Sit0, Lims, Val),
        depura_limite(Mov0, Val, Obj0, Obj).
deduce(Perceps, Sit0, Mov0, Sit, Lims, Vis, Obj0, Obj,
       Tengo_oro) :-
        cambia_coord(Mov0, Sit0, Sit),
        member(Sit, Vis),
        no_hay_nada(Sit, Obj0, Obj1),
        tengo_oro(Perceps, NewPerceps, Tengo_oro),
        renueva_conocimiento(NewPerceps, Sit, Lims, Obj1, Obj).

tengo_oro(Perceps, NewPerceps, tengo_oro):-
        select(brillo, Perceps, NewPerceps), !.
tengo_oro(Perceps, Perceps, no_tengo_oro).

% Objetos: Wn - lugares sin wumpus
%          Wp - lugares con wumpus (disyunciones)
%          An - lugares sin agujeros
%          Ap - lugares con agujeros (disyunciones)

no_hay_nada(P, o(Wn0, Wp0, An0, Ap0), o(Wn, Wp, An, Ap)):-
        posicion_neg(P, Wn0, Wp0, Wn, Wp), % no hay wumpus
        posicion_neg(P, An0, Ap0, An, Ap). % no hay agujeros

posicion_neg(P, In0, Ip0, In, Ip) :-
        annade(P, In0, In),
        elimina_pos(Ip0, P, Ip).

elimina_pos([], _P, []).
elimina_pos([Disj|Disjs], P, [NDisj|NDisjs]):-
        elimina(P, Disj, NDisj),
        disj_no_vacia(NDisj),
        elimina_pos(Disjs, P, NDisjs).

disj_no_vacia([_|_]) :- !.
disj_no_vacia([]) :-
        write('Disyuncion vacia! Error interno!'), nl, abort.


renueva_conocimiento(Perceps, Sit, Lims, Obj0, Obj) :-
        Obj0 = o(Wn0, Wp0, An0, Ap0),
        Obj  = o(Wn, Wp, An, Ap),
        % wumpus alrededor?
        comprueba_percepcion(hedor, Perceps, Sit, Lims,
                             Wn0, Wp0, Wn, Wp),
        % agujero alrededor?
        comprueba_percepcion(brisa, Perceps, Sit, Lims,
                             An0, Ap0, An, Ap).

comprueba_percepcion(P, Ps, Sit, Lims, In, Ip, In, [Disj|Ip]) :-
        member(P, Ps), !,
        objeto_alrededor(Sit, Lims, In, Disj),
        disj_no_vacia(Disj).
comprueba_percepcion(_, _, Sit, Lims, In0, Ip0, In, Ip) :-
        vacia_alrededor([n,s,e,o], Sit, Lims, In0, Ip0, In, Ip).

objeto_alrededor(Sit, Lims, Ineg, Disj) :-
        findall(SitAlr, (
                            member(M, [n,s,e,o]),
                            cambia_coord(M, Sit, SitAlr),
                            dentro_limites(SitAlr, Lims),
                            \+ member(SitAlr, Ineg)
                        ),
                        Disj).

vacia_alrededor([], _Sit, _Lims, In, Ip, In, Ip).
vacia_alrededor([M|Ms], Sit, Lims, In0, Ip0, In, Ip):-
        cambia_coord(M, Sit, SitAlr),
        dentro_limites(SitAlr, Lims), !,
        posicion_neg(SitAlr, In0, Ip0, In1, Ip1),
        vacia_alrededor(Ms, Sit, Lims, In1, Ip1, In, Ip).
vacia_alrededor([_|Ms], Sit, Lims, In0, Ip0, In, Ip):-
        vacia_alrededor(Ms, Sit, Lims, In0, Ip0, In, Ip).


nuevos_movs(tengo_oro, Sit, Vis, _Obj, Movs):-
        dame_camino(Sit, p(0,0), Vis, Movs0),
        append(Movs0, [q], Movs).
nuevos_movs(no_tengo_oro, Sit, Vis, Obj, Movs):-
        findall(D-P, (
                         posicion_vacia(P, Obj),
                         \+ member_ol(P, Vis),
                         d_manhattan(P, Sit, D)
                     ),
                     Objetivos),
        nueva_posicion(Objetivos, Sit, Vis, Obj, Movs).

nueva_posicion([], Sit, Vis, Obj, Movs):-  !,
        findall(D-P, (
                         posicion_insegura(P, Obj),
                         d_manhattan(P, Sit, D)
                     ),
                     Objetivos),
        arriesga(Objetivos, Sit, Vis, Movs).
nueva_posicion(Objetivos, Sit, Vis, _Obj, Movs):- 
        keysort(Objetivos, [_-P|_]),
        dame_camino(Sit, P, Vis, Movs).

arriesga([], _Sit, _Vis, _Movs):-
        write('No hay solucion'), nl, abort.
arriesga(Objetivos, Sit, Vis, Movs):-
        keysort(Objetivos, [_-P|_]),
        dame_camino(Sit, P, Vis, Movs).


posicion_vacia(P, o(Wn, _Wp, An, _Ap)) :-
        member(P, Wn),
        member(P, An).

posicion_insegura(P, Obj) :-
        Disj = [_,_|_],
        conjunto_inseguro(Disj, Obj),
        member(P, Disj),
        \+ conjunto_inseguro([P], Obj).

conjunto_inseguro(Disj, o(_Wn, Wp, _An, _Ap)) :-
        member(Disj, Wp).
conjunto_inseguro(Disj, o(_Wn, _Wp, _An, Ap)) :-
        member(Disj, Ap).

dame_camino(Po, Pf, Pos, C) :-
        dame_un_camino(Po, Pf, Pos, CC), !,
        acorta_camino(CC, C).

dame_un_camino(Pf, Pf, _Pos, []) :- !. % no necesario Pf en Pos
dame_un_camino(Po, Pf, Pos, [M|Ms]) :-
        select_ol(Po, Pos, NPos), !,
        sel_mov(Po, Pf, M),
        cambia_coord(M, Po, P1),
        dame_un_camino(P1, Pf, NPos, Ms).

sel_mov(p(Xo, Yo), p(Xf, Yf), M) :-
        Dx is Xf-Xo,
        Dy is Yf-Yo,
        pref_movs(Dx, Dy, Mlist),
        member(M, Mlist).

pref_movs(Dx, Dy, [n,e,o,s]) :- Dx >= 0, Dy > Dx, !.
pref_movs(Dx, Dy, [n,o,e,s]) :- Dx < 0,  Dy >= -Dx, !.
pref_movs(Dx, Dy, [o,n,s,e]) :- Dy >= 0, Dy < -Dx, !.
pref_movs(Dx, Dy, [o,s,n,e]) :- Dy < 0,  Dy >= Dx, !.
pref_movs(Dx, Dy, [s,o,e,n]) :- Dx =< 0, Dx > Dy, !.
pref_movs(Dx, Dy, [s,e,o,n]) :- Dx > 0,  Dx < -Dy, !.
pref_movs(Dx, Dy, [e,s,n,o]) :- Dy =< 0, Dx >= -Dy, !.
pref_movs(Dx, Dy, [e,n,s,o]) :- Dy > 0,  Dx >= Dy, !.

acorta_camino([], []) :- !.
acorta_camino(CC, [M|C]) :-
        acorta_principio(CC, 0, 0, 0, [M|CC_]), !,
        acorta_camino(CC_, C).
acorta_camino([M|CC], [M|C]) :-
        acorta_camino(CC, C).

acorta_principio([M|Ms], Count, X0, Y0, C) :-
        delta_coord(M, Dx, Dy),
        X is X0+Dx,
        Y is Y0+Dy,
        acorta_principio(Ms, s(Count), X, Y, C).
acorta_principio(Ms, s(s(s(_))), Dx, Dy, [M|Ms]) :-
        delta_coord(M, Dx, Dy).
        
cambia_limites(n, p(_,Ymax), lim(_, _, _, Ymax), Ymax).
cambia_limites(s, p(_,Ymin), lim(_, Ymin, _, _), Ymin).
cambia_limites(e, p(Xmax,_), lim(_, _, Xmax, _), Xmax).
cambia_limites(o, p(Xmin,_), lim(Xmin, _, _, _), Xmin).

depura_limite(Mov, Val, o(Wn0, Wp0, An0, Ap0),
                        o(Wn, Wp, An, Ap)):-
        depura_limite_l(Wn0, Mov, Val, Wn),
        depura_limite_l(An0, Mov, Val, An),
        depura_limite_l_l(Wp0, Mov, Val, Wp),
        depura_limite_l_l(Ap0, Mov, Val, Ap).

depura_limite_l_l([], _, _, []).
depura_limite_l_l([L|Ls], Mov, Val, [N|Ns]) :-
        depura_limite_l(L, Mov, Val, N),
        disj_no_vacia(N),
        depura_limite_l_l(Ls, Mov, Val, Ns).

depura_limite_l([], _, _, []).
depura_limite_l([P|Ps], Mov, Val, NPs) :-
        fuera_limite(Mov, Val, P), !,
        depura_limite_l(Ps, Mov, Val, NPs).
depura_limite_l([P|Ps], Mov, Val, [P|NPs]) :-
        depura_limite_l(Ps, Mov, Val, NPs).

fuera_limite(n, Ymax, p(_,Y)) :- Y > Ymax.
fuera_limite(s, Ymin, p(_,Y)) :- Y < Ymin.
fuera_limite(e, Xmax, p(X,_)) :- X > Xmax.
fuera_limite(o, Xmin, p(X,_)) :- X < Xmin.


dentro_limites(p(X,Y), lim(Xmin, Ymin, Xmax, Ymax)) :-
        lim_inferior(Xmin, X),
        lim_inferior(Ymin, Y),
        lim_superior(Xmax, X),
        lim_superior(Ymax, Y).

lim_inferior(Min, _) :- var(Min), !.
lim_inferior(Min, X) :- Min =< X.

lim_superior(Max, _) :- var(Max), !.
lim_superior(Max, X) :- Max >= X.


cambia_coord(q, P, P):- !.
cambia_coord(M, p(X, Y), p(Nx, Ny)):-
        delta_coord(M, Dx, Dy),
        Nx is X + Dx,
        Ny is Y + Dy.


member_ol(E, L):- 
        nonvar(L),
        L = [X|Xs],
        member_ol(E, X, Xs).

member_ol(E, E, _).
member_ol(E, _, L):- member_ol(E, L).


select_ol(X, L, R) :-
        nonvar(L),
        L = [Y|Ys],
        select_ol(X, Y, Ys, R).

select_ol(X, X, R, R).
select_ol(X, Y, L, [Y|R]) :-
        select_ol(X, L, R).

d_manhattan(p(X1, Y1), p(X2, Y2), D):-
        d_pt(X1, X2, Dx),
        d_pt(Y1, Y2, Dy),
        D is Dx + Dy.

d_pt(X1, X2, D) :- X1 >= X2, !, D is X1-X2.
d_pt(X1, X2, D) :- D is X2-X1.

annade(E, L, L) :- member(E, L), !.
annade(E, L, [E|L]).

elimina(E, L, R) :- select(E, L, R), !.
elimina(_, L, L).
