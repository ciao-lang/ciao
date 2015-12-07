%% wumpus.pl -- The WUMPUS world
 %% AFSID           : $__Header$
 %% Author          : Daniel Cabeza, Manuel Carro 
 %% Created On      : Fri Mar  7 13:53:44 1997
 %% Last Modified By: 
 %% Last Modified On: Sun Aug  7 18:53:11 2005
 %% Update Count    : 191
 %% Status          : Completed


 %% elementos en el mundo: 0'a(gujero), 0'w(umpus), 0'o(ro), 0'n(ada)

 %% percepciones: hedor, brisa, brillo, golpe

 %% mundo: m(Robot, SituacionOro, ObjetosMundo)

:- module(wumpus, [w_n/1, w/0,w/1]).
:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(library(between)).

:- set_prolog_flag(multi_arity_warnings, off).

w_n(N) :-
	between(1, N, _),
	w,
	fail
 ;
	true.

:- include(robot).

percepciones(Mov, Mundo, NuevoMundo, Percepciones, Dec):-
        Mundo = m(robot(Xr, Yr), Oro, Objetos),
        aplica_mov(Mov, Xr, Yr, Objetos, NXr, NYr), !,
        calcula_percepciones(Objetos, NXr, NYr, Oro, NOro, Percepciones, Dec),
        NuevoMundo = m(robot(NXr, NYr), NOro, Objetos).
percepciones(_Mov, Mundo, Mundo, [golpe], 2).

aplica_mov(Mov, Xr, Yr, Objetos, NXr, NYr):-
        delta_coord(Mov, Dx, Dy),
        NXr is Xr + Dx,
        NYr is Yr + Dy,
        dentro_mundo(Objetos, NXr, NYr).

calcula_percepciones(Objetos, X, Y, SitOro, NSitOro, P, Dec) :-
        objeto_en(Objetos, X, Y, Ob),
        comprueba_oro(Ob, SitOro, NSitOro, P, P_),
        calcula(Ob, Dec),
        findall(Percepcion, percepcion_cercana(Objetos, X, Y, Percepcion), P1),
        sort(P1, P_).                    %%  Remove duplicates

percepcion_cercana(Objetos, X, Y, Percepcion) :-
        delta_coord(_, Dx, Dy),
        NX is X+Dx,
        NY is Y+Dy,
        dentro_mundo(Objetos, NX, NY),
        objeto_en(Objetos, NX, NY, Ob),
        produce(Ob, Percepcion).

comprueba_oro(0'o, suelo, robot, [brillo|P], P):- !.
comprueba_oro(_Ob, SitOro, SitOro, P, P).

calcula(0'o, 1).
calcula(0'n, 1).
calcula(0'w, 1000).
calcula(0'a, 1000).

produce(0'w, hedor).
produce(0'a, brisa).

delta_coord(n,  0,  1).
delta_coord(s,  0, -1).
delta_coord(e,  1,  0).
delta_coord(o, -1,  0).

dentro_mundo(mat(DimX, DimY, _), X, Y) :-
        X > 0,
        X =< DimX,
        Y > 0,
        Y =< DimY.

objeto_en(mat(_, _, Objetos), X, Y, Ob) :-
        arg(Y, Objetos, Columna),
        arg(X, Columna, Ob).

w:- w(world).

w(File):-
        mundo(File, Mundo),
        despertar(Cons),
        Mundo = m(Inicio, Oro, Objetos),
        Inicio = robot(Xr, Yr),
        calcula_percepciones(Objetos, Xr, Yr, Oro, _NOro, Percepciones, _Dec),
        acciones(Mundo, 1000, Cons, Percepciones, Inicio).

acciones(Mundo, Score, Cons, Percepciones, Inicio):-
        Score > 0,
        robot(Cons, Percepciones, Mov, NewCons), !,
        continua(Mov, Mundo, Score, NewCons, Inicio).
acciones(_Mundo, _, _, _, _):-
        write_out(['AAAAAAAaaaaaaaaaaahhhhhhhhh!!!!!!!!']).

continua(q, Mundo, Score, _, Inicio) :- !,
        comprobar_salida(Mundo, Inicio, Score).
continua(Mov, Mundo, Score, Cons, Inicio) :-
        percepciones(Mov, Mundo, NuevoMundo, Percepciones, Dec),
        NuevoScore is Score - Dec,
        acciones(NuevoMundo, NuevoScore, Cons, Percepciones, Inicio).

comprobar_salida(Mundo, Inicio, _Score) :-
        Mundo = m(Inicio, robot, _Objetos), !.
        %write_out(['Bien hecho! Puntuacion = ', Score]).
comprobar_salida(_, _, _).
        %write_out(['Salida ilegal!']).

mundo(File, Mundo) :-
        open(File, read, St),
        lee_fila(St, List, Robot),
        Fila =.. [h|List],
        lee_otras_filas(St, [Fila], Filas, Robot),
        close(St),
        M =.. [h|Filas],
        functor(M, _, Rows),
        functor(Fila, _, Cols),
        Mat = mat(Cols, Rows, M),
        posicion(Robot, Mat, RobotPos),
        Mundo = m(RobotPos, suelo, Mat).

lee_fila(St, List, Robot) :-
        get_code(St, C),
        lee_fila_(C, St, List, Robot).

lee_fila_(0'\n, _, [], _) :- !. % newline
lee_fila_(0'r, St, [R|L], R) :- !,
        get_code(St, C),
        lee_fila_(C, St, L, _).
lee_fila_(O, St, [O|L], R) :-
        get_code(St, C),
        lee_fila_(C, St, L, R).

lee_otras_filas(St, Filas0, Filas, R) :-
        get_code(St, C),
        lee_otras_filas_(C, St, Filas0, Filas, R).

lee_otras_filas_(-1, _, Filas, Filas, _) :- !. % EOF 
lee_otras_filas_(0'\n, _, Filas, Filas, _) :- !. % Linea en blanco
lee_otras_filas_(C, St, Filas0, Filas, Robot) :-
        lee_fila_(C, St, List, Robot),
        Fila =.. [h|List],
        get_code(St, D),
        lee_otras_filas_(D, St, [Fila|Filas0], Filas, Robot).

posicion(Robot, mat(Cols, Rows, M), robot(X,Y)) :-
        between(1, Rows, Y),
        between(1, Cols, X),
        arg(Y, M, F),
        arg(X, F, O),
        O == Robot, !,
        Robot = 0'n.        

 %% between(Min, Max, X) :-
 %%         Min =< Max,
 %%         between_(Min, Max, X).
 %% between_(Min, _Max, Min).
 %% between_(Min, Max, X) :-
 %%         Min1 is Min+1,
 %%         between(Min1, Max, X).

write_out([]).
write_out([X|Xs]) :- write(X), write_out(Xs).