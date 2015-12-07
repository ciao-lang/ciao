/*
PROLOG Digest           Thursday, 28 Aug 1986      Volume 4 : Issue 44

Today's Topics:
             Performance - Benchmarking Systems & Part II
----------------------------------------------------------------------

Date: Tue, 22 Jul 86 07:46:55 -0100
>From: Jean Claude Syre <unido!ecrcvax!jclaude@seismo.css.gov>
Subject: Benchmarking Prolog systems (part 2 of 3)


         ***********************************************
         *** BENCHMARK PROGRAMS FOR PROLOG SYSTEMS   ***
         ***   (FINAL VERSION)                       ***
         ***********************************************
                       Part 2 (of 3)


1.3. Program to test the handling of environments  (envir).

The creation and deletion of environments are  an important
feature in prolog machines. The following program attempts to
evaluate that.
A usual condition to have environments is that the clause is
made of several goals. Thus there will be calls in the clause
creating environments, and some work to set the parameters of
each call. Three arguments per goal were chosen because this number
comes close to the average number of arguments of a predicate and to
the average number of permanent variables in an environment.
The arguments were arranged in different orders for every goal,
because we did not want to measure the merits of register transfer
optimisations.  Note that these transfers exist, so the results
cannot be compared with those given by the program which
creates choice points (generally slower).

Another version, envir0ar(N), with 0 argument in each call, can
also be usefully tried

--------cut here - beginning of program listing--------------------
*/

:- module(envir,[envir/1, envir0ar/1], []).

:- use_module(benchmark_utilities).

/* envir(N): 3 arguments environment creation */
/*         creates 79 environments and 158 calls     */
/* suggested value for N: 1000 (interp), 1000 (comp) */
/* results for Cprolog: N=1000           */
/* Tloop=38.6 Tcomp=0.97 Tnet=37.6 Klips=4.23 */

envir(N):- common_loop(3, N, env0(_,_,_), 159, envir).

env0(X,Y,Z):-env1(Z,X,Y),env2(Y,Z,X). /* creates 79 environments */
env1(X,Y,Z):-env3(Z,Y,X),env4(Y,Z,X).
env2(X,Y,Z):-env3(Z,Y,X),env4(Y,Z,X).  /* and 158 calls */
env3(X,Y,Z):-env5(Z,Y,X),env6(Y,Z,X).
env4(X,Y,Z):-env5(Z,Y,X),env6(Y,Z,X).
env5(X,Y,Z):-env7(Z,Y,X),env8(Y,Z,X).
env6(X,Y,Z):-env7(Z,Y,X),env8(Y,Z,X).
env7(X,Y,Z):-env9(Z,Y,X),env10(Y,Z,X).
env8(X,Y,Z):-env9(Z,Y,X),env10(Y,Z,X).
env9(X,Y,Z):-env11(Z,Y,X),env12(Y,Z,X).
env10(X,Y,Z):-env12(Z,Y,X),env12(Y,Z,X).
env11(X,Y,Z):-env12(Z,Y,X),env12(Y,Z,X).
env12(_X,_Y,_Z).


/* envir0ar(N): zero argument environment creation */
/*       creates 79 environments and 158 calls     */
/* suggested value for N: 1000 (interp), 1000 (comp) */
/* results for Cprolog: N=1000           */
/* Tloop=18.88 Tcomp=1.01 Tnet=17.87 Klips=8.9 */

envir0ar(N):- common_loop(3, N, env0_0, 159, envir0ar).


env0_0:-env1_0,env2_0. /* creates 79 environments */
env1_0:-env3_0,env4_0.
env2_0:-env3_0,env4_0.  /* and 158 calls */
env3_0:-env5_0,env6_0.
env4_0:-env5_0,env6_0.
env5_0:-env7_0,env8_0.
env6_0:-env7_0,env8_0.
env7_0:-env9_0,env10_0.
env8_0:-env9_0,env10_0.
env9_0:-env11_0,env12_0.
env10_0:-env12_0,env12_0.
env11_0:-env12_0,env12_0.
env12_0.

/* ---------------cut here - end of program listing---------------- */
